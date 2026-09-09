# =============================================================================
# batch_pipeline.R — Execution logic for multi-date batch runs
# =============================================================================

run_batch <- function() {

  # ------------------------------------------------------------------
  # 1. Expand all calendar dates for the configured season / year range
  # ------------------------------------------------------------------
  all_dates <- expand_batch_dates(
    BATCH_START_YEAR, BATCH_END_YEAR,
    BATCH_START_MMDD, BATCH_END_MMDD
  )

  total_dates <- length(all_dates)
  message("=============================================================")
  message(" Batch run: ", total_dates, " date(s) | ",
          BATCH_START_YEAR, "-", BATCH_START_MMDD, " to ",
          BATCH_END_YEAR,   "-", BATCH_END_MMDD)
  message(" Event: ", BATCH_EVENT,
          "  Offsets: ", BATCH_START_OFFSET_MIN, " to ",
          BATCH_END_OFFSET_MIN, " min")
  message(" Output: ", BATCH_OUTPUT_FILE)
  message("=============================================================")

  # ------------------------------------------------------------------
  # 2. Check if the output file is open/locked before running
  # ------------------------------------------------------------------
  if (file.exists(BATCH_OUTPUT_FILE)) {
    message("Checking if existing output file is accessible (not open in Excel)...")
    tryCatch({
      con <- file(BATCH_OUTPUT_FILE, open = "r+b")
      close(con)
    }, error = function(e) {
      stop("FATAL ERROR: The existing batch_results.xlsx file is locked or inaccessible!\n",
           "System Error: ", e$message, "\n",
           "Please ensure the file is NOT currently open in Microsoft Excel or another program, then try again.")
    })
  }

  # 0. Clear output directory at start (preserving batch_results.xlsx)
  if (exists("CLEAN_OUTPUT_ON_START") && isTRUE(CLEAN_OUTPUT_ON_START)) {
    clear_output_directory()
  }

  n_scans_total    <- 0L
  n_dates_ok       <- 0L
  n_dates_skipped  <- 0L
  is_first_write   <- FALSE # Preserve existing batch_results.xlsx data across runs


  # ------------------------------------------------------------------
  # 3. Main loop: for each calendar date
  # ------------------------------------------------------------------
  for (di in seq_along(all_dates)) {

    current_date <- all_dates[[di]]
    timestamp    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message("\n[", timestamp, "] [", di, "/", total_dates, "] Date: ", current_date)
    # ------ 3a. Calculate scan window from sunrise/sunset ----------
    window <- get_scan_window(
      date             = current_date,
      lat              = ROOST_LAT,
      lon              = ROOST_LON,
      event            = BATCH_EVENT,
      start_offset_min = BATCH_START_OFFSET_MIN,
      end_offset_min   = BATCH_END_OFFSET_MIN
    )

    if (is.na(window$event_time)) {
      message("  Skipped: could not compute ", BATCH_EVENT,
              " for this date/location.")
      n_dates_skipped <- n_dates_skipped + 1L
      next
    }

    message("  ", BATCH_EVENT, " @ ",
            format(window$event_time, "%H:%M UTC"),
            "  |  scan window: ",
            format(window$start, "%H:%M"), " – ",
            format(window$end,   "%H:%M"), " UTC")

    # ------ 3b. Temporarily override MAX_FILES_PER_RUN if configured
    prev_max <- if (exists("MAX_FILES_PER_RUN")) MAX_FILES_PER_RUN else NA
    if (!is.na(BATCH_MAX_FILES_PER_DATE)) {
      assign("MAX_FILES_PER_RUN", as.integer(BATCH_MAX_FILES_PER_DATE),
             envir = .GlobalEnv)
    }

    # ------ 3c. Run the detection pipeline for this date's window --
    scan_results <- tryCatch({
      run_pipeline(date_start = window$start, date_end = window$end)
    }, error = function(e) {
      message("  SKIPPED: ", conditionMessage(e))
      NULL
    })

    # Restore MAX_FILES_PER_RUN
    assign("MAX_FILES_PER_RUN", prev_max, envir = .GlobalEnv)

    if (is.null(scan_results) || length(scan_results) == 0) {
      n_dates_skipped <- n_dates_skipped + 1L
      next
    }

    # ------ 3d. Format and write rows to Excel for this date -------
    date_rows <- list()
    for (sr in scan_results) {
      rows <- tryCatch(
        format_batch_rows(sr$scan_res, sr$streams, window, BATCH_EVENT),
        error = function(e) {
          warning("  Could not format rows for scan ", sr$scan_res$scan_time,
                  ": ", e$message)
          NULL
        }
      )
      if (!is.null(rows)) {
        date_rows[[length(date_rows) + 1]] <- rows
        n_scans_total <- n_scans_total + 1L
      }
    }

    if (length(date_rows) > 0) {
      date_df <- do.call(rbind, date_rows)
      message("  Writing ", nrow(date_df), " stream row(s) to Excel...")
      write_batch_rows(date_df, BATCH_OUTPUT_FILE, overwrite = (is_first_write && BATCH_OVERWRITE_OUTPUT))
      is_first_write <- FALSE
    }

    n_dates_ok <- n_dates_ok + 1L

    # Purge downloaded pvol files for this date to keep server storage clean
    if (exists("CLEAN_PVOL_AFTER_RUN") && isTRUE(CLEAN_PVOL_AFTER_RUN)) {
      clear_pvol_directory()
    }
  }   # end date loop

  # Final storage cleanup check at end of batch
  if (exists("CLEAN_PVOL_AFTER_RUN") && isTRUE(CLEAN_PVOL_AFTER_RUN)) {
    clear_pvol_directory()
  }

  # ------------------------------------------------------------------
  # 4. Summary
  # ------------------------------------------------------------------
  message("\n=============================================================")
  message(" Batch complete.")
  message("  Dates processed : ", n_dates_ok)
  message("  Dates skipped   : ", n_dates_skipped)
  message("  Scans written   : ", n_scans_total)
  message("  Output file     : ",
          if (file.exists(BATCH_OUTPUT_FILE)) {
            normalizePath(BATCH_OUTPUT_FILE, winslash = "/")
          } else {
            "(no detections — file not created)"
          })
  message("=============================================================")
}

