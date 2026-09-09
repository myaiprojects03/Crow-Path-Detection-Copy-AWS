# =============================================================================
# batch_output.R — Excel row construction and accumulation
#
# Writes one row per detected stream per scan into a single .xlsx workbook.
# If no streams are detected for a scan, writes one row with path columns
# set to NA (preserves the full temporal record for weather merging).
# =============================================================================

# ---------------------------------------------------------------------------
# format_batch_rows()
#
# Converts one scan's results into a data.frame ready for the Excel file.
# Each detected stream → one row.  Zero detections → one NA row.
#
# Arguments:
#   scan_result  — list returned by process_scan_file()
#   streams      — data.frame returned by select_streams() (may have 0 rows)
#   window       — list(start, end, event_time) from get_scan_window()
#   event_label  — "sunset" or "sunrise"
# ---------------------------------------------------------------------------
format_batch_rows <- function(scan_result, streams, window, event_label) {

  scan_time <- scan_result$scan_time

  # VCP from pvol header (populated by process_scan_file in io.R)
  vcp <- if (!is.null(scan_result$vcp) && nzchar(scan_result$vcp)) scan_result$vcp else "unknown"

  # Weather flag
  weather_cov_raw <- if (!is.null(scan_result$annulus_weather_coverage)) scan_result$annulus_weather_coverage else NA_real_
  is_weather_skip <- !is.na(weather_cov_raw) &&
    exists("MAX_ANNULUS_WEATHER_COVERAGE") &&
    weather_cov_raw > MAX_ANNULUS_WEATHER_COVERAGE
  weather_flag <- if (is_weather_skip) {
    paste0("weather_skip (", round(weather_cov_raw * 100, 1), "% coverage)")
  } else {
    "clear"
  }

  # Build one row per stream (or one blank row if none detected)
  if (nrow(streams) == 0) {
    df <- data.frame(
      roost          = ROOST_NAME,
      radar          = RADAR_ID,
      scan_time_utc  = format(scan_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      vcp            = vcp,
      weather_flag   = weather_flag,
      event          = event_label,
      event_time_utc = format(window$event_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      compass        = NA_character_,
      bearing_deg    = NA_real_,
      begin_lat      = NA_real_,
      begin_lon      = NA_real_,
      end_lat        = NA_real_,
      end_lon        = NA_real_,
      stringsAsFactors = FALSE
    )

    df[["annulus_weather_coverage"]]     <- round(weather_cov_raw, 4)
    df[["contiguous_extent_km"]]         <- NA_real_
    df[["angular_width_degree"]]         <- NA_real_
    df[["volume_eta_sum"]]               <- NA_real_
    df[["form_contrast_ratio"]]          <- NA_real_
    return(df)
  }

  rows <- lapply(seq_len(nrow(streams)), function(i) {
    s <- streams[i, ]

    # Begin point: PLOT_MIN_DIST_KM from roost along the bearing
    begin_pt <- destination_point(ROOST_LAT, ROOST_LON, s$bearing_deg, PLOT_MIN_DIST_KM)
    # End   point: max_extent_km from roost along the bearing
    end_pt   <- destination_point(ROOST_LAT, ROOST_LON, s$bearing_deg,
                                  max(s$max_extent_km, PLOT_MIN_DIST_KM))

    # Thickness calculation: number of supporting bearings * BEARING_STEP_DEG
    bearing_count <- if (!is.null(s$supporting_bearings) && nchar(s$supporting_bearings) > 0) {
      length(strsplit(s$supporting_bearings, ";", fixed = TRUE)[[1]])
    } else {
      1L
    }
    angular_width <- bearing_count * BEARING_STEP_DEG

    df <- data.frame(
      roost          = ROOST_NAME,
      radar          = RADAR_ID,
      scan_time_utc  = format(scan_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      vcp            = vcp,
      weather_flag   = weather_flag,
      event          = event_label,
      event_time_utc = format(window$event_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      compass        = s$compass,
      bearing_deg    = round(s$bearing_deg),
      begin_lat      = round(begin_pt[2], 6),
      begin_lon      = round(begin_pt[1], 6),
      end_lat        = round(end_pt[2],   6),
      end_lon        = round(end_pt[1],   6),
      stringsAsFactors = FALSE
    )
    df[["annulus_weather_coverage"]]     <- round(weather_cov_raw, 4)
    df[["contiguous_extent_km"]]         <- round(s$contiguous_extent_km, 3)
    df[["angular_width_degree"]]         <- round(angular_width, 1)
    df[["volume_eta_sum"]]               <- round(s$corridor_eta_sum, 2)
    df[["form_contrast_ratio"]]          <- round(s$contrast_ratio, 2)
    df
  })

  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# write_batch_rows()
#
# Appends rows_df to the Excel workbook at BATCH_OUTPUT_FILE.
# Creates the file with a styled header on the first write.
# Appends without re-writing the header on subsequent writes.
#
# Writing after every scan (not buffering) means a crash mid-batch still
# preserves all previously completed results.
# ---------------------------------------------------------------------------
write_batch_rows <- function(rows_df, output_file, overwrite = FALSE) {

  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  file_exists <- file.exists(output_file) && file.info(output_file)$size > 0

  if (!file_exists || overwrite) {
    # ---- First write: create workbook with styled header ----
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Detections")

    # Header style
    header_style <- openxlsx::createStyle(
      fontColour = "#FFFFFF",
      fgFill     = "#2C3E50",
      halign     = "CENTER",
      textDecoration = "bold",
      border     = "Bottom",
      borderColour   = "#AAAAAA"
    )

    openxlsx::writeData(wb, "Detections", rows_df,
                        startRow = 1, headerStyle = header_style)
    openxlsx::freezePane(wb, "Detections", firstRow = TRUE)

    # Auto column widths
    openxlsx::setColWidths(wb, "Detections", cols = seq_len(ncol(rows_df)),
                           widths = "auto")

    tryCatch({
      openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
      message("  Created output file: ", output_file)
    }, error = function(e) {
      if (grepl("Permission denied", e$message, ignore.case = TRUE)) {
        stop("FATAL ERROR: Cannot save to ", output_file, ". The file is OPEN in Excel. Please close it.")
      } else {
        stop(e)
      }
    })

  } else {
    # ---- Subsequent writes: append rows below existing data ----
    wb <- tryCatch({
      openxlsx::loadWorkbook(output_file)
    }, error = function(e) {
      warning("Failed to load workbook, recreating: ", e$message)
      openxlsx::createWorkbook()
    })

    if (!"Detections" %in% names(wb)) {
      openxlsx::addWorksheet(wb, "Detections")
    }

    existing <- tryCatch({
      openxlsx::read.xlsx(output_file, sheet = "Detections")
    }, error = function(e) {
      data.frame()
    })

    next_row <- if (is.null(existing) || nrow(existing) == 0) 2L else nrow(existing) + 2L

    openxlsx::writeData(wb, "Detections", rows_df,
                        startRow = next_row, colNames = FALSE)
    tryCatch({
      openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    }, error = function(e) {
      if (grepl("Permission denied", e$message, ignore.case = TRUE)) {
        stop("FATAL ERROR: Cannot append to ", output_file, ". The file is OPEN in Excel. Please close it.")
      } else {
        stop(e)
      }
    })
  }
}
