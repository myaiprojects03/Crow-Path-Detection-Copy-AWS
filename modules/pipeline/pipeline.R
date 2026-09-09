# =============================================================================
# pipeline.R — High-level orchestration of the detection process
# =============================================================================

# date_start / date_end default to globals set in config.R so that
# calling run_pipeline() with no arguments (main.R) still works unchanged.
# batchmain.R passes explicit per-date windows as arguments.
run_pipeline <- function(date_start = DATE_START, date_end = DATE_END) {
  # Assign execution parameters to global environment for access by other modules
  assign("DATE_START", date_start, envir = .GlobalEnv)
  assign("DATE_END",   date_end,   envir = .GlobalEnv)
  
  message("Starting Crow Path Detection Pipeline...")

  # 0. Storage Cleanup at start (Single mode only; Batch mode handles cleanup in run_batch)
  if (exists("CLEAN_OUTPUT_ON_START") && isTRUE(CLEAN_OUTPUT_ON_START)) {
    if (!exists("RUN_MODE") || tolower(RUN_MODE) != "batch") {
      clear_output_directory()
    }
  }

  # 1. Setup output directory
  run_dir <- file.path(OUTPUT_DIR, run_stub_for(date_start, date_end))

  if (OVERWRITE_RUN_OUTPUT && dir.exists(run_dir)) {
    message("Cleaning existing output directory: ", run_dir)
    unlink(run_dir, recursive = TRUE)
  }
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  write_settings_file(run_dir)

  # 2. Discover / download files
  file_list <- download_radar(date_start, date_end, RADAR_ID, DATA_DIR)
  message("Processing ", length(file_list), " scans...")

  # 3. Process each scan
  scan_summaries <- list()

  if (exists("USE_PARALLEL") && USE_PARALLEL) {
    message("Initializing parallel cluster with ", NUM_CORES, " cores...")
    
    cl <- parallel::makeCluster(NUM_CORES)
    on.exit({
      message("Stopping parallel cluster...")
      parallel::stopCluster(cl)
    }, add = TRUE)
    
    # Initialize each worker
    parallel::clusterEvalQ(cl, {
      user_lib <- Sys.getenv("R_LIBS_USER")
      if (nchar(user_lib) == 0) {
        user_lib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library",
                              paste(R.version$major, substr(R.version$minor, 1, 1), sep = "."))
      }
      .libPaths(unique(c(user_lib, .libPaths())))
      
      library(bioRad)
      library(vol2birdR)
      library(ggplot2)
      library(geosphere)
      library(sp)
      library(ggspatial)
      library(prettymapr)
      library(rosm)
      library(suncalc)
      library(openxlsx)
      
      source("modules/pipeline/bootstrap.R")
      source_project_modules()
    })
    
    # Export current global environment variables to override defaults on workers
    global_vars <- ls(envir = .GlobalEnv)
    parallel::clusterExport(cl, varlist = global_vars, envir = .GlobalEnv)
    
    # Run process_single_scan in parallel
    message("Processing ", length(file_list), " scans in parallel...")
    results <- parallel::parLapply(cl, file_list, function(f) {
      process_single_scan(f, run_dir)
    })
    
    # Filter out NULLs
    scan_summaries <- Filter(Negate(is.null), results)
    
  } else {
    # Sequential processing
    for (i in seq_along(file_list)) {
      f <- file_list[i]
      message("[", i, "/", length(file_list), "] Processing: ", basename(f))
      res <- process_single_scan(f, run_dir)
      if (!is.null(res)) {
        scan_summaries[[length(scan_summaries) + 1]] <- res
      }
    }
  }

  # 4. Save run index
  if (length(scan_summaries) > 0) {
    index_rows <- do.call(rbind, lapply(scan_summaries, `[[`, "summary"))
    utils::write.csv(
      index_rows,
      file      = file.path(run_dir, "scan_index.csv"),
      row.names = FALSE
    )
    message("Pipeline complete. Results saved to: ", run_dir)
  } else {
    message("Pipeline finished with no scans processed.")
  }

  # 5. Purge downloaded pvol files if configured (Single mode handles here; Batch mode handles per-date in run_batch)
  if (exists("CLEAN_PVOL_AFTER_RUN") && isTRUE(CLEAN_PVOL_AFTER_RUN)) {
    if (!exists("RUN_MODE") || tolower(RUN_MODE) != "batch") {
      clear_pvol_directory()
    }
  }

  # Return scan results invisibly (used by batchmain.R)
  invisible(scan_summaries)
}


# Helper: generate a run folder name from explicit date args
run_stub_for <- function(date_start, date_end) {
  paste0(
    RADAR_ID, "_",
    format(date_start, "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "_to_",
    format(date_end,   "%Y%m%dT%H%M%SZ", tz = "UTC")
  )
}

# Process a single radar file and save results
process_single_scan <- function(f, run_dir) {
  message("Processing: ", basename(f))
  tryCatch({
    # Step A: Load and project
    scan_res <- process_scan_file(f)

    # Step B: Prepare pixels for detection (distance, bearing, annulus, etc)
    scan_res$detect_df <- prepare_detection_pixels(scan_res$detect_df)

    # Step B2: Check for widespread weather/precipitation coverage in annulus
    annulus_pixels <- scan_res$detect_df[scan_res$detect_df$in_annulus, , drop = FALSE]
    is_weather_skip <- FALSE
    coverage <- 0.0
    if (nrow(annulus_pixels) > 0) {
      non_na_pixels <- sum(!is.na(annulus_pixels$DBZH))
      if (non_na_pixels > 0) {
        active_pixels <- sum(!is.na(annulus_pixels$DBZH) & annulus_pixels$DBZH >= WEATHER_COVERAGE_DBZH_MIN)
        coverage <- active_pixels / non_na_pixels
        if (coverage > MAX_ANNULUS_WEATHER_COVERAGE) {
          message("  Widespread weather/precipitation detected in annulus (coverage: ", round(coverage * 100, 1), "% of non-NA pixels). Skipping stream detection.")
          is_weather_skip <- TRUE
        }
      }
    }
    scan_res$annulus_weather_coverage <- coverage

    if (is_weather_skip) {
      # Force 0 detections but run normal scoring and saving to preserve output files and maps
      scores <- score_bearings(scan_res$detect_df)
      scores$stream_detected <- FALSE
      
      streams <- data.frame(
        bearing_deg            = numeric(0),
        compass                = character(0),
        quality_score          = numeric(0),
        corridor_eta_sum       = numeric(0),
        corridor_eta_mean      = numeric(0),
        contrast_ratio         = numeric(0),
        local_prominence_ratio = numeric(0),
        corridor_valid_pixels  = integer(0),
        contiguous_extent_km   = numeric(0),
        max_extent_km          = numeric(0),
        supporting_bearings    = character(0)
      )
      
      summary_row <- save_scan_outputs(scan_res, scores, streams, run_dir,
                                        weather_skip = TRUE,
                                        weather_coverage = round(coverage * 100, 1))
      summary_row$detected_bearings <- "weather_skip"
      
      return(list(
        summary  = summary_row,
        scan_res = scan_res,
        streams  = streams
      ))
    }

    # Step C: Score each bearing and select best streams
    scores  <- score_bearings(scan_res$detect_df)
    streams <- select_streams(scores)

    # Step D: Save CSVs and plots
    summary_row <- save_scan_outputs(scan_res, scores, streams, run_dir,
                                      weather_skip = FALSE,
                                      weather_coverage = round(coverage * 100, 1))
    return(list(
      summary  = summary_row,
      scan_res = scan_res,
      streams  = streams
    ))

  }, error = function(e) {
    warning("Failed to process scan ", f, ": ", e$message)
    return(NULL)
  })
}

