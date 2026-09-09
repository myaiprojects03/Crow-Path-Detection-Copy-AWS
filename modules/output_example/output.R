# =============================================================================
# output.R — Settings file, per-scan CSVs/plots, and scan index
# =============================================================================

# Write parameter values used in this run to a plain-text file
write_settings_file <- function(run_dir) {
  lines <- c(
    paste("ROOST_LAT =",                   ROOST_LAT),
    paste("ROOST_LON =",                   ROOST_LON),
    paste("RADAR_ID =",                    RADAR_ID),
    paste("DATE_START =",                  format(DATE_START, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
    paste("DATE_END =",                    format(DATE_END,   "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
    paste("DETECTION_MIN_DIST_KM =",      DETECTION_MIN_DIST_KM),
    paste("DETECTION_MAX_DIST_KM =",      DETECTION_MAX_DIST_KM),
    paste("RADAR_SUPPRESS_DIST_KM =",     RADAR_SUPPRESS_DIST_KM),
    paste("PLOT_MIN_DIST_KM =",           PLOT_MIN_DIST_KM),
    paste("PLOT_MAX_DIST_KM =",           PLOT_MAX_DIST_KM),
    paste("BEARING_STEP_DEG =",           BEARING_STEP_DEG),
    paste("CORRIDOR_HALF_WIDTH_KM =",     CORRIDOR_HALF_WIDTH_KM),
    paste("FLANK_OUTER_WIDTH_KM =",       FLANK_OUTER_WIDTH_KM),
    paste("DETECT_DBZH_MIN =",            DETECT_DBZH_MIN),
    paste("CONTRAST_THRESHOLD =",         CONTRAST_THRESHOLD),
    paste("MIN_CORRIDOR_PIXELS =",        MIN_CORRIDOR_PIXELS),
    paste("MIN_CONTIGUOUS_BINS =",        MIN_CONTIGUOUS_BINS),
    paste("LOCAL_PROMINENCE_THRESHOLD =", LOCAL_PROMINENCE_THRESHOLD),
    paste("USE_MISTNET_FOR_DETECTION =",  USE_MISTNET_FOR_DETECTION),
    paste("MASK_CELL =",                  MASK_CELL),
    paste("MASK_WEATHER =",               MASK_WEATHER),
    paste("MASK_RHOHV =",                MASK_RHOHV),
    paste("MAP_DBZH_MIN =",              MAP_DBZH_MIN),
    paste("MAP_DBZH_MAX =",              MAP_DBZH_MAX),
    paste("MAP_DBZH_TRANSPARENT_BELOW =",MAP_DBZH_TRANSPARENT_BELOW),
    paste("MAP_PIXEL_SHAPE =",           MAP_PIXEL_SHAPE),
    paste("MAP_PIXEL_SIZE =",            MAP_PIXEL_SIZE),
    paste("MAP_PIXEL_ALPHA =",           MAP_PIXEL_ALPHA),
    paste("MAP_ZOOM =",                  ifelse(is.null(MAP_ZOOM), "NULL", MAP_ZOOM)),
    paste("OVERWRITE_RUN_OUTPUT =",      OVERWRITE_RUN_OUTPUT)
  )
  writeLines(lines, con = file.path(run_dir, "settings_used.txt"))
}

# Write CSVs and plots for one scan; return a one-row index data frame
save_scan_outputs <- function(scan_result, scores, streams, run_dir,
                              weather_skip = FALSE,
                              weather_coverage = 0.0) {
  scan_dir <- file.path(run_dir, scan_stub(scan_result$scan_time))
  dir.create(scan_dir, recursive = TRUE, showWarnings = FALSE)

  # Retrieve VCP recorded in scan_result (set by process_scan_file in io.R)
  vcp <- if (!is.null(scan_result$vcp) && nzchar(scan_result$vcp)) scan_result$vcp else "unknown"

  # Build weather flag string
  weather_flag <- if (weather_skip) {
    paste0("weather_skip (", weather_coverage, "% coverage)")
  } else {
    "clear"
  }

  # --- bearing scores CSV ---
  scores_out <- scores[order(scores$bearing_deg), ]
  scores_out$scan_time <- format(scan_result$scan_time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  scores_out$vcp           <- vcp
  scores_out$weather_flag  <- weather_flag
  scores_out <- scores_out[, c(
    "scan_time", "vcp", "weather_flag", "bearing_deg", "compass",
    "corridor_eta_mean", "corridor_eta_sum", "corridor_density",
    "flank_eta_mean", "contrast_ratio", "local_background_eta",
    "local_prominence_ratio", "corridor_valid_pixels", "flank_valid_pixels",
    "corridor_total_pixels", "max_contiguous_bins", "contiguous_extent_km",
    "max_extent_km", "quality_score", "stream_detected"
  )]
  utils::write.csv(
    scores_out,
    file      = file.path(scan_dir, "bearing_scores.csv"),
    row.names = FALSE
  )

  # --- detected streams CSV ---
  if (nrow(streams) > 0) {
    streams$vcp          <- vcp
    streams$weather_flag <- weather_flag
    utils::write.csv(
      streams,
      file      = file.path(scan_dir, "detected_streams.csv"),
      row.names = FALSE
    )
  } else {
    empty_streams <- data.frame(
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
      supporting_bearings    = character(0),
      vcp                    = character(0),
      weather_flag           = character(0)
    )
    utils::write.csv(
      empty_streams,
      file      = file.path(scan_dir, "detected_streams.csv"),
      row.names = FALSE
    )
  }

  # --- plots ---
  save_raw_radar_map(
    raw_ppi    = scan_result$raw_ppi,
    streams_df = streams,
    scan_time  = scan_result$scan_time,
    out_file   = file.path(scan_dir, "raw_radar_map.png")
  )
  save_bearing_profile(
    scores     = scores,
    streams_df = streams,
    scan_time  = scan_result$scan_time,
    out_file   = file.path(scan_dir, "bearing_profile.png")
  )

  # --- one-row index entry ---
  data.frame(
    scan_time = format(scan_result$scan_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    vcp       = vcp,
    weather_flag = weather_flag,
    detected_bearings = if (nrow(streams) == 0) {
      "none"
    } else {
      paste(round(streams$bearing_deg), collapse = ", ")
    },
    n_streams = nrow(streams)
  )
}

