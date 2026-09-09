# =============================================================================
# biorad_traffic_main.R — Standalone bioRad Traffic Metrics Pipeline
# =============================================================================
#
# PURPOSE:
#   Computes crow migration traffic metrics (MTR, MT, RTR, RT, VID, VIR)
#   for a chosen radar and time window using bioRad's vertical profile framework.
#
# USAGE:
#   Rscript biorad_traffic_main.R
#
#   All configuration is in:
#     modules/biorad_traffic/traffic_config.R
#
# OUTPUTS (written to output/biorad_traffic/<RADAR_ID>_<date>/):
#   - <RADAR_ID>_scan_metrics.csv          — per-scan MTR, RTR, VID, VIR, speed, direction
#   - <RADAR_ID>_event_totals.csv          — total MT, RT, peak MTR, mean speed
#   - <RADAR_ID>_altitude_profile.csv      — bird density by altitude and time
#   - <RADAR_ID>_biorad_traffic_report.xlsx — formatted Excel workbook (4 sheets)
#   - <RADAR_ID>_mtr_rtr_timeseries.png    — MTR and RTR over time
#   - <RADAR_ID>_vid_timeseries.png        — VID over time
#   - <RADAR_ID>_ground_speed.png          — ground speed vs expected crow speed
#   - <RADAR_ID>_flight_direction.png      — flight direction over time
#   - <RADAR_ID>_altitude_profile.png      — altitude-time heatmap
#   - <RADAR_ID>_event_summary.png         — bar chart summary of key totals
#
# NOTES:
#   - This is a standalone pipeline. It does NOT call main.R or any GUI.
#   - vol2bird is run on every pvol file to produce vertical profiles.
#     This is the key step that bioRad's traffic metrics depend on.
#   - TDWR radars (TIAD, TDCA) require the custom station file at
#     data/locations.dat (already present from the main pipeline).
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  Crow Path Detection — Milestone 11\n")
cat("  bioRad Traffic Metrics Pipeline\n")
cat("=================================================================\n\n")

# ---------------------------------------------------------------------------
# 0. Working directory — must be the project root
# ---------------------------------------------------------------------------
# If running from a different location, uncomment and adjust:
# setwd("C:/Users/abcd/Desktop/OC-04-2026-020/Crow Path Detection")

# ---------------------------------------------------------------------------
# 1. Library path setup (mirrors install_deps.R approach)
# ---------------------------------------------------------------------------
user_lib <- Sys.getenv("R_LIBS_USER")
if (nchar(user_lib) == 0) {
  user_lib <- file.path(
    Sys.getenv("USERPROFILE"), "R", "win-library",
    paste(R.version$major, substr(R.version$minor, 1, 1), sep = ".")
  )
}
.libPaths(unique(c(user_lib, .libPaths())))

# ---------------------------------------------------------------------------
# 2. Load required packages
# ---------------------------------------------------------------------------
required_pkgs <- c("bioRad", "vol2birdR", "ggplot2", "openxlsx", "scales")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Required package '", pkg, "' is not installed.\n",
      "Run Rscript Dependencies/install_deps.R first."
    )
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}

# ---------------------------------------------------------------------------
# 3. Source pipeline configuration and modules
# ---------------------------------------------------------------------------
source("modules/biorad_traffic/traffic_config.R")
source("modules/biorad_traffic/vp_processing.R")
source("modules/biorad_traffic/traffic_metrics.R")
source("modules/biorad_traffic/traffic_plots.R")
source("modules/biorad_traffic/traffic_report.R")

# Also need the download_radar() function from the main pipeline's io.R.
# We source only the helper functions — this does NOT run main.R.
# First load detection_config.R minimally so io.R's globals are satisfied.
source("modules/config/general/detection_config.R")
source("modules/io/io.R")

cat("Configuration loaded.\n")
cat("  Radar         :", BT_RADAR_ID, "\n")
cat("  Site          :", BT_SITE_NAME, "\n")
cat("  Window (UTC)  :", format(BT_DATE_START), "to", format(BT_DATE_END), "\n")
cat("  Crow RCS      :", BT_CROW_RCS_CM2, "cm²\n")
cat("  Flight dir    :",
    if (is.null(BT_FLIGHT_DIRECTION_DEG) || is.na(BT_FLIGHT_DIRECTION_DEG))
      "derived from u/v" else paste0(BT_FLIGHT_DIRECTION_DEG, "°"),
    "\n")
cat("\n")

# ---------------------------------------------------------------------------
# 4. Prepare output directory
# ---------------------------------------------------------------------------
run_label  <- format(BT_DATE_START, "%Y%m%d")
out_dir    <- file.path(BT_OUTPUT_DIR, paste0(BT_RADAR_ID, "_", run_label))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat("Output directory:", out_dir, "\n\n")

# ---------------------------------------------------------------------------
# 5. Download / locate pvol files
# ---------------------------------------------------------------------------
cat("--- Step 1: Locating radar files ---\n")

# Override DATA_DIR for the download function (which uses the global DATA_DIR)
DATA_DIR <- BT_DATA_DIR
RADAR_ID <- BT_RADAR_ID
CUSTOM_STATION_FILE <- BT_CUSTOM_STATION_FILE
CUSTOM_STATION_FILE_FALLBACK <- BT_CUSTOM_STATION_FILE_FALLBACK

# Use the shared io.R downloader
file_list <- tryCatch(
  download_radar(BT_DATE_START, BT_DATE_END, BT_RADAR_ID, BT_DATA_DIR),
  error = function(e) stop("Failed to locate radar files: ", e$message)
)
cat("Found", length(file_list), "pvol file(s) in the requested window.\n\n")

# ---------------------------------------------------------------------------
# 6. Compute vertical profile (vp) for each scan
# ---------------------------------------------------------------------------
cat("--- Step 2: Computing vertical profiles via vol2bird ---\n")
cat("(This step processes each scan individually — may take several minutes)\n\n")

vp_list <- vector("list", length(file_list))
for (i in seq_along(file_list)) {
  cat("[", i, "/", length(file_list), "] ")
  vp_list[[i]] <- compute_vp_for_file(file_list[[i]])
}

# Filter out failed scans
n_before <- length(vp_list)
vp_list  <- Filter(Negate(is.null), vp_list)
n_after  <- length(vp_list)
if (n_after < n_before) {
  cat("\nWarning:", n_before - n_after, "scan(s) failed vol2bird and were skipped.\n")
}
cat("\nSuccessfully processed", n_after, "vertical profile(s).\n\n")

if (n_after == 0) {
  stop("No vertical profiles were generated. Cannot continue.")
}

# ---------------------------------------------------------------------------
# 7. Regularize + Assemble vpts time-series
# ---------------------------------------------------------------------------
cat("--- Step 3: Assembling and regularizing vpts ---\n")
vpts <- build_vpts(vp_list)
cat("vpts assembled:", length(vpts$datetime), "time steps,",
    length(vpts$height), "altitude layers.\n")

# Regularize onto uniform time grid (required by integrate_profile)
vpts <- bt_regularize_vpts(vpts)

# Apply altitude filter if configured
vpts <- filter_vpts_altitude(vpts)
cat("\n")

# ---------------------------------------------------------------------------
# 8. Apply crow RCS
# ---------------------------------------------------------------------------
cat("--- Step 4: Applying crow RCS =", BT_CROW_RCS_CM2, "cm² ---\n")
vpts <- set_crow_rcs(vpts)
cat("\n")

# ---------------------------------------------------------------------------
# 9. Compute integrated traffic metrics
# ---------------------------------------------------------------------------
cat("--- Step 5: Computing MTR, RTR, VID, VIR ---\n")
integrated_df <- compute_integrated_profiles(vpts)
# ---------------------------------------------------------------------------
# 10. Display sample output
# ---------------------------------------------------------------------------
cat("\nSample of integrated metrics (first 5 rows):\n")
print(utils::head(integrated_df[, intersect(
  c("datetime", "datetime_local", "mtr", "rtr", "vid", "vir", "mt", "rt",
    "ff", "dd", "ground_speed_kmph", "flight_dir_deg"),
  names(integrated_df)
)], 5))
cat("\n")

# ---------------------------------------------------------------------------
# 11. Compute event totals (MT, RT)
# ---------------------------------------------------------------------------
cat("--- Step 6: Computing event totals (MT, RT) ---\n")
totals_df <- compute_event_totals(integrated_df)
cat("\nEvent totals:\n")
print(t(totals_df))
cat("\n")

# ---------------------------------------------------------------------------
# 12. Extract long-format vpts summary for altitude profile plot
# ---------------------------------------------------------------------------
cat("--- Step 7: Extracting altitude profile data ---\n")
vpts_df <- summarise_vpts(vpts)
cat("  Altitude profile:", if (!is.null(vpts_df)) nrow(vpts_df) else 0, "rows.\n\n")

# ---------------------------------------------------------------------------
# 13. Generate plots
# ---------------------------------------------------------------------------
cat("--- Step 8: Generating plots ---\n")
tryCatch(plot_mtr_rtr_timeseries(integrated_df, out_dir),
         error = function(e) warning("MTR/RTR plot failed: ", e$message))
tryCatch(plot_vid_vir_timeseries(integrated_df, out_dir),
         error = function(e) warning("VID plot failed: ", e$message))
tryCatch(plot_ground_speed(integrated_df, out_dir),
         error = function(e) warning("Ground speed plot failed: ", e$message))
tryCatch(plot_flight_direction(integrated_df, out_dir),
         error = function(e) warning("Flight direction plot failed: ", e$message))
tryCatch(plot_altitude_profile(vpts_df, out_dir),
         error = function(e) warning("Altitude profile plot failed: ", e$message))
tryCatch(plot_event_summary(totals_df, out_dir),
         error = function(e) warning("Event summary plot failed: ", e$message))

# Generate peak reflectivity map
cat("  Finding peak MTR scan for reflectivity map...\n")
peak_pvol_path <- NULL
if (nrow(integrated_df) > 0) {
  peak_idx <- which.max(integrated_df$mtr)
  if (length(peak_idx) > 0 && !is.na(peak_idx)) {
    peak_time <- integrated_df$datetime[peak_idx]
    file_names <- basename(file_list)
    regex_pattern <- paste0("^", BT_RADAR_ID, "(\\d{8})_(\\d{6})(?:_V\\d{2})?(?:\\.gz)?$")
    scan_times <- as.POSIXct(
      sub(regex_pattern, "\\1 \\2", file_names),
      format = "%Y%m%d %H%M%S",
      tz = "UTC"
    )
    time_diffs <- abs(as.numeric(scan_times - peak_time))
    closest_idx <- which.min(time_diffs)
    if (length(closest_idx) > 0 && !is.na(closest_idx)) {
      peak_pvol_path <- file_list[closest_idx]
    }
  }
}
tryCatch(plot_peak_reflectivity(peak_pvol_path, out_dir),
         error = function(e) warning("Peak reflectivity plot failed: ", e$message))
cat("\n")

# ---------------------------------------------------------------------------
# 14. Write report files
# ---------------------------------------------------------------------------
cat("--- Step 9: Writing CSV and Excel report ---\n")
xl_path <- write_traffic_report(integrated_df, totals_df, vpts_df, out_dir)

# ---------------------------------------------------------------------------
# 15. Final summary
# ---------------------------------------------------------------------------
cat("\n")
cat("=================================================================\n")
cat("  Pipeline complete.\n")
cat("=================================================================\n")
cat("  Radar         :", BT_RADAR_ID, "\n")
cat("  Site          :", BT_SITE_NAME, "\n")
cat("  Scans processed:", n_after, "\n")

# Print key metrics
mtr_col <- intersect(c("mtr", "MTR"), names(integrated_df))[1]
if (!is.null(mtr_col) && !is.na(mtr_col)) {
  mtr_vals <- integrated_df[[mtr_col]]
  cat("  Peak MTR      :", round(max(mtr_vals, na.rm = TRUE), 2), "birds km⁻¹ h⁻¹\n")
}
if ("total_MT" %in% names(totals_df)) {
  cat("  Total MT      :", round(totals_df$total_MT[1], 1), "birds km⁻¹\n")
}
if ("mean_ground_speed_kmph" %in% names(totals_df)) {
  cat("  Mean speed    :", totals_df$mean_ground_speed_kmph[1], "km h⁻¹",
      " (expected:", BT_CROW_SPEED_KMPH, "km h⁻¹)\n")
}
cat("  Output dir    :", out_dir, "\n")
cat("  Excel report  :", xl_path, "\n")
cat("=================================================================\n\n")
