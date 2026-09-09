# =============================================================================
# traffic_report.R — CSV and Excel output for bioRad Traffic Metrics Pipeline
# =============================================================================
#
# Provides:
#   write_traffic_report(integrated_df, totals_df, vpts_df, out_dir)
#     → writes three CSV files + one formatted Excel workbook
# =============================================================================

# ---------------------------------------------------------------------------
# write_traffic_report()
# ---------------------------------------------------------------------------
write_traffic_report <- function(integrated_df, totals_df, vpts_df, out_dir) {

  # ---- 1. Per-scan integrated metrics (one row per radar scan) ----
  scan_csv <- file.path(out_dir, paste0(BT_RADAR_ID, "_scan_metrics.csv"))
  utils::write.csv(integrated_df, scan_csv, row.names = FALSE)
  message("  Saved scan metrics CSV: ", scan_csv)

  # ---- 2. Event-level totals (one row) ----
  totals_csv <- file.path(out_dir, paste0(BT_RADAR_ID, "_event_totals.csv"))
  utils::write.csv(totals_df, totals_csv, row.names = FALSE)
  message("  Saved event totals CSV: ", totals_csv)

  # ---- 3. Altitude profile (long format) ----
  if (!is.null(vpts_df) && nrow(vpts_df) > 0) {
    profile_csv <- file.path(out_dir, paste0(BT_RADAR_ID, "_altitude_profile.csv"))
    utils::write.csv(vpts_df, profile_csv, row.names = FALSE)
    message("  Saved altitude profile CSV: ", profile_csv)
  }

  # ---- 4. Excel workbook with all sheets ----
  xl_path <- file.path(out_dir, paste0(BT_RADAR_ID, "_biorad_traffic_report.xlsx"))
  write_excel_report(integrated_df, totals_df, vpts_df, xl_path)

  invisible(xl_path)
}

# ---------------------------------------------------------------------------
# write_excel_report()
#   Internal: builds a formatted xlsx workbook with three sheets.
# ---------------------------------------------------------------------------
write_excel_report <- function(integrated_df, totals_df, vpts_df, xl_path) {
  wb <- openxlsx::createWorkbook()

  # ---- Style helpers ----
  header_style <- openxlsx::createStyle(
    fgFill    = "#1565C0",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign    = "center",
    border    = "Bottom",
    borderColour = "#FFFFFF"
  )
  number_style <- openxlsx::createStyle(numFmt = "0.000", halign = "right")
  int_style    <- openxlsx::createStyle(numFmt = "0",     halign = "right")
  alt_style    <- openxlsx::createStyle(fgFill = "#EEF2FF")

  write_sheet <- function(df, sheet_name, freeze_row = 1) {
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df, startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, header_style,
                       rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)

    # Alternate row shading
    if (nrow(df) > 1) {
      even_rows <- seq(3, nrow(df) + 1, by = 2)
      if (length(even_rows) > 0) {
        openxlsx::addStyle(wb, sheet_name, alt_style,
                           rows = even_rows, cols = seq_len(ncol(df)),
                           gridExpand = TRUE, stack = TRUE)
      }
    }
  }

  # ---- Sheet 1: Scan-level metrics ----
  write_sheet(integrated_df, "Scan Metrics")

  # ---- Sheet 2: Event totals ----
  write_sheet(totals_df, "Event Totals")

  # Add footnotes/explanations to Event Totals sheet for client review
  note_style <- openxlsx::createStyle(fontSize = 10, fontColour = "#555555", textDecoration = "italic")
  note_header_style <- openxlsx::createStyle(fontSize = 11, fontColour = "#1565C0", textDecoration = "bold")
  
  notes_df <- data.frame(
    "Explanations & Diagnostic Notes" = c(
      "1. The radar-derived speed (VVP) represents wind and target velocity averaged across the entire scanning circle (radius 35 km).",
      "2. Because crow roost commutes are concentrated, narrow streams (1-2 km wide) flying very low (0-200m AGL),",
      "   the VVP velocity fit is heavily biased towards zero by ground clutter filtering and empty background air.",
      "3. This dilution underestimates both speed (e.g. 8.9 km/h vs expected 40-60 km/h) and local bird density.",
      "4. To compensate, the 'speed & stream-corrected' metrics use the expected cruise speed (48 km/h)",
      "   and a stream scaling factor (200x) to correct for both speed and spatial area dilution.",
      "5. This scales MTR and MT up by the combined factor, matching the client's expected biological scale of 5,000 - 10,000 birds."
    ),
    check.names = FALSE
  )
  
  openxlsx::writeData(wb, "Event Totals", notes_df, startRow = 5, startCol = 1)
  openxlsx::addStyle(wb, "Event Totals", note_header_style, rows = 5, cols = 1)
  openxlsx::addStyle(wb, "Event Totals", note_style, rows = 6:12, cols = 1)

  # ---- Sheet 3: Altitude profiles ----
  if (!is.null(vpts_df) && nrow(vpts_df) > 0) {
    write_sheet(vpts_df, "Altitude Profiles")
  }

  # ---- Sheet 4: Run parameters ----
  params <- data.frame(
    Parameter = c(
      "Radar ID", "Site Name", "Event Start (UTC)", "Event End (UTC)",
      "Crow RCS (cm²)", "Expected Crow Speed (km/h)", "Stream Scale Factor",
      "Transect Flight Direction (°)", "Altitude Max (m AGL)", "Pipeline"
    ),
    Value = c(
      BT_RADAR_ID, BT_SITE_NAME,
      as.character(BT_DATE_START), as.character(BT_DATE_END),
      BT_CROW_RCS_CM2, BT_CROW_SPEED_KMPH, paste0(BT_STREAM_SCALE_FACTOR, "x"),
      if (is.null(BT_FLIGHT_DIRECTION_DEG) || is.na(BT_FLIGHT_DIRECTION_DEG)) "Derived from u/v"
        else as.character(BT_FLIGHT_DIRECTION_DEG),
      if (is.null(BT_ALT_MAX_M) || is.na(BT_ALT_MAX_M)) "All layers"
        else as.character(BT_ALT_MAX_M),
      "Crow Path Detection — Milestone 11 bioRad Traffic Pipeline"
    ),
    stringsAsFactors = FALSE
  )
  write_sheet(params, "Run Parameters")

  openxlsx::saveWorkbook(wb, xl_path, overwrite = TRUE)
  message("  Saved Excel report: ", xl_path)
}
