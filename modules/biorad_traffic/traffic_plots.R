# =============================================================================
# traffic_plots.R — Visualization for bioRad Traffic Metrics Pipeline
# =============================================================================
#
# Provides:
#   plot_mtr_rtr_timeseries(integrated_df, out_dir)  → MTR and RTR vs time
#   plot_vid_vir_timeseries(integrated_df, out_dir)  → VID and VIR vs time
#   plot_ground_speed(integrated_df, out_dir)        → Ground speed vs time
#   plot_flight_direction(integrated_df, out_dir)    → Flight direction vs time
#   plot_altitude_profile(vpts_df, out_dir)          → Bird density heatmap (alt × time)
#   plot_event_summary(totals_df, out_dir)           → Summary bar chart
# =============================================================================

# ---------------------------------------------------------------------------
# Internal helper: format datetime axis labels in local time
# ---------------------------------------------------------------------------
bt_local_labels <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), tz = BT_LOCAL_TZ, format = "%H:%M\n%b %d")
}

# ---------------------------------------------------------------------------
# Internal: common ggplot theme for all traffic plots
# ---------------------------------------------------------------------------
bt_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle   = ggplot2::element_text(colour = "#555555", size = 10),
    axis.title      = ggplot2::element_text(size = 11),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  )
}

# ---------------------------------------------------------------------------
# Internal: save a ggplot to file
# ---------------------------------------------------------------------------
bt_save_plot <- function(p, out_dir, filename) {
  path <- file.path(out_dir, filename)
  ggplot2::ggsave(
    path, plot = p,
    width  = BT_PLOT_WIDTH_IN,
    height = BT_PLOT_HEIGHT_IN,
    dpi    = BT_PLOT_DPI
  )
  message("  Saved plot: ", path)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Internal: find a column by candidate names
# ---------------------------------------------------------------------------
find_col <- function(df, candidates) {
  found <- intersect(candidates, names(df))
  if (length(found) > 0) found[1] else NULL
}

# ---------------------------------------------------------------------------
# plot_mtr_rtr_timeseries()
#   Primary output: MTR and RTR (on secondary axis) vs local time.
# ---------------------------------------------------------------------------
plot_mtr_rtr_timeseries <- function(integrated_df, out_dir) {
  mtr_col      <- find_col(integrated_df, c("mtr", "MTR"))
  mtr_corr_col <- find_col(integrated_df, c("mtr_corrected"))
  rtr_col      <- find_col(integrated_df, c("rtr", "RTR"))

  if (is.null(mtr_col)) {
    warning("MTR column not found — skipping MTR/RTR plot.")
    return(invisible(NULL))
  }

  dt       <- as.POSIXct(integrated_df$datetime, tz = "UTC")
  mtr      <- integrated_df[[mtr_col]]
  mtr_corr <- if (!is.null(mtr_corr_col)) integrated_df[[mtr_corr_col]] else NULL

  # Scale RTR to peak MTR axis for dual-axis display
  rtr_scale <- NULL
  if (!is.null(rtr_col)) {
    rtr <- integrated_df[[rtr_col]]
    max_mtr_val <- if (!is.null(mtr_corr)) max(mtr_corr, na.rm = TRUE) else max(mtr, na.rm = TRUE)
    rtr_scale <- max_mtr_val / max(rtr, na.rm = TRUE)
  }

  title_str    <- paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Migration Traffic Rate")
  subtitle_str <- paste0(
    format(min(dt, na.rm = TRUE), tz = BT_LOCAL_TZ, format = "%b %d, %Y %H:%M"),
    " – ",
    format(max(dt, na.rm = TRUE), tz = BT_LOCAL_TZ, format = "%H:%M"),
    " (", BT_LOCAL_TZ, ")  |  RCS = ", BT_CROW_RCS_CM2, " cm²  |  ",
    "Transect direction: ", BT_FLIGHT_DIRECTION_DEG, "°\n",
    "Corrected for VVP speed dilution (expected cruise speed: ", BT_CROW_SPEED_KMPH, " km/h) & stream dilution (", BT_STREAM_SCALE_FACTOR, "x)"
  )

  plot_data <- data.frame(dt = dt, mtr = mtr)
  p <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = dt))

  # Plot area fill for corrected MTR
  p <- p + ggplot2::geom_area(
    ggplot2::aes(y = mtr),
    fill = "#E53935", alpha = 0.1, colour = NA
  )

  # Plot line for corrected MTR
  p <- p + ggplot2::geom_line(
    ggplot2::aes(y = mtr, colour = "MTR (speed & stream-corrected)"),
    linewidth = 1.2
  )

  color_values <- c("MTR (speed & stream-corrected)" = "#E53935")

  if (!is.null(rtr_col) && !is.null(rtr_scale) && is.finite(rtr_scale)) {
    color_values["RTR (scaled)"] <- "#E65100"
  }

  p <- p +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = color_values
    ) +
    ggplot2::scale_x_datetime(
      labels = bt_local_labels,
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::labs(
      title    = title_str,
      subtitle = subtitle_str,
      x        = paste0("Time (", BT_LOCAL_TZ, ")"),
      y        = expression(MTR~"[birds"~km^{-1}~h^{-1}~"]")
    ) +
    bt_theme()

  # Add RTR on a secondary axis if available
  if (!is.null(rtr_col) && !is.null(rtr_scale) && is.finite(rtr_scale)) {
    rtr_df <- data.frame(dt = dt, rtr_scaled = integrated_df[[rtr_col]] * rtr_scale)
    p <- p +
      ggplot2::geom_line(
        data = rtr_df,
        ggplot2::aes(x = dt, y = rtr_scaled, colour = "RTR (scaled)"),
        linewidth = 0.8, linetype = "dashed"
      ) +
      ggplot2::scale_y_continuous(
        sec.axis = ggplot2::sec_axis(
          ~ . / rtr_scale,
          name = expression(RTR~"[cm"^2~km^{-1}~h^{-1}~"]")
        )
      )
  }

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_mtr_rtr_timeseries.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_vid_vir_timeseries()
#   VID (vertically integrated density) and VIR (reflectivity) vs time.
# ---------------------------------------------------------------------------
plot_vid_vir_timeseries <- function(integrated_df, out_dir) {
  vid_col <- find_col(integrated_df, c("vid", "VID"))
  if (is.null(vid_col)) {
    warning("VID column not found — skipping VID/VIR plot.")
    return(invisible(NULL))
  }

  dt  <- as.POSIXct(integrated_df$datetime, tz = "UTC")
  vid <- integrated_df[[vid_col]]

  p <- ggplot2::ggplot(
    data = data.frame(dt = dt, vid = vid),
    ggplot2::aes(x = dt, y = vid)
  ) +
    ggplot2::geom_area(fill = "#4CAF50", alpha = 0.2, colour = NA) +
    ggplot2::geom_line(colour = "#2E7D32", linewidth = 1.1) +
    ggplot2::scale_x_datetime(
      labels = bt_local_labels,
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::labs(
      title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Vertically Integrated Bird Density"),
      subtitle = paste0("RCS = ", BT_CROW_RCS_CM2, " cm²"),
      x        = paste0("Time (", BT_LOCAL_TZ, ")"),
      y        = expression(VID~"[birds"~km^{-2}~"]")
    ) +
    bt_theme()

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_vid_timeseries.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_ground_speed()
#   Ground speed (km/h) vs time, with a reference line at 48 km/h.
# ---------------------------------------------------------------------------
plot_ground_speed <- function(integrated_df, out_dir) {
  gs_col <- find_col(integrated_df, c("ground_speed_kmph"))
  if (is.null(gs_col)) {
    warning("ground_speed_kmph column not found — skipping ground speed plot.")
    return(invisible(NULL))
  }

  dt <- as.POSIXct(integrated_df$datetime, tz = "UTC")
  gs <- integrated_df[[gs_col]]

  p <- ggplot2::ggplot(
    data = data.frame(dt = dt, gs = gs),
    ggplot2::aes(x = dt, y = gs)
  ) +
    ggplot2::geom_hline(
      yintercept = BT_CROW_SPEED_KMPH,
      colour = "#E53935", linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text", x = min(dt, na.rm = TRUE), y = BT_CROW_SPEED_KMPH + 1.5,
      label = paste0("Expected crow speed: ", BT_CROW_SPEED_KMPH, " km/h"),
      hjust = 0, colour = "#E53935", size = 3.5
    ) +
    ggplot2::geom_line(colour = "#6A1B9A", linewidth = 1.0) +
    ggplot2::geom_point(colour = "#6A1B9A", size = 1.5, alpha = 0.7) +
    ggplot2::scale_x_datetime(
      labels = bt_local_labels,
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::labs(
      title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Ground Speed"),
      subtitle = "Radar-derived speed (VVP) is biased towards zero by low-altitude ground clutter.\nExpected biological cruise speed of 48 km/h is used for calculations.",
      x        = paste0("Time (", BT_LOCAL_TZ, ")"),
      y        = "Ground speed [km h⁻¹]"
    ) +
    bt_theme()

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_ground_speed.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_flight_direction()
#   Flight direction (degrees) vs time as a scatter/line, with a reference
#   for the configured transect direction.
# ---------------------------------------------------------------------------
plot_flight_direction <- function(integrated_df, out_dir) {
  dir_col <- find_col(integrated_df, c("flight_dir_deg"))
  if (is.null(dir_col)) {
    warning("flight_dir_deg column not found — skipping flight direction plot.")
    return(invisible(NULL))
  }

  dt  <- as.POSIXct(integrated_df$datetime, tz = "UTC")
  dir <- integrated_df[[dir_col]]

  p <- ggplot2::ggplot(
    data = data.frame(dt = dt, dir = dir),
    ggplot2::aes(x = dt, y = dir)
  ) +
    ggplot2::geom_point(colour = "#F57C00", size = 2, alpha = 0.8) +
    ggplot2::geom_line(colour = "#F57C00", linewidth = 0.7, alpha = 0.6) +
    ggplot2::scale_x_datetime(
      labels = bt_local_labels,
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, 45)) +
    ggplot2::labs(
      title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Flight Direction"),
      subtitle = "Computed from vol2bird u/v velocity components (direction birds are flying TO)",
      x        = paste0("Time (", BT_LOCAL_TZ, ")"),
      y        = "Flight direction [° from N]"
    ) +
    bt_theme()

  # Add configured direction reference if set
  if (!is.null(BT_FLIGHT_DIRECTION_DEG) && !is.na(BT_FLIGHT_DIRECTION_DEG)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = BT_FLIGHT_DIRECTION_DEG,
        colour = "#1565C0", linetype = "dashed", linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text", x = min(dt, na.rm = TRUE), y = BT_FLIGHT_DIRECTION_DEG + 8,
        label = paste0("Configured direction: ", BT_FLIGHT_DIRECTION_DEG, "°"),
        hjust = 0, colour = "#1565C0", size = 3.5
      )
  }

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_flight_direction.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_altitude_profile()
#   Heatmap of bird density (dens or eta) as a function of altitude and time.
#   Uses the long-format vpts_df from summarise_vpts().
# ---------------------------------------------------------------------------
plot_altitude_profile <- function(vpts_df, out_dir) {
  if (is.null(vpts_df) || nrow(vpts_df) == 0) {
    warning("vpts_df is empty — skipping altitude profile heatmap.")
    return(invisible(NULL))
  }

  dens_col <- find_col(vpts_df, c("dens", "eta"))
  if (is.null(dens_col)) {
    warning("dens column not found in vpts_df — skipping altitude profile.")
    return(invisible(NULL))
  }

  dt    <- as.POSIXct(vpts_df$datetime, tz = "UTC")
  alt   <- vpts_df$height_m
  dens  <- pmax(0, vpts_df[[dens_col]], na.rm = TRUE)

  p <- ggplot2::ggplot(
    data = data.frame(dt = dt, alt = alt, dens = dens),
    ggplot2::aes(x = dt, y = alt, fill = dens)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(
      name   = "Bird density\n[km⁻³]",
      option = "inferno",
      na.value = "grey90",
      trans  = "sqrt"
    ) +
    ggplot2::scale_x_datetime(
      labels = bt_local_labels,
      breaks = scales::pretty_breaks(n = 8)
    ) +
    ggplot2::labs(
      title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Altitude–Time Bird Density Profile"),
      subtitle = paste0("RCS = ", BT_CROW_RCS_CM2, " cm²"),
      x        = paste0("Time (", BT_LOCAL_TZ, ")"),
      y        = "Altitude AGL [m]"
    ) +
    bt_theme()

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_altitude_profile.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_event_summary()
#   Simple bar/text summary of key event totals for quick-look.
# ---------------------------------------------------------------------------
plot_event_summary <- function(totals_df, out_dir) {
  if (is.null(totals_df) || nrow(totals_df) == 0) {
    warning("totals_df is empty — skipping event summary plot.")
    return(invisible(NULL))
  }

  # Build a display table of key metrics
  metrics <- list(
    c("Total Migration Traffic (MT)",          "total_MT", "birds km⁻¹"),
    c("Peak Traffic Rate (MTR)",               "peak_MTR", "birds km⁻¹ h⁻¹"),
    c("Flight Ground Speed",                   "mean_ground_speed_kmph", "km h⁻¹"),
    c("Flight Direction",                      "mean_flight_dir_deg", "°")
  )

  rows <- lapply(metrics, function(m) {
    label <- m[1]; col <- m[2]; unit <- m[3]
    val <- if (col %in% names(totals_df)) {
      v <- totals_df[[col]][1]
      if (!is.null(v) && !is.na(v)) round(as.numeric(v), 1) else NA
    } else NA
    data.frame(Metric = label, Value = val, Unit = unit, stringsAsFactors = FALSE)
  })

  summary_tbl <- do.call(rbind, rows)
  summary_tbl <- summary_tbl[!is.na(summary_tbl$Value), ]

  if (nrow(summary_tbl) == 0) {
    warning("No valid totals to plot.")
    return(invisible(NULL))
  }

  # Colour bar chart
  summary_tbl$label_str <- paste0(summary_tbl$Value, "\n", summary_tbl$Unit)

  p <- ggplot2::ggplot(
    summary_tbl,
    ggplot2::aes(x = reorder(Metric, -Value), y = Value, fill = Metric)
  ) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_str),
      vjust = -0.4, size = 3.2
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Event Summary"),
      subtitle = paste0(
        totals_df$event_start_utc[1], " to ", totals_df$event_end_utc[1], " UTC  |  ",
        totals_df$n_scans[1], " scans"
      ),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 15, hjust = 1, size = 9),
      plot.title   = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )

  bt_save_plot(p, out_dir, paste0(BT_RADAR_ID, "_event_summary.png"))
  invisible(p)
}

# ---------------------------------------------------------------------------
# plot_peak_reflectivity()
#   Loads the pvol at the peak traffic time, projects it to a PPI,
#   and saves a standard bioRad PPI reflectivity map plot.
# ---------------------------------------------------------------------------
plot_peak_reflectivity <- function(peak_pvol_path, out_dir) {
  if (is.null(peak_pvol_path) || !file.exists(peak_pvol_path)) {
    warning("Peak pvol file not found — skipping peak reflectivity map.")
    return(invisible(NULL))
  }

  message("Generating peak reflectivity map from: ", basename(peak_pvol_path))

  # Load polar volume
  pvol <- tryCatch({
    bioRad::read_pvolfile(peak_pvol_path)
  }, error = function(e) {
    warning("Could not read pvol file: ", e$message)
    return(NULL)
  })
  
  if (is.null(pvol)) return(invisible(NULL))

  # Project to PPI using configured elevation angle
  ppi <- tryCatch({
    scan_obj <- bioRad::get_scan(pvol, elev = BT_ELEVATION_DEG)
    bioRad::project_as_ppi(scan_obj)
  }, error = function(e) {
    warning("Could not project pvol to PPI: ", e$message)
    return(NULL)
  })

  if (is.null(ppi)) return(invisible(NULL))

  # Standard bioRad PPI plot
  p <- tryCatch({
    plot(ppi, param = "DBZH") +
      ggplot2::labs(
        title    = paste0(BT_SITE_NAME, " — ", BT_RADAR_ID, "  |  Peak Reflectivity"),
        subtitle = paste0("Time: ", format(ppi$datetime, tz = BT_LOCAL_TZ, format = "%Y-%m-%d %H:%M (%Z)"))
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        plot.subtitle = ggplot2::element_text(colour = "#555555", size = 9)
      )
  }, error = function(e) {
    warning("Failed to generate PPI plot: ", e$message)
    return(NULL)
  })

  if (is.null(p)) return(invisible(NULL))

  # Save to file
  out_file <- file.path(out_dir, paste0(BT_RADAR_ID, "_peak_reflectivity.png"))
  ggplot2::ggsave(out_file, plot = p, width = 7, height = 6.5, dpi = BT_PLOT_DPI)
  message("  Saved peak reflectivity plot: ", out_file)
  invisible(out_file)
}
