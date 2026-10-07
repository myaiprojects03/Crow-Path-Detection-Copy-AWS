# =============================================================================
# plotting.R — Raw radar map and bearing profile chart generation
#
# All display knobs live in config.R:
#   MAP_DBZH_MIN / MAX          — colour scale floor and ceiling (dBZ)
#   MAP_DBZH_TRANSPARENT_BELOW  — pixels below this value are hidden (NA)
#   MAP_PIXEL_SHAPE             — pch code for radar pixels (15=sq, 16=circle)
#   MAP_PIXEL_SIZE              — ggplot2 size units for radar pixels
#   MAP_ZOOM                    — direct OSM integer zoom (NULL = auto, ~9 for
#                                 this area). bioRad passes this straight to
#                                 rosm/maptiles. Larger integer = more detail +
#                                 smaller label text. Try 11 or 12 to shrink
#                                 place names. If label size is the only issue,
#                                 MAP_BASEMAP <- "cartolight" is simpler.
#   MAP_BASEMAP                 — tile provider: "osm", "cartolight",
#                                 "cartodb_positron", "osm-nolabels"
#   MAP_XLIM / YLIM             — bounding box (lon/lat)
#   MAP_WIDTH_IN / HEIGHT_IN / DPI   — output PNG dimensions
#   PROFILE_WIDTH_IN / HEIGHT_IN / DPI
# =============================================================================

# Build start/end/label coordinates for stream overlay line segments
build_overlay_segments <- function(streams_df, inner_km = PLOT_MIN_DIST_KM, outer_km = NULL) {
  if (nrow(streams_df) == 0) return(data.frame())

  rows <- lapply(seq_len(nrow(streams_df)), function(i) {
    stream_outer_km <- if (is.null(outer_km)) {
      max(inner_km + 0.5, min(streams_df$max_extent_km[i], PLOT_MAX_DIST_KM))
    } else {
      min(outer_km, PLOT_MAX_DIST_KM)
    }
    near_pt <- destination_point(ROOST_LAT, ROOST_LON, streams_df$bearing_deg[i], inner_km)
    far_pt  <- destination_point(ROOST_LAT, ROOST_LON, streams_df$bearing_deg[i], stream_outer_km)
    lab_pt  <- destination_point(ROOST_LAT, ROOST_LON, streams_df$bearing_deg[i], stream_outer_km + 2.2)
    near_xy <- web_mercator_xy(near_pt[1], near_pt[2])
    far_xy  <- web_mercator_xy(far_pt[1],  far_pt[2])
    
    # Clamp label to stay within map panel limits with sufficient padding for entire box
    lims       <- map_mercator_limits()
    span_x     <- diff(lims$xlim)
    span_y     <- diff(lims$ylim)
    pad_x      <- span_x * 0.065
    pad_y      <- span_y * 0.045
    raw_lab_xy <- web_mercator_xy(lab_pt[1], lab_pt[2])
    clamped_x  <- pmax(lims$xlim[1] + pad_x, pmin(raw_lab_xy$x, lims$xlim[2] - pad_x))
    clamped_y  <- pmax(lims$ylim[1] + pad_y, pmin(raw_lab_xy$y, lims$ylim[2] - pad_y))
    lab_xy     <- data.frame(x = clamped_x, y = clamped_y)
    
    data.frame(
      bearing_deg   = streams_df$bearing_deg[i],
      compass       = streams_df$compass[i],
      max_extent_km = streams_df$max_extent_km[i],
      x = near_xy$x, y = near_xy$y,
      xend = far_xy$x, yend = far_xy$y,
      label_x = lab_xy$x, label_y = lab_xy$y
    )
  })

  do.call(rbind, rows)
}

# Fix map extent (Web Mercator metres); clip layers to panel (white outside).
apply_map_coord_limits <- function(p) {
  lims <- map_mercator_limits()
  p + ggplot2::coord_sf(
    xlim = lims$xlim, ylim = lims$ylim,
    datum = 4326, expand = FALSE, clip = "off"
  )
}

# Keep only rows inside the configured lon/lat map window
filter_to_map_bounds <- function(df, lon_col = "lon", lat_col = "lat") {
  df[
    df[[lon_col]] >= MAP_XLIM[1] & df[[lon_col]] <= MAP_XLIM[2] &
      df[[lat_col]] >= MAP_YLIM[1] & df[[lat_col]] <= MAP_YLIM[2],
    ,
    drop = FALSE
  ]
}

# km-from-roost ticks on top (E/W) and right (N/S). Drawn last so labels stay visible.
add_km_secondary_axes <- function(p) {
  lims <- map_mercator_limits()
  
  # Calculate 5 km spacing breaks for both axes
   r_east <- range(roost_km_east_from_lon(MAP_XLIM))
   km_east_breaks <- seq(from = floor(r_east[1] / 5) * 5, to = ceiling(r_east[2] / 5) * 5, by = 5)
    # Keep only breaks within east bounds and remove specific out-of-grid ticks (15)
    km_east_breaks <- km_east_breaks[km_east_breaks >= r_east[1] & km_east_breaks <= r_east[2]]
    km_east_breaks <- km_east_breaks[!km_east_breaks %in% c(15)]

    r_north <- range(roost_km_north_from_lat(MAP_YLIM))
    km_north_breaks <- seq(from = floor(r_north[1] / 5) * 5, to = ceiling(r_north[2] / 5) * 5, by = 5)
    # Keep only breaks within north bounds and remove specific out-of-grid ticks (25)
    km_north_breaks <- km_north_breaks[km_north_breaks >= r_north[1] & km_north_breaks <= r_north[2]]
    km_north_breaks <- km_north_breaks[!km_north_breaks %in% c(25)]

  top_df <- data.frame(
    km  = km_east_breaks,
    lon = vapply(km_east_breaks, lon_from_km_east, numeric(1)),
    stringsAsFactors = FALSE
  )
  top_df$x <- web_mercator_xy(top_df$lon, ROOST_LAT)$x

  right_df <- data.frame(
    km  = km_north_breaks,
    lat = vapply(km_north_breaks, lat_from_km_north, numeric(1)),
    stringsAsFactors = FALSE
  )
  right_df$y <- web_mercator_xy(ROOST_LON, right_df$lat)$y

  y_pad      <- diff(lims$ylim) * 0.03
  x_pad      <- diff(lims$xlim) * 0.03
  tick_len_y <- diff(lims$ylim) * 0.01
  tick_len_x <- diff(lims$xlim) * 0.01
  y_top      <- lims$ylim[2]
  y_bottom   <- lims$ylim[1]
  x_right    <- lims$xlim[2]
  x_left     <- lims$xlim[1]
  
  span_x     <- diff(lims$xlim)
  span_y     <- diff(lims$ylim)

  p <- p +
    # Top mask (covers leaking tiles/pixels above the grid, spanning into corners)
    ggplot2::annotate(
      "rect",
      xmin = x_left - 3 * span_x, xmax = x_right + 3 * span_x,
      ymin = y_top, ymax = y_top + 3 * span_y,
      fill = "white", color = NA
    ) +
    # Bottom mask (covers leaking tiles/pixels below the grid, spanning into corners)
    ggplot2::annotate(
      "rect",
      xmin = x_left - 3 * span_x, xmax = x_right + 3 * span_x,
      ymin = y_bottom - 3 * span_y, ymax = y_bottom,
      fill = "white", color = NA
    ) +
    # Left mask (covers leaking tiles/pixels to the left of the grid, spanning into corners)
    ggplot2::annotate(
      "rect",
      xmin = x_left - 3 * span_x, xmax = x_left,
      ymin = y_bottom - 3 * span_y, ymax = y_top + 3 * span_y,
      fill = "white", color = NA
    ) +
    # Right mask (covers leaking tiles/pixels to the right of the grid, spanning into corners)
    ggplot2::annotate(
      "rect",
      xmin = x_right, xmax = x_right + 3 * span_x,
      ymin = y_bottom - 3 * span_y, ymax = y_top + 3 * span_y,
      fill = "white", color = NA
    )

  p +
    ggplot2::geom_segment(
      data = top_df,
      ggplot2::aes(x = x, xend = x, y = y_top, yend = y_top + tick_len_y),
      inherit.aes = FALSE, linewidth = 0.3, colour = "#424242"
    ) +
    ggplot2::geom_text(
      data = top_df,
      ggplot2::aes(x = x, y = y_top + y_pad * 0.4, label = sprintf("%.0f", abs(km))),
      inherit.aes = FALSE, size = 2.6, vjust = 0, colour = "#37474f"
    ) +
    ggplot2::annotate(
      "text",
      x = mean(c(lims$xlim[1], lims$xlim[2])),
      y = y_top + y_pad * 1.45,
      label = "km from roost",
      size = 3.1, vjust = 0.5, colour = "#37474f", fontface = "bold"
    ) +
    ggplot2::geom_segment(
      data = right_df,
      ggplot2::aes(x = x_right, xend = x_right + tick_len_x, y = y, yend = y),
      inherit.aes = FALSE, linewidth = 0.3, colour = "#424242"
    ) +
    ggplot2::geom_text(
      data = right_df,
      ggplot2::aes(x = x_right + x_pad * 0.4, y = y, label = sprintf("%.0f", abs(km))),
      inherit.aes = FALSE, size = 2.6, hjust = 0, colour = "#37474f"
    ) +
    ggplot2::annotate(
      "text",
      x = x_right + x_pad * 1.15,
      y = mean(c(lims$ylim[1], lims$ylim[2])),
      label = "km from roost",
      size = 3.1, hjust = 0.5, vjust = 0.5, angle = -90, colour = "#37474f", fontface = "bold"
    )
}

# Build the plot title including detected bearings
plot_title_for_scan <- function(scan_time, streams_df) {
  det_text <- if (nrow(streams_df) == 0) {
    "none"
  } else {
    paste(round(streams_df$bearing_deg), collapse = ", ")
  }
  paste0(
    RADAR_ID, " DBZH | ",
    format(scan_time, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    " | detections: ", det_text, "\u00b0"
  )
}

# Save the raw radar map with optional stream overlays.
#
# Layer order:
#   1. bioRad::map() with param = NULL — basemap tiles only, no radar raster.
#   2. geom_point() — our radar pixels, shape/size/transparency from config.R.
#   3. Stream overlay segments and labels.
#   4. Roost marker.
#
# This avoids the double-pixel problem that occurs when bioRad draws its own
# raster AND we draw geom_point() on top.
save_raw_radar_map <- function(raw_ppi, streams_df, scan_time, out_file) {
  # Clean radar ID suffix from roost name if present (e.g., "North Bethesda KLWX" -> "North Bethesda")
  plot_roost_name <- ROOST_NAME
  if (exists("RADAR_ID") && nzchar(RADAR_ID)) {
    plot_roost_name <- sub(paste0("\\s+", RADAR_ID, "$"), "", plot_roost_name, ignore.case = TRUE)
  }

  # --- Apply RADAR_SUPPRESS_DIST_KM directly onto raw_ppi object before map rendering ---
  if (exists("RADAR_SUPPRESS_DIST_KM") && is.numeric(RADAR_SUPPRESS_DIST_KM) && RADAR_SUPPRESS_DIST_KM > 0) {
    tryCatch({
      ppi_coords <- sp::coordinates(raw_ppi$data)
      sp_wgs <- sp::spTransform(
        sp::SpatialPoints(ppi_coords, proj4string = sp::CRS(sp::proj4string(raw_ppi$data))),
        sp::CRS("+proj=longlat +datum=WGS84")
      )
      r_lat <- if (exists("RADAR_LAT") && !is.null(RADAR_LAT)) RADAR_LAT else raw_ppi$geo$lat
      r_lon <- if (exists("RADAR_LON") && !is.null(RADAR_LON)) RADAR_LON else raw_ppi$geo$lon
      if (!is.null(r_lat) && !is.null(r_lon)) {
        dist_radar <- vectorized_distance_km(r_lat, r_lon, sp::coordinates(sp_wgs)[, 2], sp::coordinates(sp_wgs)[, 1])
        if ("DBZH" %in% names(raw_ppi$data@data)) {
          raw_ppi$data@data$DBZH[!is.na(dist_radar) & dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA_real_
        }
      }
    }, error = function(e) NULL)
  }

  # --- 1. Basemap + bioRad raster (we strip the raster immediately after) ---
  # bioRad::map() requires a valid param name and always draws its own raster.
  # We remove that layer from the ggplot object so only the basemap tiles remain,
  # then draw our own pixel layer below with full shape/size/threshold control.
  zoom_arg <- if (!is.null(MAP_ZOOM)) list(zoomin = MAP_ZOOM) else list()
  p_biorad <- do.call(
    bioRad::map,
    c(
      list(
        x     = raw_ppi,
        map   = MAP_BASEMAP,
        param = "DBZH",
        xlim  = MAP_XLIM,
        ylim  = MAP_YLIM
      ),
      zoom_arg
    )
  )
  # Drop bioRad's raster and internal point layers, leaving the basemap tiles
  # and sf context only. We draw all radar pixels ourselves below.
  p_biorad$layers <- p_biorad$layers[
    !sapply(
      p_biorad$layers,
      function(l) inherits(l$geom, "GeomRaster") || inherits(l$geom, "GeomPoint")
    )
  ]
  p_biorad$scales$scales <- Filter(
    function(s) {
      !any(c("colour", "color", "fill") %in% s$aesthetics)
    },
    p_biorad$scales$scales
  )
  p <- p_biorad +
    ggplot2::labs(
      title    = paste0("Crow Streams to ", plot_roost_name, " ", format(scan_time, "%Y-%m-%d %H:%M", tz = LOCAL_TIMEZONE)),
      subtitle = paste0("(based on DBZ reflectivity data from radar ", RADAR_ID, " ", RADAR_NAME, ".)"),
      x = "Longitude", y = "Latitude"
    )
  p <- apply_map_coord_limits(p)
  p <- p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(face = "bold", size = 13, color = "#263238", margin = ggplot2::margin(b = 6)),
      plot.subtitle     = ggplot2::element_text(size = 10, color = "#546e7a", margin = ggplot2::margin(b = 35)),
      legend.title      = ggplot2::element_text(face = "bold", size = 11),
      legend.position   = "right",
      panel.grid        = ggplot2::element_line(colour = "grey90", linewidth = 0.1),
      axis.text         = ggplot2::element_text(size = 8),
      axis.ticks        = ggplot2::element_line(),
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.border      = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.5),
      legend.key        = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      plot.margin       = ggplot2::margin(t = 50, r = 15, b = 50, l = 10),
      legend.box.spacing = grid::unit(0.4, "in")
    )

  # --- 2. Radar pixels (single layer, full control) ---
  ppi_df <- raw_ppi$data@data
  if ("DBZH" %in% names(ppi_df)) {
    coords <- sp::coordinates(raw_ppi$data)
    sp_wgs <- sp::spTransform(
      sp::SpatialPoints(coords, proj4string = sp::CRS(sp::proj4string(raw_ppi$data))),
      sp::CRS("+proj=longlat +datum=WGS84")
    )
    px_df <- data.frame(
      lon  = sp::coordinates(sp_wgs)[, 1],
      lat  = sp::coordinates(sp_wgs)[, 2],
      dbzh = ppi_df$DBZH
    )
    # Clean up weather/clutter using vol2bird CELL parameter if present
    if ("CELL" %in% names(ppi_df)) {
      # CELL values >= 1 denote weather/clutter cells or their 5km fringe
      px_df$dbzh[!is.na(ppi_df$CELL) & ppi_df$CELL >= 1] <- NA
    }
    
    # Suppress pixels near radar tower in map plots
    if (exists("RADAR_SUPPRESS_DIST_KM") && is.numeric(RADAR_SUPPRESS_DIST_KM) && RADAR_SUPPRESS_DIST_KM > 0) {
      r_lat <- if (exists("RADAR_LAT") && !is.null(RADAR_LAT)) RADAR_LAT else (if (!is.null(raw_ppi$geo$lat)) raw_ppi$geo$lat else NULL)
      r_lon <- if (exists("RADAR_LON") && !is.null(RADAR_LON)) RADAR_LON else (if (!is.null(raw_ppi$geo$lon)) raw_ppi$geo$lon else NULL)
      if (!is.null(r_lat) && !is.null(r_lon)) {
        dist_radar <- vectorized_distance_km(r_lat, r_lon, px_df$lat, px_df$lon)
        px_df$dbzh[!is.na(dist_radar) & dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA
      }
    }
    # Apply transparency threshold (config: MAP_DBZH_TRANSPARENT_BELOW)
    px_df$dbzh[!is.na(px_df$dbzh) & px_df$dbzh < MAP_DBZH_TRANSPARENT_BELOW] <- NA
    px_df <- px_df[!is.na(px_df$dbzh), ]
    
    px_df <- filter_to_map_bounds(px_df)

    if (nrow(px_df) > 0) {
      merc     <- web_mercator_xy(px_df$lon, px_df$lat)
      px_df$x  <- merc$x
      px_df$y  <- merc$y

      if (exists("MAP_USE_TILES") && isTRUE(MAP_USE_TILES)) {
        grid_w <- 200 * (1 / cos(ROOST_LAT * pi / 180))
        p <- p +
          ggplot2::geom_tile(
            data        = px_df,
            ggplot2::aes(x = x, y = y, fill = dbzh),
            width       = grid_w,
            height      = grid_w,
            alpha       = MAP_PIXEL_ALPHA,
            inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_gradientn(
            colors    = radar_colors,
            values    = radar_values,
            limits    = c(MAP_DBZH_MIN, MAP_DBZH_MAX),
            oob       = squish_oob,
            na.value  = NA,
            name      = "dBZ",
            guide     = ggplot2::guide_colorbar(barheight = grid::unit(3.5, "in"))
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data        = px_df,
            ggplot2::aes(x = x, y = y, colour = dbzh),
            inherit.aes = FALSE,
            shape       = MAP_PIXEL_SHAPE,
            size        = MAP_PIXEL_SIZE,
            alpha       = MAP_PIXEL_ALPHA,
            stroke      = 0
          ) +
          ggplot2::scale_colour_gradientn(
            colors    = radar_colors,
            values    = radar_values,
            limits    = c(MAP_DBZH_MIN, MAP_DBZH_MAX),
            oob       = squish_oob,
            na.value  = NA,
            name      = "dBZ",
            guide     = ggplot2::guide_colorbar(barheight = grid::unit(3.5, "in"))
          )
      }
    }
  }

  # --- 3. Stream overlays (segments) ---
  overlay_df <- build_overlay_segments(streams_df)
  if (nrow(overlay_df) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data        = overlay_df,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        colour      = "white",
        linewidth   = 1.2,
        alpha       = 0.4
      ) +
      ggplot2::geom_segment(
        data        = overlay_df,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        colour      = "#ffd700",
        linewidth   = 1.8,
        alpha       = 0.6
      )
  }

  # --- 4. Roost marker ---
  roost_xy <- web_mercator_xy(ROOST_LON, ROOST_LAT)
  p <- p +
    ggplot2::geom_point(
      data        = roost_xy,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21, stroke = 1.2, size = 4.5, fill = "#33a2ff", colour = "white"
    ) +
    ggplot2::geom_text(
      data        = roost_xy,
      ggplot2::aes(x = x, y = y, label = plot_roost_name),
      inherit.aes = FALSE,
      vjust       = 2.0,
      size        = 3.3,
      colour      = "#0077ff",
      fontface    = "bold"
    )

  names(p$layers) <- paste0("layer_", seq_along(p$layers))

  p <- add_km_secondary_axes(p)

  # --- 5. Stream labels on top (ensures the entire box is always shown without border clipping) ---
  if (nrow(overlay_df) > 0) {
    p <- p +
      ggplot2::geom_label(
        data        = overlay_df,
        ggplot2::aes(
          x     = label_x,
          y     = label_y,
          label = paste0(round(bearing_deg), "\u00b0 ", compass, "\n", round(max_extent_km, 1), " km")
        ),
        inherit.aes = FALSE,
        size        = 3.8,
        fill        = grDevices::adjustcolor("#555555ff", alpha.f = 0.40),
        colour      = "white",
        linewidth   = 0.25
      )
  }

  ggplot2::ggsave(
    filename = out_file, plot = p,
    width = MAP_WIDTH_IN, height = MAP_HEIGHT_IN, dpi = MAP_DPI, units = "in"
  )
}

# Save bearing profile plot with detected and selected bearings highlighted
save_bearing_profile <- function(scores, streams_df, scan_time, out_file) {
  profile_df          <- scores
  profile_df$selected <- profile_df$bearing_deg %in% streams_df$bearing_deg

  # Circular rolling average of corridor_eta_sum with a +/- 5 degree window
  profile_df$smoothed_eta_sum <- vapply(profile_df$bearing_deg, function(b) {
    in_window <- angular_distance_deg(profile_df$bearing_deg, b) <= 5
    mean(profile_df$corridor_eta_sum[in_window], na.rm = TRUE)
  }, numeric(1))

  p <- ggplot2::ggplot(profile_df, ggplot2::aes(x = bearing_deg, y = corridor_eta_sum)) +
    # Smoothed trend line (+/- 5 degrees)
    ggplot2::geom_line(
      ggplot2::aes(y = smoothed_eta_sum),
      colour = "#7fcdbb",
      linewidth = 0.75,
      linetype = "dashed"
    ) +
    # Raw line
    ggplot2::geom_line(
      colour = "#2c7fb8",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data   = profile_df[profile_df$stream_detected, , drop = FALSE],
      colour = "#f39c12", size = 1.5
    ) +
    ggplot2::geom_point(
      data   = profile_df[profile_df$selected, , drop = FALSE],
      colour = "#e74c3c", size = 2.5
    ) +
    ggplot2::geom_text(
      data        = profile_df[profile_df$selected, , drop = FALSE],
      ggplot2::aes(label = paste0(round(bearing_deg), "\u00b0 ", compass)),
      inherit.aes = TRUE,
      vjust       = -1.2,
      size        = 3.2,
      colour      = "#e74c3c",
      fontface    = "bold"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 360, by = 50), limits = c(0, 360)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(
      title = paste0(
        RADAR_ID, " bearing profile | ",
        format(scan_time, "%Y-%m-%d %H:%M", tz = LOCAL_TIMEZONE)
      ),
      x = "Bearing (deg)",
      y = "Corridor eta sum"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = out_file, plot = p,
    width = PROFILE_WIDTH_IN, height = PROFILE_HEIGHT_IN,
    dpi = PROFILE_DPI, units = "in"
  )
}
