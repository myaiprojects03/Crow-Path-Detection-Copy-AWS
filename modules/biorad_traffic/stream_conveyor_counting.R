# =============================================================================
# stream_conveyor_counting.R -- Milestone 11 & 14 Stream Counting & Forecasting
# Implements:
#   1. Dynamic Doppler flight velocity validation and resolution towards roost
#   2. Fixed "Finish Line" transect gate counting (1.5 km to 2.5 km from roost)
#   3. "Conveyor-belt" block slicing and multi-scan predictive forecasting
#      extended out to regional catchment ranges (e.g. Poolesville at 30-35 km)
# =============================================================================

suppressPackageStartupMessages({
  library(bioRad)
  library(sp)
  library(geosphere)
})

# Load traffic configuration if available
if (file.exists("modules/biorad_traffic/traffic_config.R")) {
  source("modules/biorad_traffic/traffic_config.R")
}

# ---------------------------------------------------------------------------
# resolve_doppler_velocity_to_roost()
#   Converts raw radial Doppler velocity (VR) into flight speed towards roost
# ---------------------------------------------------------------------------
resolve_doppler_velocity_to_roost <- function(vr_mps, radar_lat, radar_lon, gate_lat, gate_lon, roost_lat, roost_lon, stream_bearing_deg) {
  v_min <- if (exists("TRAFFIC_SPEED_MIN_KMPH")) TRAFFIC_SPEED_MIN_KMPH else 20.0
  v_max <- if (exists("TRAFFIC_SPEED_MAX_KMPH")) TRAFFIC_SPEED_MAX_KMPH else 65.0
  
  if (is.na(vr_mps) || abs(vr_mps) < 0.5) {
    return(list(vr_radial_kmph = NA_real_, v_roost_directed_kmph = NA_real_, valid_bird_speed = FALSE))
  }
  
  # Flight heading towards roost (opposite of corridor bearing from roost)
  flight_heading_deg <- (stream_bearing_deg + 180) %% 360
  
  # Radar beam azimuth from radar tower to gate
  radar_az_deg <- geosphere::bearing(c(radar_lon, radar_lat), c(gate_lon, gate_lat))
  if (radar_az_deg < 0) radar_az_deg <- radar_az_deg + 360
  
  # Angle between flight heading and radar radial line
  alpha_rad <- abs(flight_heading_deg - radar_az_deg) * pi / 180
  cos_alpha <- cos(alpha_rad)
  
  vr_kmph <- abs(vr_mps) * 3.6
  
  # Resolve speed along corridor towards roost
  # If cos_alpha is very small (nearly tangential flight), projection is unstable; clamp to direct vr
  v_roost_kmph <- if (abs(cos_alpha) > 0.25) {
    min(75.0, max(15.0, vr_kmph / abs(cos_alpha)))
  } else {
    vr_kmph
  }
  
  # Biological validation check (crows typically cruise between 20 and 65 km/h)
  valid_bird <- v_roost_kmph >= v_min && v_roost_kmph <= v_max
  
  list(
    vr_radial_kmph = round(vr_kmph, 1),
    v_roost_directed_kmph = round(v_roost_kmph, 1),
    valid_bird_speed = valid_bird
  )
}

# ---------------------------------------------------------------------------
# compute_stream_conveyor_flow()
#   Slices corridor into conveyor blocks and computes current and forecasted counts
#   out to extended regional ranges (e.g. 35 km)
# ---------------------------------------------------------------------------
compute_stream_conveyor_flow <- function(
  pvol, 
  stream_bearing_deg, 
  finish_line_dist_km   = if (exists("TRAFFIC_FINISH_LINE_DIST_KM")) TRAFFIC_FINISH_LINE_DIST_KM else 2.0, 
  corridor_width_km     = if (exists("TRAFFIC_CORRIDOR_WIDTH_KM")) TRAFFIC_CORRIDOR_WIDTH_KM else 1.5, 
  max_forecast_range_km = if (exists("TRAFFIC_MAX_FORECAST_RANGE_KM")) TRAFFIC_MAX_FORECAST_RANGE_KM else 35.0, 
  scan_interval_mins    = if (exists("TRAFFIC_SCAN_INTERVAL_MINS")) TRAFFIC_SCAN_INTERVAL_MINS else 6.0,
  rcs_cm2               = if (exists("TRAFFIC_CROW_RCS_CM2")) TRAFFIC_CROW_RCS_CM2 else 100.0,
  layer_thickness_km    = if (exists("TRAFFIC_LAYER_THICKNESS_KM")) TRAFFIC_LAYER_THICKNESS_KM else 0.150
) {
  radar_lat <- pvol$geo$lat
  radar_lon <- pvol$geo$lon
  
  # 1. Extract base tilt (0.3 deg for TDWR, 0.5 deg for WSR-88D)
  base_scan <- pvol$scans[[1]]
  # Use high-res grid and range covering up to max_forecast_range_km + buffer
  ppi_range_max <- max(40000, ceiling((max_forecast_range_km + 5) * 1000))
  ppi <- bioRad::project_as_ppi(base_scan, grid_size = 150, range_max = ppi_range_max)
  
  ppi_df  <- ppi$data@data
  coords  <- sp::coordinates(ppi$data)
  sp_proj <- sp::SpatialPoints(coords, proj4string = sp::CRS(sp::proj4string(ppi$data)))
  sp_wgs  <- sp::spTransform(sp_proj, sp::CRS("+proj=longlat +datum=WGS84"))
  pts_wgs <- sp::coordinates(sp_wgs)
  
  px_df <- data.frame(
    lon  = pts_wgs[, 1],
    lat  = pts_wgs[, 2],
    dbzh = ppi_df$DBZH,
    vr   = if ("VRADH" %in% names(ppi_df)) ppi_df$VRADH else (if ("VRAD" %in% names(ppi_df)) ppi_df$VRAD else NA_real_)
  )
  
  # Filter out noise/clutter below minimum threshold
  min_dbz <- if (exists("TRAFFIC_MIN_DBZH")) TRAFFIC_MIN_DBZH else 5.0
  px_df$dbzh[is.na(px_df$dbzh) | px_df$dbzh < min_dbz] <- NA_real_
  
  # Geometry relative to roost
  dists_to_roost <- geosphere::distGeo(c(ROOST_LON, ROOST_LAT), as.matrix(px_df[, c("lon", "lat")])) / 1000
  bearings_from_roost <- geosphere::bearing(c(ROOST_LON, ROOST_LAT), as.matrix(px_df[, c("lon", "lat")]))
  bearings_from_roost[bearings_from_roost < 0] <- bearings_from_roost[bearings_from_roost < 0] + 360
  
  # Perpendicular and along-track distance to corridor centerline
  delta_az <- abs(bearings_from_roost - stream_bearing_deg)
  delta_az[delta_az > 180] <- 360 - delta_az[delta_az > 180]
  cross_track_dist_km <- dists_to_roost * sin(delta_az * pi / 180)
  along_track_dist_km <- dists_to_roost * cos(delta_az * pi / 180)
  
  px_df$dist_along <- along_track_dist_km
  px_df$dist_cross <- cross_track_dist_km
  
  # Filter pixels inside corridor
  corridor_mask <- abs(px_df$dist_cross) <= (corridor_width_km / 2) &
                   px_df$dist_along >= finish_line_dist_km &
                   px_df$dist_along <= max_forecast_range_km &
                   !is.na(px_df$dbzh)
  
  corridor_pixels <- px_df[corridor_mask, ]
  
  # 2. Compute average Doppler flight speed in the stream
  sample_vr <- corridor_pixels$vr[!is.na(corridor_pixels$vr) & corridor_pixels$dbzh >= 10.0]
  default_speed <- if (exists("TRAFFIC_SPEED_DEFAULT_KMPH")) TRAFFIC_SPEED_DEFAULT_KMPH else 38.0
  mean_vr_mps <- if (length(sample_vr) > 0) mean(abs(sample_vr)) else (default_speed / 3.6)
  
  # Center of stream gate coordinates for velocity projection
  gate_center <- geosphere::destPoint(c(ROOST_LON, ROOST_LAT), b = stream_bearing_deg, d = finish_line_dist_km * 1000)
  vel_info <- resolve_doppler_velocity_to_roost(
    mean_vr_mps, radar_lat, radar_lon, gate_center[2], gate_center[1], ROOST_LAT, ROOST_LON, stream_bearing_deg
  )
  
  v_roost_kmph <- if (vel_info$valid_bird_speed) vel_info$v_roost_directed_kmph else default_speed
  
  # Distance covered in one scan interval (dt = 6 minutes)
  # d_travel = (v_kmph * (dt / 60))
  d_travel_km <- v_roost_kmph * (scan_interval_mins / 60)
  
  # 3. Slice into Conveyor Blocks starting from Finish Line
  # Extended reach: no arbitrary 5-block limit. Dynamically slices up to max_forecast_range_km
  n_blocks <- max(1, floor((max_forecast_range_km - finish_line_dist_km) / d_travel_km))
  block_counts <- numeric(n_blocks)
  block_ranges <- character(n_blocks)
  
  # S-band or C-band conversion factor
  is_c_band <- startsWith(pvol$radar, "T") || (exists("RADAR_ID") && startsWith(RADAR_ID, "T"))
  lambda_cm <- if (is_c_band) {
    if (exists("TRAFFIC_LAMBDA_C_BAND_CM")) TRAFFIC_LAMBDA_C_BAND_CM else 5.35
  } else {
    if (exists("TRAFFIC_LAMBDA_S_BAND_CM")) TRAFFIC_LAMBDA_S_BAND_CM else 10.70
  }
  
  dielectric_k2 <- if (exists("TRAFFIC_DIELECTRIC_K2")) TRAFFIC_DIELECTRIC_K2 else 0.93
  eta_multiplier <- (1000 * (pi^5) * dielectric_k2) / (lambda_cm^4)
  
  for (b in 1:n_blocks) {
    b_start <- finish_line_dist_km + (b - 1) * d_travel_km
    b_end   <- b_start + d_travel_km
    block_ranges[b] <- sprintf("%.1f - %.1f km", b_start, b_end)
    
    in_b <- corridor_pixels$dist_along >= b_start & corridor_pixels$dist_along < b_end
    b_dbzh <- corridor_pixels$dbzh[in_b]
    
    if (length(b_dbzh) > 0) {
      eta_b <- eta_multiplier * (10^(b_dbzh / 10))
      dens_vol <- eta_b / rcs_cm2
      vid_b <- dens_vol * layer_thickness_km # crows / km^2
      
      # Block Area = d_travel_km * corridor_width_km
      block_area_km2 <- d_travel_km * corridor_width_km
      # Total crows in this conveyor block
      block_counts[b] <- mean(vid_b) * block_area_km2
    } else {
      block_counts[b] <- 0
    }
  }
  
  # Dynamic arrival window labels
  arrival_windows <- vapply(0:(n_blocks - 1), function(i) {
    if (i == 0) {
      sprintf("Current Scan (0 - %.0f min)", scan_interval_mins)
    } else {
      sprintf("Scan + %d (+%.0f to %.0f min)", i, i * scan_interval_mins, (i + 1) * scan_interval_mins)
    }
  }, character(1))
  
  # Build structured forecast summary row
  summary_list <- list(
    Scan_Time               = as.character(pvol$datetime),
    Stream_Bearing_deg      = stream_bearing_deg,
    Finish_Line_Dist_km     = finish_line_dist_km,
    VR_radial_kmph          = vel_info$vr_radial_kmph,
    V_roost_directed_kmph   = v_roost_kmph,
    Conveyor_Step_km        = round(d_travel_km, 2),
    Current_Scan_Crossing   = round(block_counts[1], 0)
  )
  
  for (b_idx in 2:n_blocks) {
    col_name <- sprintf("Forecast_Scan_plus_%d", b_idx - 1)
    summary_list[[col_name]] <- round(block_counts[b_idx], 0)
  }
  summary_list$Anticipated_Stream_Total <- round(sum(block_counts), 0)
  
  forecast_summary <- as.data.frame(summary_list, stringsAsFactors = FALSE)
  
  block_detail <- data.frame(
    Block_Index      = 0:(n_blocks - 1),
    Arrival_Window   = arrival_windows,
    Range_From_Roost = block_ranges,
    Estimated_Crows  = round(block_counts, 0),
    stringsAsFactors = FALSE
  )
  
  list(
    forecast_summary = forecast_summary,
    block_detail     = block_detail
  )
}

