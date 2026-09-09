# =============================================================================
# utils.R — Pure helper functions (geometry, circular stats, naming, colours)
# =============================================================================

# Convert bearing in degrees to compass direction string
bearing_to_compass <- function(bearing_deg) {
  dirs <- c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
  )
  dirs[((round(bearing_deg) + 11L) %/% 22L %% 16L) + 1L]
}

# Shortest angular distance between two bearings (0-180)
angular_distance_deg <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

# Smallest arc on the circle that contains all bearings (0-360)
group_angular_span_deg <- function(bearings) {
  b <- sort(unique(as.numeric(bearings) %% 360))
  if (length(b) <= 1) return(0)
  gaps <- diff(c(b, b[1] + 360))
  360 - max(gaps)
}

# Signed angular difference wrapped to (-180, 180]
wrapped_delta_deg <- function(a, b) {
  ((a - b + 180) %% 360) - 180
}

# Weighted circular mean of angles in degrees
circular_weighted_mean_deg <- function(angles_deg, weights) {
  if (length(angles_deg) == 0) return(NA_real_)
  theta <- angles_deg * pi / 180
  s <- sum(weights * sin(theta), na.rm = TRUE)
  c <- sum(weights * cos(theta), na.rm = TRUE)
  if (isTRUE(all.equal(s, 0)) && isTRUE(all.equal(c, 0))) {
    return(mean(angles_deg, na.rm = TRUE))
  }
  (atan2(s, c) * 180 / pi + 360) %% 360
}

# Details of the longest run of integers, allowing for small gaps
# Returns a list with: length (total bins), furthest (last bin index), start (first bin index)
get_longest_run_details <- function(values, max_gap = 1L) {
  if (length(values) == 0) return(list(length = 0L, furthest = 0L, start = 0L))
  values <- sort(unique(as.integer(values)))
  if (length(values) == 1) return(list(length = 1L, furthest = values[1], start = values[1]))
  
  # A gap is any jump > (max_gap + 1)
  # e.g. if max_gap=1, then diffs of 1 or 2 are "consecutive"
  diffs <- diff(values)
  is_gap <- diffs > (max_gap + 1L)
  runs  <- cumsum(c(1L, ifelse(is_gap, 1L, 0L)))
  
  # Find all runs
  run_lengths <- tabulate(runs)
  
  # We want the 'first significant run' (the one closest to the roost)
  # that meets a minimum bin count (e.g. 3 bins)
  significant_runs <- which(run_lengths >= 3L)
  if (length(significant_runs) == 0) {
    # Fallback to the absolute longest if none are 'significant'
    best_run_id <- which.max(run_lengths)
  } else {
    best_run_id <- significant_runs[1]
  }
  
  # Get the indices in the selected run
  best_run_vals <- values[runs == best_run_id]
  
  list(
    length   = as.integer(run_lengths[best_run_id]),
    furthest = as.integer(max(best_run_vals)),
    start    = as.integer(min(best_run_vals))
  )
}

# Furthest distance-bin reached along a bearing corridor, bridging small gaps and
# any continuation runs beyond the first near-roost segment (used for line length).
get_trail_extent_furthest_bin <- function(values, max_gap = 1L) {
  if (length(values) == 0) return(0L)
  values <- sort(unique(as.integer(values)))
  if (length(values) == 1) return(values[1])

  diffs  <- diff(values)
  is_gap <- diffs > (max_gap + 1L)
  runs   <- cumsum(c(1L, ifelse(is_gap, 1L, 0L)))
  run_lengths <- tabulate(runs)

  significant_runs <- which(run_lengths >= 3L)
  best_run_id <- if (length(significant_runs) == 0) {
    which.max(run_lengths)
  } else {
    significant_runs[1]
  }

  furthest <- max(values[runs == best_run_id])
  if (best_run_id >= max(runs)) return(furthest)

  for (rid in (best_run_id + 1L):max(runs)) {
    run_vals <- values[runs == rid]
    if (min(run_vals) - furthest <= max_gap + 1L) {
      furthest <- max(run_vals)
    } else {
      break
    }
  }
  furthest
}

# Max scored extent among bearings near a display bearing (curved-trail pooling).
neighbor_max_extent_km <- function(scores, bearing_deg,
                                   half_width_deg = EXTENT_NEIGHBOR_DEG) {
  keep <- vapply(scores$bearing_deg, function(b) {
    angular_distance_deg(b, bearing_deg) <= half_width_deg
  }, logical(1))
  subset <- scores[keep & scores$corridor_valid_pixels > 0L, , drop = FALSE]
  if (nrow(subset) == 0) return(0)
  max(subset$max_extent_km)
}

# Haversine distance in km between roost and pixel coordinates
vectorized_distance_km <- function(roost_lat, roost_lon, px_lat, px_lon) {
  earth_km <- 6371
  dlat <- (px_lat - roost_lat) * pi / 180
  dlon <- (px_lon - roost_lon) * pi / 180
  lat1 <- roost_lat * pi / 180
  lat2 <- px_lat    * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  2 * earth_km * asin(pmin(1, sqrt(a)))
}

# Forward azimuth from roost to each pixel (degrees, 0 = N clockwise)
vectorized_bearing_deg <- function(roost_lat, roost_lon, px_lat, px_lon) {
  lat1 <- roost_lat * pi / 180
  lat2 <- px_lat    * pi / 180
  dlon <- (px_lon - roost_lon) * pi / 180
  x <- sin(dlon) * cos(lat2)
  y <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  (atan2(x, y) * 180 / pi + 360) %% 360
}

# Destination point given start, bearing, and distance
destination_point <- function(lat, lon, bearing_deg, dist_km) {
  geosphere::destPoint(p = c(lon, lat), b = bearing_deg, d = dist_km * 1000)
}

# Convert lon/lat to Web Mercator x/y in metres
web_mercator_xy <- function(lon, lat) {
  radius <- 6378137
  lat    <- pmax(pmin(lat, 85.05112878), -85.05112878)
  data.frame(
    x = radius * lon * pi / 180,
    y = radius * log(tan(pi / 4 + (lat * pi / 360)))
  )
}

# Web Mercator metres <-> WGS84 (for map axis labelling)
merc_x_to_lon <- function(x) as.numeric(x) * 180 / (6378137 * pi)

merc_y_to_lat <- function(y) {
  r <- 6378137
  (2 * atan(exp(as.numeric(y) / r)) - pi / 2) * 180 / pi
}

# Signed km offsets from the roost along cardinal directions (0 at roost)
roost_km_east_from_lon <- function(lon) {
  lon <- as.numeric(lon)
  geosphere::distHaversine(
    p1 = cbind(ROOST_LON, ROOST_LAT),
    p2 = cbind(lon, ROOST_LAT)
  ) / 1000 * sign(lon - ROOST_LON)
}

roost_km_north_from_lat <- function(lat) {
  lat <- as.numeric(lat)
  geosphere::distHaversine(
    p1 = cbind(ROOST_LON, ROOST_LAT),
    p2 = cbind(ROOST_LON, lat)
  ) / 1000 * sign(lat - ROOST_LAT)
}

roost_km_east_from_merc_x <- function(x) roost_km_east_from_lon(merc_x_to_lon(x))

roost_km_north_from_merc_y <- function(y) roost_km_north_from_lat(merc_y_to_lat(y))

# Lon/lat at a signed km offset from the roost along E/W or N/S
lon_from_km_east <- function(km) {
  if (abs(km) < 1e-9) return(ROOST_LON)
  geosphere::destPoint(
    c(ROOST_LON, ROOST_LAT),
    b = if (km >= 0) 90 else 270,
    d = abs(km) * 1000
  )[1]
}

lat_from_km_north <- function(km) {
  if (abs(km) < 1e-9) return(ROOST_LAT)
  geosphere::destPoint(
    c(ROOST_LON, ROOST_LAT),
    b = if (km >= 0) 0 else 180,
    d = abs(km) * 1000
  )[2]
}

# Mercator limits for the configured map bounding box
map_mercator_limits <- function() {
  xlim <- range(web_mercator_xy(MAP_XLIM, rep(MAP_YLIM[1], 2))$x)
  ylim <- range(web_mercator_xy(rep(MAP_XLIM[1], 2), MAP_YLIM)$y)
  list(xlim = xlim, ylim = ylim)
}

# Clamp values to a range (used as ggplot2 oob handler)
squish_oob <- function(x, range = c(0, 1), ...) {
  pmax(pmin(x, range[2]), range[1])
}

# Short identifier for the whole run (used in output folder names)
run_stub <- function() {
  paste0(
    RADAR_ID, "_",
    format(DATE_START, "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "_to_",
    format(DATE_END, "%Y%m%dT%H%M%SZ", tz = "UTC")
  )
}

# Short identifier for a single scan (used in sub-folder names)
scan_stub <- function(scan_time) {
  format(scan_time, "%Y%m%d_%H%M", tz = "UTC")
}

# Call apply_mistnet from whichever package exposes it
run_mistnet <- function(file_path) {
  if ("apply_mistnet" %in% getNamespaceExports("bioRad")) {
    return(bioRad::apply_mistnet(file_path))
  }
  if ("apply_mistnet" %in% getNamespaceExports("vol2birdR")) {
    return(vol2birdR::apply_mistnet(file_path))
  }
  stop("apply_mistnet() was not found in bioRad or vol2birdR.")
}

# Check whether MistNet weights are installed
mistnet_ready <- function() {
  if ("mistnet_exists" %in% getNamespaceExports("vol2birdR")) {
    return(vol2birdR::mistnet_exists())
  }
  TRUE
}

# =============================================================================
# Radar colour scale (RadarScope-like progression)
# =============================================================================
# Breaks and colours follow a RadarScope-style flow:
# gray/blue -> green -> yellow/orange -> red -> magenta/purple.
# Display range is controlled by MAP_DBZH_MIN / MAP_DBZH_MAX in config.R.

radar_breaks <- c(-20, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
radar_colors <- c(
  "#6f6f6f", "#b8b8b8", # Low/noise (dark->light gray)
  "#8ec9ff", "#4aa4ff", # Low echoes (light->medium blue)
  "#00d8d8", "#00b85e", # Transition into green
  "#00a000", "#63d000", # Mid greens
  "#fff400", "#ffbe00", # Yellow/orange
  "#ff7a00", "#ff2a00", # Orange/red
  "#c00000", "#c43cff", # Deep red -> magenta
  "#7a2bd6"             # Highest (purple)
)
radar_values <- (radar_breaks - min(radar_breaks)) /
  (max(radar_breaks) - min(radar_breaks))

radar_fill_scale <- function() {
  ggplot2::scale_fill_gradientn(
    colors   = radar_colors,
    values   = radar_values,
    breaks   = radar_breaks,
    limits   = c(MAP_DBZH_MIN, MAP_DBZH_MAX),
    oob      = squish_oob,
    na.value = NA,          # transparent — basemap shows through masked pixels
    guide    = ggplot2::guide_colorbar(
      title     = "dBZ",
      barheight = grid::unit(3.5, "in")
    )
  )
}

# =============================================================================
# Automated Storage Cleanup Helpers
# =============================================================================

# Purge old subdirectories/files in output directory, preserving specified Excel files
clear_output_directory <- function(output_dir = if (exists("OUTPUT_DIR")) OUTPUT_DIR else "output",
                                   preserve_files = if (exists("PRESERVE_OUTPUT_FILES")) PRESERVE_OUTPUT_FILES else c("batch_results.xlsx")) {
  if (!dir.exists(output_dir)) return(invisible(NULL))

  items <- list.files(output_dir, full.names = TRUE)
  if (length(items) == 0) return(invisible(NULL))

  message("  [Storage Cleanup] Clearing output directory: ", output_dir)
  for (item in items) {
    item_name <- basename(item)
    # Exclude files listed in preserve_files or matching preserved patterns
    is_preserved <- item_name %in% preserve_files ||
      any(vapply(preserve_files, function(p) grepl(p, item_name, fixed = TRUE), logical(1)))
    
    if (is_preserved) {
      message("    \u2192 Preserved: ", item_name)
      next
    }

    tryCatch({
      unlink(item, recursive = TRUE, force = TRUE)
      message("    \u2192 Removed old item: ", item_name)
    }, error = function(e) {
      warning("    \u2192 Could not remove ", item_name, ": ", e$message)
    })
  }
}

# Purge downloaded raw pvol files from data directory
clear_pvol_directory <- function(data_dir = if (exists("DATA_DIR")) DATA_DIR else "data/data_pvol") {
  if (!dir.exists(data_dir)) return(invisible(NULL))

  pvol_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
  if (length(pvol_files) > 0) {
    message("  [Storage Cleanup] Purging ", length(pvol_files), " raw radar volume file(s) from ", data_dir, "...")
    for (f in pvol_files) {
      tryCatch(unlink(f, force = TRUE), error = function(e) NULL)
    }

    # Clean empty subdirectories in data_dir
    subdirs <- list.dirs(data_dir, recursive = TRUE, full.names = TRUE)
    norm_data_dir <- normalizePath(data_dir, winslash = "/", mustWork = FALSE)
    subdirs <- setdiff(normalizePath(subdirs, winslash = "/", mustWork = FALSE), norm_data_dir)
    
    # Process deeper subdirectories first
    subdirs <- subdirs[order(-nchar(subdirs))]
    for (d in subdirs) {
      if (dir.exists(d) && length(list.files(d, recursive = TRUE)) == 0) {
        unlink(d, recursive = TRUE, force = TRUE)
      }
    }
  }
}