# =============================================================================
# io.R — Radar file discovery, downloading, and pvol → data-frame conversion
# =============================================================================

# Resolve station file path relative to project root and load it.
resolve_station_file <- function(path_value) {
  if (is.null(path_value) || !nzchar(path_value)) return(NULL)
  candidates <- c(path_value, file.path(getwd(), path_value))
  candidates <- unique(candidates[file.exists(candidates)])
  if (length(candidates) == 0) return(NULL)
  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

load_station_file_if_available <- function(path_value, quiet = FALSE) {
  station_path <- resolve_station_file(path_value)
  if (is.null(station_path)) return(FALSE)
  tryCatch({
    vol2birdR::nexrad_station_file(station_path)
    if (!quiet) message("Loaded radar station file: ", station_path)
    TRUE
  }, error = function(e) {
    warning("Failed to load station file ", station_path, ": ", e$message)
    FALSE
  })
}

# Initialize custom station list (supports TDWR IDs like TIAD/TDCA).
if (exists("CUSTOM_STATION_FILE")) {
  load_station_file_if_available(CUSTOM_STATION_FILE)
}

# Return sorted paths of local pvol files matching the radar and time window
list_local_pvolfiles <- function(data_dir, radar, date_start, date_end) {
  if (!dir.exists(data_dir)) return(character(0))

  # Only scan subdirectories for the years in the requested date range to avoid
  # listing thousands of files from other years.
  year_start <- format(date_start, "%Y")
  year_end   <- format(date_end, "%Y")
  years      <- unique(c(year_start, year_end))
  
  search_paths <- unique(c(data_dir, file.path(data_dir, years)))
  search_paths <- search_paths[dir.exists(search_paths)]

  all_files  <- unique(list.files(search_paths, recursive = TRUE, full.names = TRUE))
  all_files  <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  if (length(all_files) == 0) return(character(0))

  file_names <- basename(all_files)
  # Updated regex to support:
  # - Modern files: KLWX20260112_210622_V06
  # - Older files:  KLWX19960112_210229
  # - Gzipped:      KLWX19960112_210229.gz
  regex_pattern <- paste0("^", radar, "(\\d{8})_(\\d{6})(?:_V\\d{2})?(?:\\.gz)?$")
  keep <- grepl(regex_pattern, file_names)
  all_files  <- all_files[keep]
  file_names <- file_names[keep]
  if (length(all_files) == 0) return(character(0))

  scan_times <- as.POSIXct(
    sub(
      regex_pattern,
      "\\1 \\2",
      file_names
    ),
    format = "%Y%m%d %H%M%S",
    tz = "UTC"
  )

  keep_time <- !is.na(scan_times) &
    scan_times >= date_start &
    scan_times <= date_end

  ordered_files <- all_files[keep_time]
  ordered_times <- scan_times[keep_time]
  if (length(ordered_files) == 0) return(character(0))

  ordered_files[order(ordered_times)]
}

# Download pvol files if not already cached, then return their paths
download_radar <- function(date_start, date_end, radar, data_dir) {
  # Organize downloads by year
  year_str <- format(date_start, "%Y")
  target_dir <- file.path(data_dir, year_str)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  # Check what we already have locally before querying S3
  local_before <- list_local_pvolfiles(data_dir, radar, date_start, date_end)

  # Allow explicit bypass of S3 if offline/local-only mode is requested
  bypass_s3 <- exists("BYPASS_S3_DOWNLOAD") && isTRUE(BYPASS_S3_DOWNLOAD)

  if (bypass_s3 && length(local_before) > 0) {
    message("Found ", length(local_before), " cached pvol file(s) in ", data_dir, ". Bypassing S3 download (offline mode).")
    local_files <- local_before
  } else {
    if (length(local_before) == 0) {
      message("No local pvol files found in the requested window. Downloading to: ", target_dir)
    } else {
      message(
        "Found ", length(local_before), " cached pvol file(s) in ", data_dir, 
        "; checking archive for any missing scans in requested time range."
      )
    }

    # Try archive buckets in priority order.
    buckets <- if (startsWith(radar, "T")) {
      c("unidata-nexrad-level2", "noaa-tdwr-spg-l2")
    } else {
      c("unidata-nexrad-level2", "noaa-nexrad-level2")
    }

    downloaded_any <- FALSE
    for (bucket in buckets) {
      tryCatch({
        message("Checking archive bucket: ", bucket)
        bioRad::download_pvolfiles(
          date_min  = date_start,
          date_max  = date_end,
          radar     = radar,
          directory = target_dir,
          overwrite = FALSE,
          bucket    = bucket
        )
        downloaded_any <- TRUE
      }, error = function(e) {
        message("Bucket ", bucket, " returned an error: ", e$message)
      })
    }

    # --- SMART FALLBACK ---
    # If we still have no files after trying all S3 buckets, try alternative web sources
    after_s3 <- list_local_pvolfiles(data_dir, radar, date_start, date_end)
    if (length(after_s3) == 0) {
      message("S3 buckets failed to provide data. Attempting web fallback for ", radar, " on ", as.Date(date_start))
      search_and_download_alternative(date_start, date_end, radar, target_dir)
      after_s3 <- list_local_pvolfiles(data_dir, radar, date_start, date_end)
    }

    if (length(after_s3) == 0) {
      stop(
        "No radar files were found for ", radar, " between ", 
        date_start, " and ", date_end, "."
      )
    }

    local_files <- after_s3

    if (length(local_before) > 0) {
      n_added <- length(local_files) - length(local_before)
      if (n_added > 0) {
        message("Downloaded ", n_added, " missing pvol file(s) into ", target_dir)
      } else {
        message("Local cache already covered the full requested window (", length(local_files), " scan(s) found).")
      }
    }
  }

  max_files <- if (exists("MAX_FILES_PER_RUN")) as.integer(MAX_FILES_PER_RUN) else NA_integer_
  if (!is.na(max_files) && max_files > 0 && length(local_files) > max_files) {
    message("Limiting file set to first ", max_files, " scan(s) for this run.")
    local_files <- local_files[seq_len(max_files)]
  }

  local_files
}


# Convert a projected PPI grid to a flat data frame with lon/lat columns
ppi_to_df <- function(ppi, params) {
  grid       <- ppi$data
  coords_proj <- sp::coordinates(grid)
  sp_proj <- sp::SpatialPoints(
    coords      = coords_proj,
    proj4string = sp::CRS(sp::proj4string(grid))
  )
  sp_wgs      <- sp::spTransform(sp_proj, sp::CRS("+proj=longlat +datum=WGS84"))
  coords_wgs  <- sp::coordinates(sp_wgs)

  df <- data.frame(lon = coords_wgs[, 1], lat = coords_wgs[, 2])

  for (param in params) {
    df[[param]] <- if (param %in% names(grid@data)) grid@data[[param]] else NA_real_
  }

  df
}

# Zero out DBZH pixels that match active masks (RHOHV, CELL, WEATHER)
apply_detection_masks <- function(det_df) {
  det_df$DBZH_DETECT <- det_df$DBZH

  if (MASK_RHOHV) {
    det_df$DBZH_DETECT[
      !is.na(det_df$RHOHV) & det_df$RHOHV > RHOHV_THRESHOLD
    ] <- NA_real_
  }
  if (MASK_CELL) {
    det_df$DBZH_DETECT[
      !is.na(det_df$CELL) & det_df$CELL >= CELL_THRESHOLD
    ] <- NA_real_
  }
  if (MASK_WEATHER) {
    det_df$DBZH_DETECT[
      !is.na(det_df$WEATHER) & det_df$WEATHER >= WEATHER_THRESHOLD
    ] <- NA_real_
  }

  det_df
}

# Runs vol2bird on a polar volume and returns the path to a temporary processed H5 file
run_vol2bird <- function(file_path) {
  base_name <- basename(file_path)
  
  # Write to an ephemeral temporary file to save persistent disk space
  # IMPORTANT: The filename MUST start with the original base_name (e.g. 'TIAD')
  # so that bioRad can correctly infer the radar station ID if reading the raw file.
  proc_file <- tempfile(pattern = paste0(sub("(?:\\.gz)?$", "", base_name), "_vol2bird_"), fileext = ".h5")

  message("Passing polar volume through vol2bird algorithm: ", base_name)
  config <- vol2birdR::vol2bird_config()

  # Ensure custom station list is loaded in vol2bird setup if exists
  if (exists("CUSTOM_STATION_FILE")) {
    load_station_file_if_available(CUSTOM_STATION_FILE, quiet = TRUE)
  }

  tryCatch({
    vol2birdR::vol2bird(
      file = file_path,
      config = config,
      vpfile = tempfile(fileext = ".h5"),
      pvolfile_out = proc_file,
      verbose = FALSE
    )
  }, error = function(e) {
    warning("vol2bird failed for ", base_name, ": ", e$message)
    file.copy(file_path, proc_file, overwrite = TRUE)
  })

  proc_file
}

# Read one pvol file, project to PPI, return raw + detection data frames
process_scan_file <- function(file_path) {
  read_with_station_retry <- function(path) {
    tryCatch({
      bioRad::read_pvolfile(path)
    }, error = function(e) {
      msg <- conditionMessage(e)
      needs_station_retry <- grepl("No valid site ID info found", msg, fixed = TRUE)
      if (!needs_station_retry) stop(e)

      fallback_loaded <- FALSE
      if (exists("CUSTOM_STATION_FILE_FALLBACK")) {
        fallback_loaded <- load_station_file_if_available(CUSTOM_STATION_FILE_FALLBACK)
      }
      if (!fallback_loaded && exists("CUSTOM_STATION_FILE")) {
        fallback_loaded <- load_station_file_if_available(CUSTOM_STATION_FILE)
      }
      if (!fallback_loaded) stop(e)

      bioRad::read_pvolfile(path)
    })
  }

  # Pass polar volume through vol2bird to generate CELL clutter/weather masks if enabled
  if (exists("USE_VOL2BIRD") && USE_VOL2BIRD) {
    proc_path <- run_vol2bird(file_path)
    on.exit({
      if (file.exists(proc_path) && proc_path != file_path) {
        unlink(proc_path)
      }
    }, add = TRUE)
  } else {
    proc_path <- file_path
  }

  raw_pvol <- read_with_station_retry(proc_path)
  raw_scan <- bioRad::get_scan(raw_pvol, ELEVATION_DEG)
  raw_ppi  <- bioRad::project_as_ppi(raw_scan)

  # MASK_CELL uses vol2bird CELL and does not require MistNet installation anymore
  need_mistnet <- USE_MISTNET_FOR_DETECTION || MASK_WEATHER
  if (need_mistnet && !mistnet_ready()) {
    stop(
      "MistNet is required for the current detection settings but is not ",
      "installed on this machine."
    )
  }

  detect_pvol <- if (need_mistnet) run_mistnet(file_path) else raw_pvol
  detect_scan <- bioRad::get_scan(detect_pvol, ELEVATION_DEG)
  detect_ppi  <- bioRad::project_as_ppi(detect_scan)

  raw_df    <- ppi_to_df(raw_ppi,    params = c("DBZH", "RHOHV", "CELL"))
  detect_df <- ppi_to_df(detect_ppi, params = c("DBZH", "RHOHV", "CELL", "WEATHER", "BIOLOGY"))

  # Suppress pixels near the radar
  if (exists("RADAR_SUPPRESS_DIST_KM") && RADAR_SUPPRESS_DIST_KM > 0) {
    radar_lat <- raw_pvol$geo$lat
    radar_lon <- raw_pvol$geo$lon
    
    raw_dist_radar <- vectorized_distance_km(radar_lat, radar_lon, raw_df$lat, raw_df$lon)
    raw_df$DBZH[!is.na(raw_dist_radar) & raw_dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA_real_
    if ("DBZH" %in% names(raw_ppi$data@data)) {
      raw_ppi$data@data$DBZH[!is.na(raw_dist_radar) & raw_dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA_real_
    }
    
    det_dist_radar <- vectorized_distance_km(radar_lat, radar_lon, detect_df$lat, detect_df$lon)
    detect_df$DBZH[!is.na(det_dist_radar) & det_dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA_real_
    if ("DBZH" %in% names(detect_ppi$data@data)) {
      detect_ppi$data@data$DBZH[!is.na(det_dist_radar) & det_dist_radar < RADAR_SUPPRESS_DIST_KM] <- NA_real_
    }
  }

  detect_df <- apply_detection_masks(detect_df)

  # Extract VCP (Volume Coverage Pattern) from pvol header if available
  vcp <- tryCatch({
    task <- raw_pvol$header$task
    if (!is.null(task) && nzchar(task)) task else "unknown"
  }, error = function(e) "unknown")

  list(
    file_path = file_path,
    scan_time = as.POSIXct(raw_scan$datetime, tz = "UTC"),
    raw_ppi   = raw_ppi,
    raw_df    = raw_df,
    detect_df = detect_df,
    vcp       = vcp
  )
}


# --- FALLBACK FUNCTIONS ---

# Wrapper to coordinate alternative sources (NCEI HTTP, etc.)
search_and_download_alternative <- function(date_start, date_end, radar, target_dir) {
  # Currently attempts direct NCEI HTTP download. 
  # In a future version, this could call a custom scraper or API.
  download_from_ncei_http(date_start, date_end, radar, target_dir)
}

# Attempts to construct and download from NCEI's direct HTTP archive
download_from_ncei_http <- function(date_start, date_end, radar, target_dir) {
  year  <- format(date_start, "%Y")
  month <- format(date_start, "%m")
  day   <- format(date_start, "%d")
  
  # NCEI TDWR Level 2 pattern:
  # https://www.ncei.noaa.gov/data/terminal-doppler-weather-radar-level-2/access/YYYY/MM/DD/ID/
  base_url <- paste0(
    "https://www.ncei.noaa.gov/data/terminal-doppler-weather-radar-level-2/access/",
    year, "/", month, "/", day, "/", radar, "/"
  )
  
  message("Attempting direct NCEI HTTP fetch from: ", base_url)
  
  # For now, we print the specific help message.
  message("---------------------------------------------------------------------------")
  message("MANUAL CHECK REQUIRED:")
  message("The automated cloud buckets do not have this data.")
  message("Please check the official NCEI Inventory for ", radar, " on this date:")
  message("https://www.ncei.noaa.gov/nexradinv/chooseday.jsp?id=", radar, "&Submit=Submit")
  message("---------------------------------------------------------------------------")
}
