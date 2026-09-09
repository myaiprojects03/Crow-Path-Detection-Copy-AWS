# =============================================================================
# vp_processing.R — Vertical profile computation and assembly
# =============================================================================
#
# Provides:
#   compute_vp_for_file(file_path)  →  a bioRad 'vp' object
#   build_vpts(vp_list)             →  a bioRad 'vpts' time-series object
#   summarise_vpts(vpts)            →  data.frame of key vpts fields for reporting
# =============================================================================

# ---------------------------------------------------------------------------
# Helper: load station file for TDWR radars
# ---------------------------------------------------------------------------
bt_load_station_file <- function(path) {
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    tryCatch(
      vol2birdR::nexrad_station_file(path),
      error = function(e) warning("Could not load station file: ", e$message)
    )
  }
}

# ---------------------------------------------------------------------------
# compute_corridor_speed_from_file()
#   Isolates raw radial velocity data from gates inside the narrow stream
#   corridor (DBZH >= 5 and distance 5km to 30km, aligned with flight heading).
#   Reconstructs true flight speed using V = |VRADH| / |cos(azimuth - theta)|.
# ---------------------------------------------------------------------------
compute_corridor_speed_from_file <- function(file_path) {
  pvol <- tryCatch({
    bioRad::read_pvolfile(file_path)
  }, error = function(e) {
    return(NA_real_)
  })
  
  if (is.null(pvol)) return(NA_real_)
  
  # Get scan at low elevation (BT_ELEVATION_DEG)
  scan_obj <- tryCatch({
    bioRad::get_scan(pvol, elev = BT_ELEVATION_DEG)
  }, error = function(e) {
    scans <- pvol$scans
    if (length(scans) > 0) scans[[1]] else NULL
  })
  
  if (is.null(scan_obj)) return(NA_real_)
  
  dbzh <- unclass(scan_obj$params$DBZH)
  vrad <- unclass(scan_obj$params$VRADH)
  if (is.null(vrad)) {
    vrad <- unclass(scan_obj$params$VRAD)
  }
  
  if (is.null(dbzh) || is.null(vrad)) return(NA_real_)
  
  r <- scan_obj$geo$rstart + (0:(nrow(dbzh) - 1)) * scan_obj$geo$rscale
  azimuth <- (0:(ncol(dbzh) - 1)) * scan_obj$geo$ascale
  
  n_bins <- length(r)
  n_rays <- length(azimuth)
  
  r_matrix <- matrix(r, nrow = n_bins, ncol = n_rays, byrow = FALSE)
  azimuth_matrix <- matrix(azimuth, nrow = n_bins, ncol = n_rays, byrow = TRUE)
  
  # Flight heading: use 85 degrees (ESE roost direction)
  theta <- 85
  cos_diff <- abs(cos((azimuth_matrix - theta) * pi / 180))
  
  # Select gates in crow corridor
  valid <- which(
    !is.na(dbzh) & !is.na(vrad) &
    dbzh >= 5 & dbzh <= 30 &
    r_matrix >= 5000 & r_matrix <= 30000 &
    cos_diff >= 0.5,
    arr.ind = TRUE
  )
  
  if (nrow(valid) >= 50) {
    vrad_vals <- vrad[valid]
    cos_vals  <- cos_diff[valid]
    
    speeds_kmph <- (abs(vrad_vals) / cos_vals) * 3.6
    clean_speeds <- speeds_kmph[speeds_kmph >= 10 & speeds_kmph <= 100]
    
    if (length(clean_speeds) >= 30) {
      return(median(clean_speeds))
    }
  }
  
  return(NA_real_)
}

# ---------------------------------------------------------------------------
# compute_vp_for_file()
#   Runs vol2bird on one pvol file and returns a vp object.
#   Returns NULL on failure (so the caller can skip gracefully).
# ---------------------------------------------------------------------------
compute_vp_for_file <- function(file_path) {
  base_name <- basename(file_path)
  message("  [vol2bird] Processing: ", base_name)

  # Load station file (needed for TDWR IDs not in default bioRad table)
  bt_load_station_file(BT_CUSTOM_STATION_FILE)

  # Temporary output file for the vp H5 result
  vp_tmp <- tempfile(
    pattern = paste0(sub("(?:\\.gz)?$", "", base_name), "_vp_"),
    fileext = ".h5"
  )

  # Run vol2bird and read back the vp — cleanup temp file after read
  vp <- tryCatch({
    config <- vol2birdR::vol2bird_config()
    config$minNyquist <- 0.0
    vol2birdR::vol2bird(
      file    = file_path,
      config  = config,
      vpfile  = vp_tmp,
      verbose = FALSE
    )
    # Read back as a bioRad vp object (must happen BEFORE cleanup)
    vp_obj <- bioRad::read_vpfiles(vp_tmp)
    message("    \u2192 vp loaded (", nrow(vp_obj$data), " altitude layers)")
    
    # Estimate corridor speed using raw scan data
    corridor_speed_kmph <- tryCatch({
      compute_corridor_speed_from_file(file_path)
    }, error = function(e) {
      warning("Could not calculate corridor speed for ", base_name, ": ", e$message)
      NA_real_
    })
    attr(vp_obj, "corridor_speed_kmph") <- corridor_speed_kmph
    
    vp_obj
  }, error = function(e) {
    warning("vol2bird failed for ", base_name, ": ", e$message)
    NULL
  })

  # Always clean up temp file, regardless of success or failure
  if (file.exists(vp_tmp)) unlink(vp_tmp)

  vp
}

# ---------------------------------------------------------------------------
# build_vpts()
#   Combines a list of vp objects into a single vpts (vertical profile
#   time-series) object, sorted by scan time, applying raw corridor-restricted
#   radial velocities to override the velocity parameters.
# ---------------------------------------------------------------------------
build_vpts <- function(vp_list) {
  # Drop any NULLs (failed scans)
  vp_list <- Filter(Negate(is.null), vp_list)
  if (length(vp_list) == 0) {
    stop("No valid vertical profiles to assemble into vpts.")
  }

  # 1. Extract corridor-restricted speeds from attributes
  speeds <- sapply(vp_list, function(vp) {
    val <- attr(vp, "corridor_speed_kmph")
    if (is.null(val)) NA_real_ else val
  })
  
  # 2. Compute event-wide average of valid corridor speeds to use as fallback
  avg_speed_kmph <- mean(speeds, na.rm = TRUE)
  if (is.na(avg_speed_kmph) || avg_speed_kmph == 0) {
    avg_speed_kmph <- BT_CROW_SPEED_KMPH  # config fallback (48 km/h)
  }
  
  message("  Estimated corridor speeds across scans (median): ", 
          paste(round(speeds, 1), collapse = ", "))
  message("  Event-wide average corridor speed (fallback): ", round(avg_speed_kmph, 1), " km/h")
  
  # 3. Apply to each vp object's data slots
  for (i in seq_along(vp_list)) {
    speed_kmph <- if (is.na(speeds[i])) avg_speed_kmph else speeds[i]
    speed_ms <- speed_kmph / 3.6
    
    # Overwrite ff vector
    vp_list[[i]]$data$ff <- speed_ms
    
    # Update u and v using target direction (or existing dd direction if valid)
    dd_rad <- vp_list[[i]]$data$dd * pi / 180
    default_dir <- if (is.null(BT_FLIGHT_DIRECTION_DEG) || is.na(BT_FLIGHT_DIRECTION_DEG)) 90 else BT_FLIGHT_DIRECTION_DEG
    dd_rad[is.na(dd_rad)] <- default_dir * pi / 180
    
    vp_list[[i]]$data$u <- speed_ms * sin(dd_rad)
    vp_list[[i]]$data$v <- speed_ms * cos(dd_rad)
  }

  message("Assembling vpts from ", length(vp_list), " vertical profiles...")
  vpts <- bioRad::bind_into_vpts(vp_list)
  vpts
}

# ---------------------------------------------------------------------------
# summarise_vpts()
#   Extracts the most useful fields from a vpts object into a flat
#   data.frame for reporting and inspection.
# ---------------------------------------------------------------------------
summarise_vpts <- function(vpts) {
  # bioRad stores vpts as a list with $data (height × time matrix) and $datetime
  # We pull the key integrated quantities already stored in the vpts object
  tryCatch({
    # Retrieve column data
    dt   <- vpts$datetime
    dens <- bioRad::get_quantity(vpts, "dens")   # bird density (km^-3)
    eta  <- bioRad::get_quantity(vpts, "eta")    # reflectivity (cm^2 km^-3)
    u    <- bioRad::get_quantity(vpts, "u")      # E-W wind component (m/s)
    v    <- bioRad::get_quantity(vpts, "v")      # N-S wind component (m/s)
    w    <- bioRad::get_quantity(vpts, "w")      # vertical velocity (m/s)
    sd_vvp <- bioRad::get_quantity(vpts, "sd_vvp")  # radial velocity std dev

    # Heights
    heights <- vpts$height  # vector of altitude bin centres (m AGL)

    # Build long-format data.frame: one row per (time × height)
    n_times  <- length(dt)
    n_heights <- length(heights)

    expand_to_df <- function(mat, name) {
      as.vector(mat)
    }

    df <- data.frame(
      datetime = rep(dt,        each = n_heights),
      height_m = rep(heights,   times = n_times),
      dens     = expand_to_df(dens,   "dens"),
      eta      = expand_to_df(eta,    "eta"),
      u_ms     = expand_to_df(u,      "u"),
      v_ms     = expand_to_df(v,      "v"),
      w_ms     = expand_to_df(w,      "w"),
      sd_vvp   = expand_to_df(sd_vvp, "sd_vvp"),
      stringsAsFactors = FALSE
    )

    # Derived: ground speed and direction from u,v
    df$ground_speed_ms   <- sqrt(df$u_ms^2 + df$v_ms^2)
    df$ground_speed_kmph <- df$ground_speed_ms * 3.6
    # Compass direction TO which the birds are flying (clockwise from North):
    df$flight_dir_deg <- (atan2(df$u_ms, df$v_ms) * 180 / pi) %% 360

    df
  }, error = function(e) {
    warning("Could not extract vpts summary: ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# regularize_vpts()
#   Regularizes the vpts onto a uniform time grid.
#   bioRad requires a regular vpts for integrate_profile().
# ---------------------------------------------------------------------------
bt_regularize_vpts <- function(vpts) {
  if (!isTRUE(vpts$regular)) {
    message("  Regularizing vpts onto uniform time grid...")
    vpts <- bioRad::regularize_vpts(vpts)
  }
  vpts
}

# ---------------------------------------------------------------------------
# filter_vpts_altitude()
#   Subsets a vpts to heights below BT_ALT_MAX_M (if set).
#   Note: bioRad 0.11.0 filter_vpts() does not accept an 'alt' argument,
#   so we subset the height dimension manually.
# ---------------------------------------------------------------------------
filter_vpts_altitude <- function(vpts) {
  if (is.null(BT_ALT_MAX_M) || is.na(BT_ALT_MAX_M)) return(vpts)

  keep <- vpts$height <= BT_ALT_MAX_M
  if (all(keep)) return(vpts)   # nothing to filter

  vpts$height <- vpts$height[keep]

  # Subset each matrix in vpts$data (rows = heights, cols = times)
  for (nm in names(vpts$data)) {
    m <- vpts$data[[nm]]
    if (is.matrix(m) && nrow(m) == length(keep)) {
      vpts$data[[nm]] <- m[keep, , drop = FALSE]
    }
  }

  message("  Altitude filter applied: 0 – ", BT_ALT_MAX_M, " m AGL (",
          sum(keep), " of ", length(keep), " height layers retained)")
  vpts
}
