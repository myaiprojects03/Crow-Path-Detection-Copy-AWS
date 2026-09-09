# =============================================================================
# traffic_metrics.R — MTR, MT, RTR, RT computation
# =============================================================================
#
# Provides:
#   set_crow_rcs(vpts)                      →  vpts with RCS set to crow value
#   compute_integrated_profiles(vpts)       →  data.frame of per-scan integrated quantities
#   compute_event_totals(integrated_df)     →  data.frame of event-level MT, RT, etc.
#   estimate_flight_direction(integrated_df) → mean flight direction (degrees) from u/v
#
# Key formula reference (bioRad article, Fig. 2):
#   MTR [km⁻¹ h⁻¹] = VID [km⁻²] × ground_speed [km h⁻¹] × |cos(α)|
#     where α = angle between flight direction and transect normal
#   MT  [km⁻¹]    = ∫ MTR dt  (integral over the event window)
#   RTR [cm² km⁻¹ h⁻¹] = VIR × ground_speed × |cos(α)|
#   RT  [cm² km⁻¹]     = ∫ RTR dt
# =============================================================================

# ---------------------------------------------------------------------------
# set_crow_rcs()
#   Applies the crow radar cross-section to the vpts so that bioRad's
#   integrate_profile() converts reflectivity (eta) → bird density correctly.
# ---------------------------------------------------------------------------
set_crow_rcs <- function(vpts) {
  # bioRad stores RCS in the 'attributes' of the vpts object
  # The standard accessor is rcs(vpts) <- value
  tryCatch({
    bioRad::rcs(vpts) <- BT_CROW_RCS_CM2
    message("  Crow RCS set to ", BT_CROW_RCS_CM2, " cm²")
  }, error = function(e) {
    # Fallback: set as a direct attribute (older bioRad versions)
    attr(vpts, "rcs") <- BT_CROW_RCS_CM2
    message("  Crow RCS set via attr() to ", BT_CROW_RCS_CM2, " cm² (fallback)")
  })
  vpts
}

# ---------------------------------------------------------------------------
# scale_vpts_density_and_reflectivity()
#   Scales the dens (bird density) and eta (reflectivity) matrices in vpts
#   by the stream dilution correction factor (BT_STREAM_SCALE_FACTOR).
# ---------------------------------------------------------------------------
scale_vpts_density_and_reflectivity <- function(vpts) {
  if (is.null(BT_STREAM_SCALE_FACTOR) || is.na(BT_STREAM_SCALE_FACTOR)) {
    return(vpts)
  }
  
  vpts$data$dens <- vpts$data$dens * BT_STREAM_SCALE_FACTOR
  vpts$data$eta  <- vpts$data$eta  * BT_STREAM_SCALE_FACTOR
  
  message("  Scaled vertical profile bird density and reflectivity by stream factor: ", BT_STREAM_SCALE_FACTOR, "x")
  vpts
}

# ---------------------------------------------------------------------------
# compute_integrated_profiles()
#   Calls bioRad::integrate_profile() on the vpts and returns a data.frame.
# ---------------------------------------------------------------------------
compute_integrated_profiles <- function(vpts) {
  # Apply density corrections directly to the vpts object before integration
  vpts <- scale_vpts_density_and_reflectivity(vpts)

  message("Computing vertically integrated profiles (VID, VIR, MTR, RTR, MT, RT)...")

  integrated <- tryCatch({
    bioRad::integrate_profile(vpts)
  }, error = function(e) {
    stop("integrate_profile() failed: ", e$message)
  })

  # Ensure datetime is in the correct timezone for display
  if ("datetime" %in% names(integrated)) {
    integrated$datetime_local <- format(
      as.POSIXct(integrated$datetime, tz = "UTC"),
      tz    = BT_LOCAL_TZ,
      format = "%Y-%m-%d %H:%M"
    )
  }

  # bioRad uses 'ff' for ground speed (m/s) and 'dd' for direction
  if ("ff" %in% names(integrated)) {
    integrated$ground_speed_ms   <- integrated$ff
    integrated$ground_speed_kmph <- integrated$ff * 3.6
  }
  if ("dd" %in% names(integrated)) {
    # In bioRad, target dd is already the direction TO which the birds are flying
    integrated$flight_dir_deg <- integrated$dd
  }

  # Map standard columns to _corrected columns for downstream compatibility
  integrated$mtr_corrected <- integrated$mtr
  integrated$mt_corrected  <- integrated$mt
  integrated$rtr_corrected <- integrated$rtr
  integrated$rt_corrected  <- integrated$rt

  message("  integrate_profile() returned ", nrow(integrated), " rows.")
  message("  Columns: ", paste(names(integrated), collapse = ", "))
  integrated
}

# ---------------------------------------------------------------------------
# compute_event_totals()
#   Sums MTR and RTR over the full event window using the trapezoidal rule
#   (integrating over time) to give MT and RT.
#
#   MT  [km⁻¹]       = ∫ MTR dt   (total birds crossing 1 km transect)
#   RT  [cm² km⁻¹]   = ∫ RTR dt   (total reflectivity traffic — diagnostic)
# ---------------------------------------------------------------------------
compute_event_totals <- function(integrated_df) {
  message("Computing event totals (MT, RT)...")

  # bioRad's integrate_profile() already provides 'mt' and 'rt' columns
  # representing the cumulative sum (they accumulate over the vpts time range).
  # The final row gives the total event-level MT and RT.

  # Identify total columns
  col_map <- list(
    MTR             = c("mtr",  "MTR"),
    MT              = c("mt",   "MT"),
    RTR             = c("rtr",  "RTR"),
    RT              = c("rt",   "RT"),
    VID             = c("vid",  "VID"),
    VIR             = c("vir",  "VIR"),
    MTR_corrected   = c("mtr_corrected"),
    MT_corrected    = c("mt_corrected"),
    RTR_corrected   = c("rtr_corrected"),
    RT_corrected    = c("rt_corrected")
  )

  # Helper to find a column by candidate names
  find_col <- function(df, candidates) {
    found <- intersect(candidates, names(df))
    if (length(found) > 0) found[1] else NA_character_
  }

  totals_row <- list()
  for (metric in names(col_map)) {
    col_name <- find_col(integrated_df, col_map[[metric]])
    if (!is.na(col_name)) {
      vals <- integrated_df[[col_name]]
      non_na <- vals[!is.na(vals)]
      if (length(non_na) > 0) {
        if (metric %in% c("MT", "RT", "MT_corrected", "RT_corrected")) {
          # These are already cumulative sums — take the final value
          totals_row[[paste0("total_", metric)]] <- tail(non_na, 1)
        } else {
          # For rates, report peak and mean
          totals_row[[paste0("mean_",  metric)]] <- mean(non_na)
          totals_row[[paste0("peak_",  metric)]] <- max(non_na)
        }
      }
    }
  }

  # Add event window info
  totals_row$event_start_utc <- as.character(min(integrated_df$datetime, na.rm = TRUE))
  totals_row$event_end_utc   <- as.character(max(integrated_df$datetime, na.rm = TRUE))
  totals_row$n_scans         <- nrow(integrated_df)
  totals_row$radar_id        <- BT_RADAR_ID
  totals_row$site_name       <- BT_SITE_NAME
  totals_row$crow_rcs_cm2             <- BT_CROW_RCS_CM2
  totals_row$expected_ground_speed_kmph <- BT_CROW_SPEED_KMPH
  totals_row$flight_dir_deg           <- BT_FLIGHT_DIRECTION_DEG

  # Ground speed summary — bioRad stores ground speed as 'ff' (m/s)
  # We added 'ground_speed_kmph' as a derived column in compute_integrated_profiles()
  gs_col <- intersect(c("ground_speed_kmph"), names(integrated_df))
  if (length(gs_col) > 0) {
    gs <- integrated_df[[gs_col[1]]]
    gs <- gs[!is.na(gs)]
    if (length(gs) > 0) {
      totals_row$mean_ground_speed_kmph   <- round(mean(gs), 1)
      totals_row$median_ground_speed_kmph <- round(median(gs), 1)
    }
  }

  # Flight direction summary
  fd_col <- intersect(c("flight_dir_deg"), names(integrated_df))
  if (length(fd_col) > 0) {
    fd <- integrated_df[[fd_col[1]]]
    fd <- fd[!is.na(fd)]
    if (length(fd) > 0) {
      totals_row$mean_flight_dir_deg <- round(mean(fd), 1)
    }
  }

  totals_df <- as.data.frame(totals_row, stringsAsFactors = FALSE)
  message("  Event totals computed.")
  totals_df
}

# ---------------------------------------------------------------------------
# estimate_flight_direction()
#   Computes the reflectivity-weighted mean flight direction from u/v columns
#   in the integrated profiles data.frame.
#   Useful when BT_FLIGHT_DIRECTION_DEG is NA.
# ---------------------------------------------------------------------------
estimate_flight_direction <- function(integrated_df) {
  if (!all(c("u", "v") %in% names(integrated_df))) {
    warning("u/v columns not found in integrated profiles — cannot estimate flight direction.")
    return(NA_real_)
  }
  # Use VID as weight if available
  weight_col <- intersect(c("vid", "VID"), names(integrated_df))
  weights <- if (length(weight_col) > 0) {
    pmax(0, integrated_df[[weight_col[1]]], na.rm = TRUE)
  } else {
    rep(1, nrow(integrated_df))
  }
  weights[is.na(weights)] <- 0

  u_mean <- stats::weighted.mean(integrated_df$u, weights, na.rm = TRUE)
  v_mean <- stats::weighted.mean(integrated_df$v, weights, na.rm = TRUE)

  dir_deg <- (atan2(-u_mean, -v_mean) * 180 / pi) %% 360
  message("  Estimated flight direction (VID-weighted): ", round(dir_deg, 1), "°")
  dir_deg
}
