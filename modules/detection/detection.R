# =============================================================================
# detection.R — Pixel preparation, corridor scoring, and stream selection
# =============================================================================

# Add distance, bearing, annulus flag, eta, and distance-bin columns to pixels
prepare_detection_pixels <- function(det_df) {
  det_df$dist_km <- vectorized_distance_km(
    ROOST_LAT, ROOST_LON, det_df$lat, det_df$lon
  )
  det_df$bearing_px <- vectorized_bearing_deg(
    ROOST_LAT, ROOST_LON, det_df$lat, det_df$lon
  )
  det_df$in_annulus <- det_df$dist_km >= DETECTION_MIN_DIST_KM &
    det_df$dist_km <= DETECTION_MAX_DIST_KM

  det_df$eta <- ifelse(
    !is.na(det_df$DBZH_DETECT) & det_df$DBZH_DETECT >= DETECT_DBZH_MIN,
    10^(det_df$DBZH_DETECT / 10),
    NA_real_
  )

  det_df$dist_bin <- pmax(
    1L,
    floor((det_df$dist_km - DETECTION_MIN_DIST_KM) / DIST_BIN_KM) + 1L
  )

  det_df
}

# Compute corridor/flank eta statistics and detection flags for every bearing
score_bearings <- function(pixel_df) {
  annulus_df <- pixel_df[pixel_df$in_annulus, , drop = FALSE]
  bearings   <- seq(0, 359, by = BEARING_STEP_DEG)

  scored <- lapply(bearings, function(bearing_deg) {
    if (nrow(annulus_df) == 0) {
      return(data.frame(
        bearing_deg            = bearing_deg,
        corridor_eta_mean      = 0,
        corridor_eta_sum       = 0,
        corridor_density       = 0,
        flank_eta_mean         = 0,
        contrast_ratio         = 0,
        corridor_valid_pixels  = 0L,
        flank_valid_pixels     = 0L,
        corridor_total_pixels  = 0L,
        max_contiguous_bins    = 0L,
        contiguous_extent_km   = 0,
        max_extent_km          = 0
      ))
    }

    delta_deg      <- wrapped_delta_deg(annulus_df$bearing_px, bearing_deg)
    along_track_km <- annulus_df$dist_km * cos(delta_deg * pi / 180)
    cross_track_km <- annulus_df$dist_km * abs(sin(delta_deg * pi / 180))

    corridor_all <- along_track_km >= 0 & cross_track_km <= CORRIDOR_HALF_WIDTH_KM
    # Restrict flank to pixels within FLANK_MAX_DELTA_DEG of the corridor bearing.
    # Without this, a bright adjacent stream bleeds into the flank reference zone,
    # raising flank_eta_mean and collapsing the contrast ratio below threshold.
    flank_all    <- along_track_km >= 0 &
      cross_track_km > CORRIDOR_HALF_WIDTH_KM &
      cross_track_km <= FLANK_OUTER_WIDTH_KM &
      abs(delta_deg) <= FLANK_MAX_DELTA_DEG

    corridor_sig <- corridor_all & !is.na(annulus_df$eta)

    use_matched_flank <- exists("MATCHED_FLANK_BINS") && isTRUE(MATCHED_FLANK_BINS)
    if (use_matched_flank) {
      active_bins <- unique(annulus_df$dist_bin[corridor_sig])
      flank_all   <- flank_all & (annulus_df$dist_bin %in% active_bins)
    }

    flank_sig    <- flank_all    & !is.na(annulus_df$eta)

    corridor_eta  <- annulus_df$eta[corridor_sig]
    corridor_dist <- annulus_df$dist_km[corridor_sig]
    flank_eta     <- annulus_df$eta[flank_sig]

    corridor_eta_mean <- if (length(corridor_eta) > 0) mean(corridor_eta) else 0
    corridor_eta_sum  <- if (length(corridor_eta) > 0) sum(corridor_eta)  else 0
    flank_eta_mean    <- if (length(flank_eta) > 0)    mean(flank_eta)    else 0

    contrast_ratio <- if (flank_eta_mean > 0) {
      corridor_eta_mean / flank_eta_mean
    } else if (corridor_eta_mean > 0) {
      Inf
    } else {
      0
    }

    use_dist_scoring  <- exists("DISTANCE_WEIGHT_SCORING") && isTRUE(DISTANCE_WEIGHT_SCORING)
    dist_weighted_eta <- if (use_dist_scoring && length(corridor_eta) > 0) {
      sum(corridor_eta * ((corridor_dist / 10.0)^1.5))
    } else {
      corridor_eta_sum
    }

    corridor_bins <- annulus_df$dist_bin[corridor_sig]
    run_details   <- get_longest_run_details(corridor_bins, max_gap = MAX_GAP_BINS)
    extent_bin    <- get_trail_extent_furthest_bin(corridor_bins, max_gap = MAX_GAP_BINS)

    # Line length uses the furthest corridor signal, including bridged gaps and
    # continuation beyond the first near-roost run (detection metrics still use
    # run_details from the first significant segment only).
    max_extent_km <- if (extent_bin > 0) {
      DETECTION_MIN_DIST_KM + (extent_bin * DIST_BIN_KM)
    } else {
      0
    }

    # Use span-based denominator (furthest - start + 1) so a dense run that
    # starts beyond bin 1 is not penalised for the gap between the roost and
    # the run start.
    run_fill_ratio <- if (run_details$furthest > 0) {
      run_details$length / (run_details$furthest - run_details$start + 1)
    } else {
      0
    }

    data.frame(
      bearing_deg           = bearing_deg,
      corridor_eta_mean     = round(corridor_eta_mean, 6),
      corridor_eta_sum      = round(corridor_eta_sum, 6),
      dist_weighted_eta     = round(dist_weighted_eta, 6),
      corridor_density      = round(
        ifelse(sum(corridor_all) > 0, sum(corridor_sig) / sum(corridor_all), 0),
        6
      ),
      flank_eta_mean        = round(flank_eta_mean, 6),
      contrast_ratio        = round(contrast_ratio, 6),
      corridor_valid_pixels = as.integer(sum(corridor_sig)),
      flank_valid_pixels    = as.integer(sum(flank_sig)),
      corridor_total_pixels = as.integer(sum(corridor_all)),
      max_contiguous_bins   = as.integer(run_details$length),
      contiguous_extent_km  = round(run_details$length * DIST_BIN_KM, 3),
      max_extent_km         = round(max_extent_km, 3),
      run_fill_ratio        = round(run_fill_ratio, 3),
      run_start_bin         = as.integer(run_details$start)
    )
  })

  scores <- do.call(rbind, scored)

  use_dist_scoring <- exists("DISTANCE_WEIGHT_SCORING") && isTRUE(DISTANCE_WEIGHT_SCORING)

  # Local prominence: ratio of each bearing's eta to its angular neighbourhood
  scores$local_background_eta <- vapply(scores$bearing_deg, function(b) {
    d    <- angular_distance_deg(scores$bearing_deg, b)
    keep <- d >= LOCAL_PROMINENCE_EXCLUDE_DEG & d <= LOCAL_PROMINENCE_WINDOW_DEG
    vals <- if (use_dist_scoring) scores$dist_weighted_eta[keep] else scores$corridor_eta_sum[keep]
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(0)
    mean(vals)
  }, numeric(1))

  prom_num <- if (use_dist_scoring) scores$dist_weighted_eta else scores$corridor_eta_sum
  scores$local_prominence_ratio <- ifelse(
    scores$local_background_eta > 0,
    prom_num / scores$local_background_eta,
    ifelse(prom_num > 0, Inf, 0)
  )

  # Composite quality score used for stream ranking
  scores$contrast_for_score <- pmin(scores$contrast_ratio, MAX_CONTRAST_FOR_SCORING)
  if (use_dist_scoring) {
    scores$quality_score <- with(
      scores,
      pmax(corridor_valid_pixels, 1) *
        (pmax(contiguous_extent_km, DIST_BIN_KM)^1.5) *
        (pmax(max_extent_km, DIST_BIN_KM)^1.5) *
        pmax(contrast_for_score, 0.5) *
        pmax(local_prominence_ratio, 0.5) *
        pmax(run_fill_ratio, 0.1)
    )
  } else {
    scores$quality_score <- with(
      scores,
      corridor_eta_sum *
        pmax(contrast_for_score, 1) *
        pmax(contiguous_extent_km, DIST_BIN_KM) *
        pmax(local_prominence_ratio, 1) *
        pmax(run_fill_ratio, 0.1)
    )
  }

  # Boolean detection flag: all thresholds must pass
  scores$stream_detected <- with(
    scores,
    corridor_eta_mean >= ETA_THRESHOLD &
      contrast_ratio >= CONTRAST_THRESHOLD &
      corridor_valid_pixels >= MIN_CORRIDOR_PIXELS &
      max_contiguous_bins >= MIN_CONTIGUOUS_BINS &
      local_prominence_ratio >= LOCAL_PROMINENCE_THRESHOLD &
      corridor_density >= CORRIDOR_DENSITY_THRESHOLD &
      run_fill_ratio >= RUN_FILL_RATIO_MIN &
      run_start_bin <= RUN_START_BIN_MAX
  )

  scores$compass <- bearing_to_compass(scores$bearing_deg)
  scores
}

# Group detected bearings into contiguous angular zones, handling wraparound
split_bearing_zones <- function(bearings, merge_gap_deg) {
  bearings <- sort(unique(bearings))
  if (length(bearings) == 0) return(list())
  if (length(bearings) == 1) return(list(bearings))

  zones   <- list()
  current <- bearings[1]

  for (i in 2:length(bearings)) {
    if ((bearings[i] - bearings[i - 1]) <= merge_gap_deg) {
      current <- c(current, bearings[i])
    } else {
      zones[[length(zones) + 1L]] <- current
      current <- bearings[i]
    }
  }
  zones[[length(zones) + 1L]] <- current

  # Merge last zone into first if they wrap around 360°
  if (length(zones) > 1 &&
      angular_distance_deg(
        zones[[1]][1],
        zones[[length(zones)]][length(zones[[length(zones)]])]
      ) <= merge_gap_deg) {
    zones[[1]] <- c(zones[[length(zones)]], zones[[1]])
    zones <- zones[-length(zones)]
  }

  zones
}

# Merge selected streams that lie within DISPLAY_MERGE_GAP_DEG of each other
# into one display row (mean bearing, max extent).
merge_display_streams <- function(streams_df,
                                  merge_gap_deg = DISPLAY_MERGE_GAP_DEG) {
  if (nrow(streams_df) <= 1) return(streams_df)

  ordered <- streams_df[order(streams_df$bearing_deg), , drop = FALSE]
  groups  <- list(1L)

  for (i in 2:nrow(ordered)) {
    grp_idx   <- groups[[length(groups)]]
    trial     <- ordered$bearing_deg[c(grp_idx, i)]
    if (group_angular_span_deg(trial) <= merge_gap_deg) {
      groups[[length(groups)]] <- c(grp_idx, i)
    } else {
      groups[[length(groups) + 1L]] <- i
    }
  }

  if (length(groups) > 1) {
    first_idx <- groups[[1]]
    last_idx  <- groups[[length(groups)]]
    trial     <- c(ordered$bearing_deg[first_idx], ordered$bearing_deg[last_idx])
    if (group_angular_span_deg(trial) <= merge_gap_deg) {
      groups[[1]] <- c(last_idx, first_idx)
      groups      <- groups[-length(groups)]
    }
  }

  merged <- lapply(groups, function(idx) {
    chunk <- ordered[idx, , drop = FALSE]
    extent_vals <- if ("own_max_extent_km" %in% names(chunk)) chunk$own_max_extent_km else chunk$max_extent_km
    best_idx <- order(-extent_vals, -chunk$corridor_valid_pixels, -chunk$quality_score)[1]
    
    use_dist_compass <- exists("DISTANCE_WEIGHT_COMPASS") && isTRUE(DISTANCE_WEIGHT_COMPASS)
    display_bearing  <- if (use_dist_compass && nrow(chunk) > 1) {
      max_d <- if (exists("DETECTION_MAX_DIST_KM")) DETECTION_MAX_DIST_KM else 30.0
      dist_weight <- 1 + (pmin(chunk$max_extent_km, max_d) / max_d)
      weights <- dist_weight * chunk$corridor_valid_pixels
      round(circular_weighted_mean_deg(chunk$bearing_deg, weights)) %% 360
    } else {
      chunk$bearing_deg[best_idx]
    }

    support_parts <- unlist(strsplit(chunk$supporting_bearings, ";", fixed = TRUE))
    support_parts <- sort(unique(support_parts[nzchar(support_parts)]))

    data.frame(
      bearing_deg            = display_bearing,
      compass                = bearing_to_compass(display_bearing),
      quality_score          = chunk$quality_score[best_idx],
      corridor_eta_sum       = chunk$corridor_eta_sum[best_idx],
      corridor_eta_mean      = chunk$corridor_eta_mean[best_idx],
      contrast_ratio         = chunk$contrast_ratio[best_idx],
      local_prominence_ratio = chunk$local_prominence_ratio[best_idx],
      corridor_valid_pixels  = chunk$corridor_valid_pixels[best_idx],
      contiguous_extent_km   = max(chunk$contiguous_extent_km),
      max_extent_km          = max(chunk$max_extent_km),
      supporting_bearings    = paste(support_parts, collapse = ";"),
      stringsAsFactors       = FALSE
    )
  })

  out <- do.call(rbind, merged)
  out[order(-out$quality_score, -out$corridor_eta_sum), , drop = FALSE]
}

# Keep only bearings near the peak whose quality score is >= min_peak_fraction
refine_zone_support <- function(zone_active, peak_bearing_deg,
                                min_peak_fraction  = 0.45,
                                max_refine_span_deg = 6) {
  ordered   <- zone_active[order(zone_active$bearing_deg), , drop = FALSE]
  peak_idx  <- which(ordered$bearing_deg == peak_bearing_deg)[1]
  peak_score <- ordered$quality_score[peak_idx]
  keep <- rep(FALSE, nrow(ordered))
  keep[peak_idx] <- TRUE

  left_idx <- peak_idx - 1L
  while (left_idx >= 1L) {
    if (angular_distance_deg(ordered$bearing_deg[left_idx], peak_bearing_deg) >
        max_refine_span_deg) break
    if (ordered$quality_score[left_idx] < peak_score * min_peak_fraction) break
    keep[left_idx] <- TRUE
    left_idx <- left_idx - 1L
  }

  right_idx <- peak_idx + 1L
  while (right_idx <= nrow(ordered)) {
    if (angular_distance_deg(ordered$bearing_deg[right_idx], peak_bearing_deg) >
        max_refine_span_deg) break
    if (ordered$quality_score[right_idx] < peak_score * min_peak_fraction) break
    keep[right_idx] <- TRUE
    right_idx <- right_idx + 1L
  }

  refined <- ordered[keep, , drop = FALSE]
  if (nrow(refined) == 0) refined <- ordered[peak_idx, , drop = FALSE]
  refined
}

# Pick the best stream per zone and return up to MAX_STREAMS_PER_SCAN
select_streams <- function(scores) {
  active <- scores[scores$stream_detected, , drop = FALSE]
  if (nrow(active) == 0) return(data.frame())

  zones    <- split_bearing_zones(active$bearing_deg, MERGE_GAP_DEG)
  selected <- lapply(zones, function(zone_bearings) {
    zone_scores <- scores[scores$bearing_deg %in% zone_bearings, , drop = FALSE]
    zone_active <- zone_scores[zone_scores$stream_detected, , drop = FALSE]
    use_dist_compass <- exists("DISTANCE_WEIGHT_COMPASS") && isTRUE(DISTANCE_WEIGHT_COMPASS)
    peak_idx <- if (use_dist_compass) {
      order(
        -zone_active$max_extent_km,
        -zone_active$contiguous_extent_km,
        -zone_active$corridor_valid_pixels,
        -zone_active$quality_score
      )[1]
    } else {
      order(
        -zone_active$quality_score,
        -zone_active$corridor_eta_sum,
         zone_active$bearing_deg
      )[1]
    }
    peak         <- zone_active[peak_idx, , drop = FALSE]
    refined_zone <- if (use_dist_compass) {
      refine_zone_support(zone_active, peak$bearing_deg, min_peak_fraction = 0.20, max_refine_span_deg = 15)
    } else {
      refine_zone_support(zone_active, peak$bearing_deg)
    }

    # Weighted mean bearing of the refined support region (weights further pixels when enabled)
    max_d <- if (exists("DETECTION_MAX_DIST_KM")) DETECTION_MAX_DIST_KM else 30.0
    dist_weight <- 1 + (pmin(refined_zone$max_extent_km, max_d) / max_d)
    bearing_weights <- if (use_dist_compass) {
      dist_weight * refined_zone$corridor_valid_pixels
    } else {
      pmax(refined_zone$quality_score, 1) * (refined_zone$max_extent_km^2)
    }
    display_bearing <- round(circular_weighted_mean_deg(
      refined_zone$bearing_deg,
      bearing_weights
    )) %% 360
    display_idx <- which.min(angular_distance_deg(
      refined_zone$bearing_deg,
      display_bearing
    ))
    display_row <- refined_zone[display_idx, , drop = FALSE]
    own_extent_km <- display_row$max_extent_km
    extent_km   <- max(
      display_row$max_extent_km,
      neighbor_max_extent_km(scores, display_bearing, EXTENT_NEIGHBOR_DEG)
    )

    data.frame(
      bearing_deg            = display_row$bearing_deg,
      compass                = bearing_to_compass(display_row$bearing_deg),
      quality_score          = display_row$quality_score,
      corridor_eta_sum       = display_row$corridor_eta_sum,
      corridor_eta_mean      = display_row$corridor_eta_mean,
      contrast_ratio         = display_row$contrast_ratio,
      local_prominence_ratio = display_row$local_prominence_ratio,
      corridor_valid_pixels  = display_row$corridor_valid_pixels,
      contiguous_extent_km   = display_row$contiguous_extent_km,
      own_max_extent_km      = own_extent_km,
      max_extent_km          = round(extent_km, 3),
      supporting_bearings    = paste(refined_zone$bearing_deg, collapse = ";"),
      stringsAsFactors       = FALSE
    )
  })

  selected <- do.call(rbind, selected)
  
  # Filter out weak streams that are close to much stronger streams
  if (nrow(selected) > 1) {
    keep <- rep(TRUE, nrow(selected))
    for (i in seq_len(nrow(selected))) {
      bearing_i <- selected$bearing_deg[i]
      score_i   <- selected$quality_score[i]
      extent_i  <- selected$own_max_extent_km[i]

      for (j in seq_len(nrow(selected))) {
        if (i == j) next
        bearing_j <- selected$bearing_deg[j]
        score_j   <- selected$quality_score[j]
        extent_j  <- selected$own_max_extent_km[j]

        ang_dist <- angular_distance_deg(bearing_i, bearing_j)
        if (ang_dist <= WEAK_STREAM_ANGLE_WINDOW_DEG) {
          if (score_j > score_i && (score_i / score_j) < WEAK_STREAM_FRACTION_THRESHOLD) {
            # Do not discard a long reach candidate (>=20km) for a short reach candidate (<20km)
            if (extent_i >= 20 && extent_j < 20) {
              next
            }
            keep[i] <- FALSE
            break
          }
        }
      }
    }
    selected <- selected[keep, , drop = FALSE]
  }

  selected <- selected[order(-selected$quality_score, -selected$corridor_eta_sum), , drop = FALSE]
  selected <- merge_display_streams(selected)
  head(selected, MAX_STREAMS_PER_SCAN)
}
