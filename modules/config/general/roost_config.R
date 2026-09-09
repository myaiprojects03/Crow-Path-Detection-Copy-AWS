# =============================================================================
# roost_config.R — Consolidated Roost and Batch settings for Crow Path Detection
# =============================================================================

# Load general detection configurations first
source("modules/config/general/detection_config.R")

# 1. Roost Configurations & Presets
ROOST_PRESETS <- list()
dir.create("modules/config/roosts", showWarnings = FALSE, recursive = TRUE)
roost_files <- list.files("modules/config/roosts", pattern = "\\.R$", full.names = TRUE)
roost_files <- setdiff(roost_files, "modules/config/roosts/custom_roost_presets.R")
for (f in roost_files) {
  preset <- source(f)$value
  if (!is.null(preset) && is.list(preset) && !is.null(preset$name)) {
    # Ensure radars property exists, fallback to single radar_id if absent
    if (is.null(preset$radars) && !is.null(preset$radar_id)) {
      preset$radars <- preset$radar_id
    }
    if (is.null(preset$radar_id) && !is.null(preset$radars)) {
      preset$radar_id <- preset$radars[1]
    }
    ROOST_PRESETS[[preset$name]] <- preset
  }
}

# Load custom presets if the file exists
if (file.exists("modules/config/roosts/custom_roost_presets.R")) {
  source("modules/config/roosts/custom_roost_presets.R")
}

# Helper function to load a preset into the global environment
load_roost_preset <- function(preset_name, sensitivity = NULL) {
  if (!preset_name %in% names(ROOST_PRESETS)) {
    stop("Preset '", preset_name, "' not found.")
  }
  preset <- ROOST_PRESETS[[preset_name]]
  
  # Determine default sensitivity bundle: custom presets default to "high_sensitivity"
  built_ins <- c("North Bethesda KLWX", "North Bethesda TIAD", "Shirlington VA")
  sens_bundle <- sensitivity
  if (is.null(sens_bundle)) {
    if (exists("DETECTION_SENSITIVITY")) {
      if (DETECTION_SENSITIVITY == "standard" && !(preset_name %in% built_ins)) {
        sens_bundle <- "high_sensitivity"
      } else {
        sens_bundle <- DETECTION_SENSITIVITY
      }
    } else {
      sens_bundle <- if (preset_name %in% built_ins) "standard" else "high_sensitivity"
    }
  }
  
  # Initialize with sensitivity bundle parameters first, so roost-specific overrides take precedence
  if (exists("apply_sensitivity_bundle")) {
    apply_sensitivity_bundle(sens_bundle)
  }
  
  assign("ROOST_NAME",                 preset$name,                       envir = .GlobalEnv)
  assign("ROOST_LAT",                  preset$lat,                        envir = .GlobalEnv)
  assign("ROOST_LON",                  preset$lon,                        envir = .GlobalEnv)
  assign("LOCAL_TIMEZONE",             preset$timezone,                   envir = .GlobalEnv)
  assign("RADAR_ID",                   preset$radar_id,                   envir = .GlobalEnv)
  # Determine radar elevation: use preset override if defined, otherwise guess based on radar ID prefix
  elevation_deg <- preset$elevation
  if (is.null(elevation_deg)) {
    if (grepl("^T", preset$radar_id, ignore.case = TRUE)) {
      elevation_deg <- 0.3 # TDWR radars default to 0.3
    } else {
      elevation_deg <- 0.5 # WSR-88D radars default to 0.5
    }
  }
  assign("ELEVATION_DEG",              elevation_deg,                     envir = .GlobalEnv)
  if (!is.null(preset$date_start)) {
    assign("DATE_START",                 preset$date_start,                 envir = .GlobalEnv)
  }
  if (!is.null(preset$date_end)) {
    assign("DATE_END",                   preset$date_end,                   envir = .GlobalEnv)
  }
  # Calculate default map limits if not provided (30 km in all directions N/W/S/E)
  map_xlim <- preset$map_xlim
  map_ylim <- preset$map_ylim
  if (is.null(map_xlim) || is.null(map_ylim)) {
    # 1 degree of latitude is approx 111 km
    lat_offset <- 30 / 111
    # 1 degree of longitude is approx 111 * cos(lat) km
    lon_offset <- 30 / (111 * cos(preset$lat * pi / 180))
    
    if (is.null(map_xlim)) {
      map_xlim <- c(preset$lon - lon_offset, preset$lon + lon_offset)
    }
    if (is.null(map_ylim)) {
      map_ylim <- c(preset$lat - lat_offset, preset$lat + lat_offset)
    }
  }
  
  assign("MAP_XLIM",map_xlim, envir = .GlobalEnv)
  assign("MAP_YLIM",map_ylim, envir = .GlobalEnv)
  
  # Radar Display name mapping
  rad_name <- switch(preset$radar_id,
    "KLWX" = "Sterling",
    "TIAD" = "Dulles",
    "TDCA" = "National",
    "Sterling"
  )
  assign("RADAR_NAME",                 rad_name,                          envir = .GlobalEnv)
  
  # Override detection variables
  if (!is.null(preset$CONTRAST_THRESHOLD)) {
    assign("CONTRAST_THRESHOLD",         preset$CONTRAST_THRESHOLD,         envir = .GlobalEnv)
  }
  if (!is.null(preset$MAP_DBZH_TRANSPARENT_BELOW)) {
    assign("MAP_DBZH_TRANSPARENT_BELOW", preset$MAP_DBZH_TRANSPARENT_BELOW, envir = .GlobalEnv)
  }
  if (!is.null(preset$CORRIDOR_DENSITY_THRESHOLD)) {
    assign("CORRIDOR_DENSITY_THRESHOLD", preset$CORRIDOR_DENSITY_THRESHOLD, envir = .GlobalEnv)
  }
  if (!is.null(preset$LOCAL_PROMINENCE_THRESHOLD)) {
    assign("LOCAL_PROMINENCE_THRESHOLD", preset$LOCAL_PROMINENCE_THRESHOLD, envir = .GlobalEnv)
  }
  if (!is.null(preset$PIPELINE_FOCUS)) {
    assign("PIPELINE_FOCUS",             preset$PIPELINE_FOCUS,             envir = .GlobalEnv)
  } else {
    assign("PIPELINE_FOCUS",             "detail",                          envir = .GlobalEnv)
  }
  
  # Allow overriding any other detection variables if present in the preset
  extra_overrides <- c(
    "DETECT_DBZH_MIN", "MIN_CORRIDOR_PIXELS", "MIN_CONTIGUOUS_BINS",
    "RUN_START_BIN_MAX", "FLANK_MAX_DELTA_DEG", "LOCAL_PROMINENCE_WINDOW_DEG",
    "LOCAL_PROMINENCE_EXCLUDE_DEG", "MAX_CONTRAST_FOR_SCORING",
    "RUN_FILL_RATIO_MIN", "MAX_ANNULUS_WEATHER_COVERAGE",
    "MERGE_GAP_DEG", "DISPLAY_MERGE_GAP_DEG",
    "WEAK_STREAM_ANGLE_WINDOW_DEG", "WEAK_STREAM_FRACTION_THRESHOLD",
    "MAX_GAP_BINS", "EXTENT_NEIGHBOR_DEG", "RADAR_SUPPRESS_DIST_KM"
  )
  for (var_name in extra_overrides) {
    if (!is.null(preset[[var_name]])) {
      assign(var_name, preset[[var_name]], envir = .GlobalEnv)
      if (var_name == "DETECT_DBZH_MIN") {
        assign("ETA_THRESHOLD", 10^(preset[[var_name]] / 10), envir = .GlobalEnv)
      }
    }
  }
}

# =============================================================================
# AWS / HEADLESS CONFIGURATION (Manually edit these settings for non-GUI runs)
# =============================================================================
# Run mode. Options: "single" (single run), "batch" (batch operations)
RUN_MODE <- "batch"

# Roost preset to load. See available presets in modules/config/roosts/
# e.g., "North Bethesda KLWX", "North Bethesda TIAD", "Shirlington VA", 
# or any custom preset in custom_roost_presets.R
#ACTIVE_ROOST_PRESET <- "North Bethesda KLWX"


# Option B: Solar event relative offset window on a specific date
# (Leave SINGLE_RUN_DATE empty if you prefer using Option A)
SINGLE_RUN_DATE         <- ""       # Format: "YYYY-MM-DD" (e.g. "2024-10-12")
SINGLE_RUN_EVENT        <- "sunset" # Options: "sunset", "sunrise"
SINGLE_RUN_START_OFFSET <- -60      # minutes relative to event (negative is before)
SINGLE_RUN_END_OFFSET   <- 30       # minutes relative to event (positive is after)

