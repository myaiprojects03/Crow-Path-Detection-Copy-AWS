# =============================================================================
# main.R â€” Unified Entry Point for Crow Path Detection Pipeline
# =============================================================================
#
# Usage:
#   Rscript main.R [mode/preset]
#
# =============================================================================

# 1. Setup Dependencies & Load Environment
source("Dependencies/install_deps.R")
source("modules/pipeline/bootstrap.R")
load_required_packages()

# Load batch packages explicitly to ensure global availability
library(suncalc)
library(openxlsx)
library(aws.s3)
library(rlang)

# Authenticated Carto tile source registered via detection_config.R


# Source all general project modules and pipeline logic
source_project_modules()
source("modules/pipeline/pipeline.R")

# Source unified configuration settings
source("modules/config/general/roost_config.R")
source("modules/config/general/batch_config.R")

# Source batch-specific dependencies
source("Dependencies/batch_dependencies/dates.R")
source("Dependencies/batch_dependencies/suntimes.R")
source("modules/output_example/batch_output.R")
source("Dependencies/batch_dependencies/batch_pipeline.R")

# 2. Parse Command-line Arguments and Resolve Settings
args <- commandArgs(trailingOnly = TRUE)
cmd_mode <- if (length(args) > 0) tolower(trimws(args[1])) else NULL

# Resolve active mode and load preset
if (!is.null(cmd_mode)) {
  # Command line override mode
  if (cmd_mode == "batch") {
    mode <- "batch"
    if (exists("ACTIVE_ROOST_PRESET")) {
      load_roost_preset(ACTIVE_ROOST_PRESET)
    } else {
      # Fallback to first available preset if none is active
      load_roost_preset(names(ROOST_PRESETS)[1])
    }
  } else {
    # Check if cmd_mode corresponds to a command preset or name mapping
    preset_name <- cmd_mode
    if (cmd_mode == "shirlington") {
      preset_name <- "Shirlington VA"
    } else if (cmd_mode %in% c("bethesda", "bethesda_klwx", "default")) {
      preset_name <- "North Bethesda KLWX"
    } else if (cmd_mode == "bethesda_tiad") {
      preset_name <- "North Bethesda TIAD"
    }
    
    # Load preset
    load_roost_preset(preset_name)
    mode <- "single"
  }
} else {
  # Use configuration file settings
  if (!exists("RUN_MODE")) {
    stop("RUN_MODE is not defined in roost_config.R. Please set it to 'single' or 'batch'.")
  }
  mode <- tolower(RUN_MODE)
  if (exists("ACTIVE_ROOST_PRESET")) {
    load_roost_preset(ACTIVE_ROOST_PRESET)
  }
}

# Apply parameters based on Pipeline Focus Mode (speed vs detail)
if (exists("PIPELINE_FOCUS")) {
  if (PIPELINE_FOCUS == "speed") {
    assign("USE_VOL2BIRD",              TRUE,  envir = .GlobalEnv)
    assign("USE_MISTNET_FOR_DETECTION", FALSE, envir = .GlobalEnv)
    assign("MASK_CELL",                 TRUE,  envir = .GlobalEnv)
    assign("MASK_WEATHER",              FALSE, envir = .GlobalEnv)
  } else {
    assign("MAP_DPI",                   300,   envir = .GlobalEnv)
    assign("PROFILE_DPI",               300,   envir = .GlobalEnv)
    assign("USE_VOL2BIRD",              FALSE, envir = .GlobalEnv)
    assign("MASK_CELL",                 FALSE, envir = .GlobalEnv)
    assign("USE_MISTNET_FOR_DETECTION", FALSE, envir = .GlobalEnv)
    assign("MASK_WEATHER",              FALSE, envir = .GlobalEnv)
  }
}

# 3. Execution Router
if (mode == "single") {
  message("\n=========================================================")
  message("LAUNCHING SINGLE RUN FOR PRESET: ", ROOST_NAME)
  message("=========================================================\n")
  
  # Resolve date range
  if (exists("SINGLE_RUN_DATE") && nzchar(SINGLE_RUN_DATE)) {
    # Option B: Solar event relative offsets on a specific date
    event_val <- if (exists("SINGLE_RUN_EVENT")) SINGLE_RUN_EVENT else "sunset"
    start_off <- if (exists("SINGLE_RUN_START_OFFSET")) SINGLE_RUN_START_OFFSET else -60L
    end_off   <- if (exists("SINGLE_RUN_END_OFFSET")) SINGLE_RUN_END_OFFSET else 30L
    
    window <- get_scan_window(
      date             = SINGLE_RUN_DATE,
      lat              = ROOST_LAT,
      lon              = ROOST_LON,
      event            = event_val,
      start_offset_min = start_off,
      end_offset_min   = end_off
    )
    
    if (is.na(window$start) || is.na(window$end)) {
      stop("Error computing sunrise/sunset for date '", SINGLE_RUN_DATE, "' at lat=", ROOST_LAT, " lon=", ROOST_LON)
    }
    
    start_time <- window$start
    end_time   <- window$end
  } else {
    # Option A: Absolute time window
    if (!exists("SINGLE_RUN_START") || !exists("SINGLE_RUN_END")) {
      stop("SINGLE_RUN_START or SINGLE_RUN_END is not defined in roost_config.R.")
    }
    
    parse_tz <- if (exists("SINGLE_RUN_TZ") && tolower(SINGLE_RUN_TZ) == "utc") "UTC" else LOCAL_TIMEZONE
    start_time <- as.POSIXct(SINGLE_RUN_START, tz = parse_tz)
    end_time   <- as.POSIXct(SINGLE_RUN_END, tz = parse_tz)
    
    if (is.na(start_time) || is.na(end_time)) {
      stop("Invalid SINGLE_RUN_START or SINGLE_RUN_END date-time format. Please use YYYY-MM-DD HH:MM.")
    }
  }
  
  # Print details
  message("  Roost Lat:   ", ROOST_LAT)
  message("  Roost Lon:   ", ROOST_LON)
  message("  Radar ID:    ", RADAR_ID)
  message("  Radar Name:  ", RADAR_NAME)
  message("  Elevation:   ", ELEVATION_DEG, " degrees")
  message("  Start Time:  ", format(start_time, "%Y-%m-%d %H:%M %Z", tz = LOCAL_TIMEZONE))
  message("  End Time:    ", format(end_time, "%Y-%m-%d %H:%M %Z", tz = LOCAL_TIMEZONE))
  message("=========================================================\n")
  
  run_pipeline(start_time, end_time)
  
} else if (mode == "batch") {
  message("\n=========================================================")
  message("LAUNCHING CROW DETECTOR IN BATCH RUN MODE")
  message("  Roost Preset: ", ROOST_NAME)
  message("=========================================================\n")
  
  run_batch()
  
} else {
  stop("Unknown run option '", mode, "'. Supported modes: 'single', 'batch'")
}

