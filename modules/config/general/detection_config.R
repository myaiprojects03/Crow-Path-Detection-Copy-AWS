# =============================================================================
# detection_config.R — General detection, masking, and display configurations
# =============================================================================

CUSTOM_STATION_FILE          <- "data/locations.dat"
CUSTOM_STATION_FILE_FALLBACK <- "data/wsr88d_locations.dat"
MAX_FILES_PER_RUN            <- 3000L

#cartolight API Key
CARTO_API_KEY <- "cb1_2irk_1_3c9fadc7865a5b6c0e39cfb7"

# Scoring annulus (DETECTION ONLY)
DETECTION_MIN_DIST_KM <- 1
DETECTION_MAX_DIST_KM <- 30

# Suppress pixels near radar
RADAR_SUPPRESS_DIST_KM <- 12.0

# Plotting limits
PLOT_MIN_DIST_KM <- 1
PLOT_MAX_DIST_KM <- 30

# Corridor / flank geometry
BEARING_STEP_DEG       <- 1
CORRIDOR_HALF_WIDTH_KM <- 0.50
FLANK_OUTER_WIDTH_KM   <- 2.00
DIST_BIN_KM            <- 0.50

# Detection sensitivity bundle setting
# Options: "standard", "high_sensitivity" (errs on the side of false positives, e.g. for planet exploration)
if (!exists("DETECTION_SENSITIVITY")) {
  DETECTION_SENSITIVITY <- "standard"
}

# Detection thresholds
DETECT_DBZH_MIN     <- 8
ETA_THRESHOLD       <- 10^(DETECT_DBZH_MIN / 10)
MIN_CORRIDOR_PIXELS <- 4
MIN_CONTIGUOUS_BINS <- 3
RUN_START_BIN_MAX   <- 25L
FLANK_MAX_DELTA_DEG <- 15
MAX_GAP_BINS        <- 35L

# Local prominence
LOCAL_PROMINENCE_WINDOW_DEG  <- 12
LOCAL_PROMINENCE_EXCLUDE_DEG <- 2
LOCAL_PROMINENCE_THRESHOLD   <- 1.05
MAX_CONTRAST_FOR_SCORING     <- 5

# Ratio and coverage thresholds
CONTRAST_THRESHOLD           <- 0.80
CORRIDOR_DENSITY_THRESHOLD   <- 0.05
RUN_FILL_RATIO_MIN           <- 0.30
MAX_ANNULUS_WEATHER_COVERAGE <- 0.30
WEATHER_COVERAGE_DBZH_MIN    <- 10.0

apply_sensitivity_bundle <- function(bundle_name = DETECTION_SENSITIVITY) {
  if (bundle_name == "high_sensitivity") {
    assign("DETECT_DBZH_MIN",              4.0,  envir = .GlobalEnv)
    assign("ETA_THRESHOLD",                10^(4.0 / 10), envir = .GlobalEnv)
    assign("MIN_CORRIDOR_PIXELS",          2L,   envir = .GlobalEnv)
    assign("MIN_CONTIGUOUS_BINS",          2L,   envir = .GlobalEnv)
    assign("RUN_START_BIN_MAX",            35L,  envir = .GlobalEnv)
    assign("LOCAL_PROMINENCE_THRESHOLD",   0.30, envir = .GlobalEnv)
    assign("CONTRAST_THRESHOLD",           0.45, envir = .GlobalEnv)
    assign("CORRIDOR_DENSITY_THRESHOLD",   0.02, envir = .GlobalEnv)
    assign("RUN_FILL_RATIO_MIN",           0.15, envir = .GlobalEnv)
    assign("MAX_ANNULUS_WEATHER_COVERAGE", 0.50, envir = .GlobalEnv)
  } else if (bundle_name == "morning") {
    assign("DETECT_DBZH_MIN",              6.0,  envir = .GlobalEnv)
    assign("ETA_THRESHOLD",                10^(6.0 / 10), envir = .GlobalEnv)
    assign("MIN_CORRIDOR_PIXELS",          3L,   envir = .GlobalEnv)
    assign("RUN_START_BIN_MAX",            30L,  envir = .GlobalEnv)
    assign("RUN_FILL_RATIO_MIN",           0.20, envir = .GlobalEnv)
    assign("MAX_ANNULUS_WEATHER_COVERAGE", 0.50, envir = .GlobalEnv)
  } else if (bundle_name == "tdwr") {
    assign("DETECT_DBZH_MIN",              5.0,  envir = .GlobalEnv)
    assign("ETA_THRESHOLD",                10^(5.0 / 10), envir = .GlobalEnv)
    assign("CONTRAST_THRESHOLD",           0.85, envir = .GlobalEnv)
    assign("CORRIDOR_DENSITY_THRESHOLD",   0.08, envir = .GlobalEnv)
    assign("MAX_ANNULUS_WEATHER_COVERAGE", 0.30, envir = .GlobalEnv)
  } else {
    assign("DETECT_DBZH_MIN",              8.0,  envir = .GlobalEnv)
    assign("ETA_THRESHOLD",                10^(8.0 / 10), envir = .GlobalEnv)
    assign("MIN_CORRIDOR_PIXELS",          4L,   envir = .GlobalEnv)
    assign("MIN_CONTIGUOUS_BINS",          3L,   envir = .GlobalEnv)
    assign("RUN_START_BIN_MAX",            25L,  envir = .GlobalEnv)
    assign("LOCAL_PROMINENCE_THRESHOLD",   1.05, envir = .GlobalEnv)
    assign("CONTRAST_THRESHOLD",           0.80, envir = .GlobalEnv)
    assign("CORRIDOR_DENSITY_THRESHOLD",   0.05, envir = .GlobalEnv)
    assign("RUN_FILL_RATIO_MIN",           0.30, envir = .GlobalEnv)
    assign("MAX_ANNULUS_WEATHER_COVERAGE", 0.30, envir = .GlobalEnv)
  }
}

apply_sensitivity_bundle(DETECTION_SENSITIVITY)

# Stream merging / selection
# Merge parameters
MERGE_GAP_DEG         <- 6
MAX_STREAMS_PER_SCAN  <- 5
DISPLAY_MERGE_GAP_DEG <- 12
EXTENT_NEIGHBOR_DEG   <- 5

# Detection masking
USE_VOL2BIRD              <- FALSE
USE_MISTNET_FOR_DETECTION <- FALSE
MASK_CELL                 <- FALSE
MASK_WEATHER              <- FALSE
MASK_RHOHV                <- TRUE

CELL_THRESHOLD    <- 1
WEATHER_THRESHOLD <- 0.25
RHOHV_THRESHOLD   <- 0.95


# ---------------------------------------------------------------------------
# Map display settings
# ---------------------------------------------------------------------------
# Note: Radar scan elevation angle (ELEVATION_DEG) is dynamically determined 
# at load time based on the radar ID prefix:
# - TDWR radars (starting with "T", e.g., TDCA, TIAD) default to 0.3 degrees.
# - WSR-88D weather radars (starting with "K", e.g., KLWX) default to 0.5 degrees.
# These defaults can be overridden individually within any roost preset config.
MAP_BASEMAP     <- "cartolight"
MAP_DBZH_MIN    <- -20
MAP_DBZH_MAX    <-  50
MAP_PIXEL_SHAPE <- 15
MAP_PIXEL_SIZE  <- 2.5
MAP_PIXEL_ALPHA <- 0.95
MAP_ZOOM        <- 0
MAP_DBZH_TRANSPARENT_BELOW <- 5

MAP_WIDTH_IN      <- 10
MAP_HEIGHT_IN     <-  8
MAP_DPI           <- 220
PROFILE_WIDTH_IN  <- 10
PROFILE_HEIGHT_IN <-  4.5
PROFILE_DPI       <- 180

# File I/O
DATA_DIR             <- "data/data_pvol"
OUTPUT_DIR           <- "output"
OVERWRITE_RUN_OUTPUT <- FALSE

# Weak stream filtering near strong neighbors
WEAK_STREAM_ANGLE_WINDOW_DEG   <- 40
WEAK_STREAM_FRACTION_THRESHOLD <- 0.05

# Parallel Processing Settings
USE_PARALLEL <- TRUE
NUM_CORES    <- 7L

