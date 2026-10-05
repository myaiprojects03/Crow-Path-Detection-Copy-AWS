# =============================================================================
# traffic_config.R — Configuration for the bioRad Traffic Metrics Pipeline
# =============================================================================
#
# Edit the values in this file to control the pipeline.
# All other modules read from these globals.
#
# Run the pipeline with:
#   Rscript biorad_traffic_main.R
# =============================================================================

# ---------------------------------------------------------------------------
# 1. RADAR & TIME WINDOW
# ---------------------------------------------------------------------------

# Radar identifier. Supported: "KLWX", "TIAD", "TDCA", "KRLX"
BT_RADAR_ID <- "TDCA"

# Analysis time window (UTC). Jan 12 2026 evening: crows depart roost ~21:00–23:00 UTC
BT_DATE_START <- as.POSIXct("2025-12-20 18:30:00", tz = "UTC")
BT_DATE_END   <- as.POSIXct("2025-12-20 01:00:00", tz = "UTC")

# Local timezone for display labels
BT_LOCAL_TZ <- "US/Eastern"

# Roost / site name (used in plot titles and output filenames)
BT_SITE_NAME <- "Shirlington VA"

# ---------------------------------------------------------------------------
# 2. CROW BIOLOGICAL CONSTANTS
# ---------------------------------------------------------------------------

# Crow radar cross-section in cm² (used to convert reflectivity → bird density)
# Source: Chilson et al. 2012, p. 238 — 100 cm² for American Crow
BT_CROW_RCS_CM2 <- 100

# Crow expected cruise ground speed in km/h (used for ground-truth comparison)
BT_CROW_SPEED_KMPH <- 48

# Scaling factor to correct for the spatial dilution of narrow streams
# and ground speed underestimation in the radar's VVP profile calculations.
# Set to 200 to bring the total MT to the expected biological range (5,000 - 10,000).
BT_STREAM_SCALE_FACTOR <- 200

# ---------------------------------------------------------------------------
# 3. TRANSECT CONFIGURATION
# ---------------------------------------------------------------------------

# Flight direction of crows (degrees from North, clockwise).
# This is the direction birds are flying — used in the MTR cosine correction.
# MTR is maximized when the transect is perpendicular to this bearing.
#
# For KLWX Jan 12 2026 evening: crows fly roughly SE toward the roost (~150°)
# Set to NA to use the mean flight direction extracted from the vpts u/v components.
BT_FLIGHT_DIRECTION_DEG <- 150

# Altitude integration range (meters AGL). Integrate all heights below this.
# Set to NA to use bioRad default (all available heights).
BT_ALT_MAX_M <- 3000

# ---------------------------------------------------------------------------
# 4. DATA & OUTPUT PATHS
# ---------------------------------------------------------------------------

# Where to cache downloaded pvol files (shared with main pipeline)
BT_DATA_DIR <- "data/data_pvol"

# Where to write all output for this pipeline
BT_OUTPUT_DIR <- "output/biorad_traffic"

# Custom station file needed for TDWR radars (TIAD, TDCA)
BT_CUSTOM_STATION_FILE          <- "data/locations.dat"
BT_CUSTOM_STATION_FILE_FALLBACK <- "data/wsr88d_locations.dat"

# ---------------------------------------------------------------------------
# 5. vol2bird SETTINGS
# ---------------------------------------------------------------------------

# Radar elevation angle to use for PPI (degrees).
# WSR-88D (KLWX, KRLX): 0.5 — TDWR (TIAD, TDCA): 0.3
BT_ELEVATION_DEG <- if (startsWith(BT_RADAR_ID, "T")) 0.3 else 0.5

# ---------------------------------------------------------------------------
# 6. PLOT SETTINGS
# ---------------------------------------------------------------------------

BT_PLOT_WIDTH_IN  <- 10
BT_PLOT_HEIGHT_IN <- 5
BT_PLOT_DPI       <- 180

# ---------------------------------------------------------------------------
# 7. CONVEYOR & STREAM TRAFFIC PARAMETERS
# ---------------------------------------------------------------------------
# Biological & radar scattering assumptions
TRAFFIC_CROW_RCS_CM2          <- 100.0
TRAFFIC_LAYER_THICKNESS_KM    <- 0.150  # 150 meters
TRAFFIC_DIELECTRIC_K2         <- 0.93
TRAFFIC_LAMBDA_C_BAND_CM      <- 5.35   # TDWR C-band (TDCA, TIAD)
TRAFFIC_LAMBDA_S_BAND_CM      <- 10.70  # WSR-88D S-band (KLWX)

# Finish Line & Spatial Geometry
TRAFFIC_FINISH_LINE_DIST_KM   <- 2.0
TRAFFIC_CORRIDOR_WIDTH_KM     <- 1.5
TRAFFIC_MAX_FORECAST_RANGE_KM <- 35.0
TRAFFIC_SCAN_INTERVAL_MINS    <- 6.0

# Flight Speed & Biological Envelope
TRAFFIC_SPEED_DEFAULT_KMPH    <- 38.0
TRAFFIC_SPEED_MIN_KMPH        <- 20.0
TRAFFIC_SPEED_MAX_KMPH        <- 65.0
TRAFFIC_MIN_DBZH              <- 5.0
