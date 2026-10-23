# =============================================================================
# shirlington_va.R — Shirlington VA Roost Configuration
# =============================================================================
list(
  name                           = "Shirlington VA",
  lat                            = 38.84,
  lon                            = -77.091,
  timezone                       = "US/Eastern",
  radars                         = c("TDCA"),
  radar_id                       = "TDCA",
  
  # Roost-specific detection overrides
  # Geographic boundaries for map plots (overrides default 30 km limits)
  map_xlim                       = c(-77.38, -77.00),
  map_ylim                       = c(38.68,  38.98),
  MAP_ZOOM                       = 1,
  MAP_USE_TILES                  = TRUE,
  CONTRAST_THRESHOLD             = 0.80,
  DETECT_DBZH_MIN                = 8.0,
  MIN_CORRIDOR_PIXELS            = 25,
  MIN_CONTIGUOUS_BINS            = 5L,
  MAP_DBZH_TRANSPARENT_BELOW     = 9,
  CORRIDOR_DENSITY_THRESHOLD     = 0.04,
  RADAR_SUPPRESS_DIST_KM         = 7.0,
  LOCAL_PROMINENCE_THRESHOLD     = 1.10,
  RUN_START_BIN_MAX              = 8L,
  MAX_GAP_BINS                   = 1L,
  EXTENT_NEIGHBOR_DEG            = 5L,
  MERGE_GAP_DEG                  = 6,
  WEAK_STREAM_ANGLE_WINDOW_DEG   = 30,
  WEAK_STREAM_FRACTION_THRESHOLD = 0.15,
  DISPLAY_MERGE_GAP_DEG          = 15,
  MAX_STREAMS_PER_SCAN           = 5L,
  PIPELINE_FOCUS                 = "detail",
  DISTANCE_WEIGHT_SCORING        = TRUE,
  DISTANCE_WEIGHT_COMPASS        = TRUE,
  DETECTION_MIN_DIST_KM          = 2.8
)

