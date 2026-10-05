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
  CONTRAST_THRESHOLD             = 1.25,
  DETECT_DBZH_MIN                = 9.0,
  MIN_CORRIDOR_PIXELS            = 12L,
  MIN_CONTIGUOUS_BINS            = 3L,
  MAP_DBZH_TRANSPARENT_BELOW     = 12,
  CORRIDOR_DENSITY_THRESHOLD     = 0.04,
  RADAR_SUPPRESS_DIST_KM         = 7.0,
  LOCAL_PROMINENCE_THRESHOLD     = 1.05,
  RUN_START_BIN_MAX              = 25L,
  MAX_GAP_BINS                   = 35L,
  EXTENT_NEIGHBOR_DEG            = 5L,
  MERGE_GAP_DEG                  = 1,
  WEAK_STREAM_ANGLE_WINDOW_DEG   = 15,
  WEAK_STREAM_FRACTION_THRESHOLD = 0.05,
  DISPLAY_MERGE_GAP_DEG          = 12,
  MAX_STREAMS_PER_SCAN           = 5L,
  PIPELINE_FOCUS                 = "detail"
)

