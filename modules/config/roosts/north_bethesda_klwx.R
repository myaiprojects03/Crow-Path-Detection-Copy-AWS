# =============================================================================
# north_bethesda_klwx.R — North Bethesda Roost Configuration (KLWX Radar)
# =============================================================================
list(
  name                       = "North Bethesda KLWX",
  lat                        = 39.047,
  lon                        = -77.103,
  timezone                   = "US/Eastern",
  radars                     = c("KLWX"),
  radar_id                   = "KLWX",
  
  # Roost-specific detection overrides
  # Geographic boundaries for map plots (overrides default 30 km limits)
  map_xlim                   = c(-77.45, -76.97),
  map_ylim                   = c(38.95,  39.28),
  CONTRAST_THRESHOLD         = 1.25,
  DETECT_DBZH_MIN            = 9.0,
  MIN_CORRIDOR_PIXELS        = 10L,
  MIN_CONTIGUOUS_BINS        = 3L,
  MAP_DBZH_TRANSPARENT_BELOW = 5,
  CORRIDOR_DENSITY_THRESHOLD = 0.01,
  RADAR_SUPPRESS_DIST_KM     = 16.0,
  LOCAL_PROMINENCE_THRESHOLD = 0.30,
  RUN_START_BIN_MAX          = 35L,
  MAX_GAP_BINS               = 35L,
  EXTENT_NEIGHBOR_DEG        = 12L,
  MERGE_GAP_DEG              = 1,
  WEAK_STREAM_ANGLE_WINDOW_DEG   = 12,
  WEAK_STREAM_FRACTION_THRESHOLD = 0.02,
  DISPLAY_MERGE_GAP_DEG      = 10,
  PIPELINE_FOCUS                 = "detail"
)


