# =============================================================================
# shirlington_va.R — Shirlington VA Roost Configuration
# =============================================================================
list(
  name                       = "Shirlington VA",
  lat                        = 38.84,
  lon                        = -77.091,
  timezone                   = "US/Eastern",
  radars                     = c("TDCA"),
  radar_id                   = "TDCA",
  # Roost-specific detection overrides
  # Geographic boundaries for map plots (overrides default 30 km limits)
  map_xlim                   = c(-77.38, -77.00),
  map_ylim                   = c(38.68,  38.98),
  CONTRAST_THRESHOLD            = 1.25,
  MAP_DBZH_TRANSPARENT_BELOW    = 2,
  CORRIDOR_DENSITY_THRESHOLD    = 0.04,
  RADAR_SUPPRESS_DIST_KM        = 16.0,
  DETECT_DBZH_MIN               = 9.0,
  MIN_CORRIDOR_PIXELS           = 15L,
  WEAK_STREAM_ANGLE_WINDOW_DEG  = 15,
  MAX_STREAMS_PER_SCAN          = 5L,
  PIPELINE_FOCUS                = "detail"
)


