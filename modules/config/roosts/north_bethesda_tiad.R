# =============================================================================
# north_bethesda_tiad.R — North Bethesda Roost Configuration (TIAD Radar)
# =============================================================================
list(
  name                       = "North Bethesda TIAD",
  lat                        = 39.047,
  lon                        = -77.103,
  timezone                   = "US/Eastern",
  radars                     = c("TIAD"),
  radar_id                   = "TIAD",
  
  # Roost-specific detection overrides
  # Geographic boundaries for map plots (overrides default 30 km limits)
  map_xlim                   = c(-77.45, -76.97),
  map_ylim                   = c(38.95,  39.28),
  CONTRAST_THRESHOLD         = 0.85,
  MAP_DBZH_TRANSPARENT_BELOW = 5,
  CORRIDOR_DENSITY_THRESHOLD = 0.08,
  RADAR_SUPPRESS_DIST_KM     = 8.0,
  DETECT_DBZH_MIN            = 5,
  LOCAL_PROMINENCE_THRESHOLD = 0.40,
  MERGE_GAP_DEG              = 1,
  WEAK_STREAM_ANGLE_WINDOW_DEG   = 15,
  WEAK_STREAM_FRACTION_THRESHOLD = 0.02,
  DISPLAY_MERGE_GAP_DEG      = 12,
  PIPELINE_FOCUS                 = "detail"
)
