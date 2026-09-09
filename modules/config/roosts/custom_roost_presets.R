# =============================================================================
# custom_roost_presets.R — Consolidation of all custom roost presets
# =============================================================================

ROOST_PRESETS[["WVCH Charleston"]] <- list(
  name                       = "WVCH Charleston",
  lat                        = 38.37,
  lon                        = -81.68,
  timezone                   = "US/Eastern",
  radars                     = c("KRLX"),
  radar_id                   = "KRLX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["IADM Des Moines"]] <- list(
  name                       = "IADM Des Moines",
  lat                        = 41.6004,
  lon                        = -93.7037,
  timezone                   = "US/Central",
  radars                     = c("KDMX"),
  radar_id                   = "KDMX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Sacramento, CA, USA"]] <- list(
  name                       = "Sacramento, CA, USA",
  lat                        = 38.57813,
  lon                        = -121.49442,
  timezone                   = "US/Pacific",
  radars                     = c("KDAX"),
  radar_id                   = "KDAX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["PAPI Pittsburgh"]] <- list(
  name                       = "PAPI Pittsburgh",
  lat                        = 40.5091,
  lon                        = -79.9703,
  timezone                   = "US/Eastern",
  radars                     = c("KPBZ"),
  radar_id                   = "KPBZ"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["NYPL Plattsburgh"]] <- list(
  name                       = "NYPL Plattsburgh",
  lat                        = 44.6496,
  lon                        = -73.4737,
  timezone                   = "US/Eastern",
  radars                     = c("KCXX"),
  radar_id                   = "KCXX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["ORPD Portland"]] <- list(
  name                       = "ORPD Portland",
  lat                        = 45.5167,
  lon                        = -122.7,
  timezone                   = "US/Pacific",
  radars                     = c("KRTX"),
  radar_id                   = "KRTX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["CASD San Diego"]] <- list(
  name                       = "CASD San Diego",
  lat                        = 32.6476,
  lon                        = -117.1234,
  timezone                   = "US/Pacific",
  radars                     = c("KNKX"),
  radar_id                   = "KNKX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Chula Vista, CA (Gibson)"]] <- list(
  name                       = "Chula Vista, CA (Gibson)",
  lat                        = 32.64005,
  lon                        = -117.0842,
  timezone                   = "US/Pacific",
  radars                     = c("KNKX"),
  radar_id                   = "KNKX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["CAOC Orange County (coastal)"]] <- list(
  name                       = "CAOC Orange County (coastal)",
  lat                        = 33.6725,
  lon                        = -117.9453,
  timezone                   = "US/Pacific",
  radars                     = c("KSOX"),
  radar_id                   = "KSOX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Troy, NY (Gibson)"]] <- list(
  name                       = "Troy, NY (Gibson)",
  lat                        = 42.72841,
  lon                        = -73.69179,
  timezone                   = "US/Eastern",
  radars                     = c("KENX"),
  radar_id                   = "KENX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Trenton, NJ"]] <- list(
  name                       = "Trenton, NJ",
  lat                        = 40.22018,
  lon                        = -74.76423,
  timezone                   = "US/Eastern",
  radars                     = c("KDIX"),
  radar_id                   = "KDIX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["MIAA Ann Arbor"]] <- list(
  name                       = "MIAA Ann Arbor",
  lat                        = 42.3172,
  lon                        = -83.7781,
  timezone                   = "US/Eastern",
  radars                     = c("KDTX"),
  radar_id                   = "KDTX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["SCAB Ace Basin"]] <- list(
  name                       = "SCAB Ace Basin",
  lat                        = 32.6126,
  lon                        = -80.4813,
  timezone                   = "US/Eastern",
  radars                     = c("KCLX"),
  radar_id                   = "KCLX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Bothell, WA"]] <- list(
  name                       = "Bothell, WA",
  lat                        = 47.76011,
  lon                        = -122.20545,
  timezone                   = "US/Pacific",
  radars                     = c("KATX"),
  radar_id                   = "KATX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["OHCC Clark County"]] <- list(
  name                       = "OHCC Clark County",
  lat                        = 39.9214,
  lon                        = -83.7798,
  timezone                   = "US/Eastern",
  radars                     = c("KILN"),
  radar_id                   = "KILN"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Springfield, OH"]] <- list(
  name                       = "Springfield, OH",
  lat                        = 39.92344,
  lon                        = -83.80987,
  timezone                   = "US/Eastern",
  radars                     = c("KILN"),
  radar_id                   = "KILN"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["WVWH Wheeling"]] <- list(
  name                       = "WVWH Wheeling",
  lat                        = 40.0966,
  lon                        = -80.6618,
  timezone                   = "US/Eastern",
  radars                     = c("KPBZ"),
  radar_id                   = "KPBZ"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Baltimore, MD"]] <- list(
  name                       = "Baltimore, MD",
  lat                        = 39.2905,
  lon                        = -76.61041,
  timezone                   = "US/Eastern",
  radars                     = c("TDCA"),
  radar_id                   = "TDCA"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["OHYO Youngstown"]] <- list(
  name                       = "OHYO Youngstown",
  lat                        = 41.0467,
  lon                        = -80.6983,
  timezone                   = "US/Eastern",
  radars                     = c("KPBZ"),
  radar_id                   = "KPBZ"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["WVRC Raleigh County"]] <- list(
  name                       = "WVRC Raleigh County",
  lat                        = 37.7667,
  lon                        = -81.2895,
  timezone                   = "US/Eastern",
  radars                     = c("KRLX"),
  radar_id                   = "KRLX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["MAAN Andover"]] <- list(
  name                       = "MAAN Andover",
  lat                        = 42.635,
  lon                        = -71.1419,
  timezone                   = "US/Eastern",
  radars                     = c("KBOX"),
  radar_id                   = "KBOX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["CAOA Oakland"]] <- list(
  name                       = "CAOA Oakland",
  lat                        = 37.8146,
  lon                        = -122.233,
  timezone                   = "US/Pacific",
  radars                     = c("KMUX"),
  radar_id                   = "KMUX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["FLEC Econlockhatchee"]] <- list(
  name                       = "FLEC Econlockhatchee",
  lat                        = 28.7094,
  lon                        = -81.1467,
  timezone                   = "US/Eastern",
  radars                     = c("KMLB"),
  radar_id                   = "KMLB"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["QCSJ St-Jean-sur-le-Richelieu"]] <- list(
  name                       = "QCSJ St-Jean-sur-le-Richelieu",
  lat                        = 45.3091,
  lon                        = -73.243,
  timezone                   = "US/Eastern",
  radars                     = c("KCXX"),
  radar_id                   = "KCXX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Auburn, NY (Gibson)"]] <- list(
  name                       = "Auburn, NY (Gibson)",
  lat                        = 42.93173,
  lon                        = -76.56605,
  timezone                   = "US/Eastern",
  radars                     = c("KBGM"),
  radar_id                   = "KBGM"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Rochester, NY"]] <- list(
  name                       = "Rochester, NY",
  lat                        = 43.15658,
  lon                        = -77.60885,
  timezone                   = "US/Eastern",
  radars                     = c("KBUF"),
  radar_id                   = "KBUF"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["WAKA Kent-Auburn"]] <- list(
  name                       = "WAKA Kent-Auburn",
  lat                        = 47.3598,
  lon                        = -122.1787,
  timezone                   = "US/Pacific",
  radars                     = c("KATX"),
  radar_id                   = "KATX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["OHMA Mansfield"]] <- list(
  name                       = "OHMA Mansfield",
  lat                        = 40.7042,
  lon                        = -82.5486,
  timezone                   = "US/Eastern",
  radars                     = c("KCLE"),
  radar_id                   = "KCLE"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["ONSL St. Clair N.W.A."]] <- list(
  name                       = "ONSL St. Clair N.W.A.",
  lat                        = 42.3667,
  lon                        = -82.35,
  timezone                   = "US/Eastern",
  radars                     = c("KDTX"),
  radar_id                   = "KDTX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["CTHA Hartford"]] <- list(
  name                       = "CTHA Hartford",
  lat                        = 41.766,
  lon                        = -72.6727,
  timezone                   = "US/Eastern",
  radars                     = c("KOKX"),
  radar_id                   = "KOKX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["INTH Terre Haute"]] <- list(
  name                       = "INTH Terre Haute",
  lat                        = 39.4165,
  lon                        = -87.407,
  timezone                   = "US/Central",
  radars                     = c("KIND"),
  radar_id                   = "KIND"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["NYDC Dutchess County"]] <- list(
  name                       = "NYDC Dutchess County",
  lat                        = 41.687,
  lon                        = -73.7933,
  timezone                   = "US/Eastern",
  radars                     = c("KENX"),
  radar_id                   = "KENX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["QCGR Granby"]] <- list(
  name                       = "QCGR Granby",
  lat                        = 45.3937,
  lon                        = -72.71,
  timezone                   = "US/Eastern",
  radars                     = c("KCXX"),
  radar_id                   = "KCXX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["FLZE Zellwood-Mount Dora"]] <- list(
  name                       = "FLZE Zellwood-Mount Dora",
  lat                        = 28.7309,
  lon                        = -81.6062,
  timezone                   = "US/Eastern",
  radars                     = c("KMLB"),
  radar_id                   = "KMLB"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Lebanon, NH (Gibson)"]] <- list(
  name                       = "Lebanon, NH (Gibson)",
  lat                        = 43.64229,
  lon                        = -72.25176,
  timezone                   = "US/Eastern",
  radars                     = c("KCXX"),
  radar_id                   = "KCXX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["Middle Fork River Rankin, IL"]] <- list(
  name                       = "Middle Fork River Rankin, IL",
  lat                        = 40.46504,
  lon                        = -87.89642,
  timezone                   = "US/Central",
  radars                     = c("KILX"),
  radar_id                   = "KILX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["PALA Lancaster"]] <- list(
  name                       = "PALA Lancaster",
  lat                        = 39.9921,
  lon                        = -76.3614,
  timezone                   = "US/Eastern",
  radars                     = c("TIAD"),
  radar_id                   = "TIAD"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["IAKE Keokuk"]] <- list(
  name                       = "IAKE Keokuk",
  lat                        = 40.4726,
  lon                        = -91.4581,
  timezone                   = "US/Central",
  radars                     = c("KDVN"),
  radar_id                   = "KDVN"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["ONWS Woodstock"]] <- list(
  name                       = "ONWS Woodstock",
  lat                        = 43.1725,
  lon                        = -80.7124,
  timezone                   = "US/Eastern",
  radars                     = c("KBUF"),
  radar_id                   = "KBUF"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)

ROOST_PRESETS[["ONOH Ottawa-Gatineau"]] <- list(
  name                       = "ONOH Ottawa-Gatineau",
  lat                        = 45.4247,
  lon                        = -75.6997,
  timezone                   = "US/Eastern",
  radars                     = c("KTYX"),
  radar_id                   = "KTYX"
  
  # Roost-specific detection overrides (Optional)
  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3
  # CONTRAST_THRESHOLD         = 0.80,
  # MAP_DBZH_TRANSPARENT_BELOW = 5,
  # CORRIDOR_DENSITY_THRESHOLD = 0.05
)
