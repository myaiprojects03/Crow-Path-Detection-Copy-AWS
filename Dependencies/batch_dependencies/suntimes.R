# =============================================================================
# suntimes.R — Sunrise / sunset calculation using the suncalc package
#
# suncalc computes astronomical solar times purely from lat/lon/date —
# no internet connection or API key required, and works for any date
# back to the 1800s (well beyond NEXRAD's 1991 start).
# =============================================================================

# ---------------------------------------------------------------------------
# get_event_time()
#
# Returns the UTC POSIXct time of "sunset" or "sunrise" for a given
# calendar date at the roost location.
# ---------------------------------------------------------------------------
get_event_time <- function(date, lat, lon, event = "sunset") {

  valid_events <- c("sunrise", "sunset", "solarNoon",
                    "dawn", "dusk", "nauticalDawn", "nauticalDusk")
  if (!event %in% valid_events) {
    stop("Unknown event '", event, "'. Choose from: ",
         paste(valid_events, collapse = ", "))
  }

  result <- suncalc::getSunlightTimes(
    date = as.Date(date),
    lat  = lat,
    lon  = lon,
    keep = event,
    tz   = "UTC"
  )

  event_time <- result[[event]]

  if (is.na(event_time)) {
    warning("Could not compute '", event, "' for ", date,
            " at lat=", lat, " lon=", lon,
            ". Skipping this date.")
  }

  event_time
}

# ---------------------------------------------------------------------------
# get_scan_window()
#
# Applies start/end minute offsets to the solar event time and returns
# the UTC scan window plus the raw event time (for logging/output).
#
# Returns a named list:
#   $start      — POSIXct UTC start of the download/processing window
#   $end        — POSIXct UTC end   of the download/processing window
#   $event_time — POSIXct UTC time  of the solar event itself
# ---------------------------------------------------------------------------
get_scan_window <- function(date, lat, lon, event,
                            start_offset_min, end_offset_min) {

  event_time <- get_event_time(date, lat, lon, event)

  if (is.na(event_time)) {
    return(list(start = NA, end = NA, event_time = NA))
  }

  list(
    start      = event_time + as.numeric(start_offset_min) * 60,
    end        = event_time + as.numeric(end_offset_min)   * 60,
    event_time = event_time
  )
}
