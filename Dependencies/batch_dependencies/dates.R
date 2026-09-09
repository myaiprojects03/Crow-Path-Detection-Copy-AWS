# =============================================================================
# dates.R — Season date expansion with winter year-rollover support
# =============================================================================

# Parse a "MM-DD" string into a list(month, day)
parse_mmdd <- function(mmdd) {
  parts <- as.integer(strsplit(mmdd, "-")[[1]])
  list(month = parts[1], day = parts[2])
}

# Build a Date from year + "MM-DD" string
date_from_year_mmdd <- function(year, mmdd) {
  as.Date(paste0(year, "-", mmdd))
}

# ---------------------------------------------------------------------------
# expand_batch_dates()
#
# Returns a vector of Date objects for every calendar day to process,
# handling the case where the season spans Jan 1 (e.g. Nov–Mar).
#
# Winter season logic (start_month > end_month):
#   Season Y = start_year: YYYY-Nov-01  →  (YYYY+1)-Mar-31
#   Season Y = start_year+1: ...
#   The last season begins in end_year, ending in end_year+1.
#
# Same-year season logic (start_month <= end_month):
#   Season Y: YYYY-start → YYYY-end  (all within the same year)
# ---------------------------------------------------------------------------
expand_batch_dates <- function(start_year, end_year, start_mmdd, end_mmdd) {

  s <- parse_mmdd(start_mmdd)
  e <- parse_mmdd(end_mmdd)

  is_winter_wrap <- s$month > e$month   # e.g. Nov(11) > Mar(3) → TRUE

  all_dates <- as.Date(character(0))

  for (yr in start_year:end_year) {

    season_start <- date_from_year_mmdd(yr, start_mmdd)

    if (is_winter_wrap) {
      # Season starts in yr, ends in yr+1
      season_end <- date_from_year_mmdd(yr + 1L, end_mmdd)
    } else {
      # Season is fully within yr
      season_end <- date_from_year_mmdd(yr, end_mmdd)
    }

    # Guard: skip degenerate or impossible ranges
    if (season_start > season_end) next

    day_seq <- seq(season_start, season_end, by = "day")
    all_dates <- c(all_dates, day_seq)
  }

  # Remove duplicates that can arise from consecutive seasons sharing a day
  sort(unique(all_dates))
}
