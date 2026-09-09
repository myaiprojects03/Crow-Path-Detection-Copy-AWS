# =============================================================================
# generate_roost_configs.R — Auto-generate roost config files from Excel locations
# =============================================================================
library(openxlsx)

# Read matched user locations spreadsheet
xlsx_file <- "modules/config/matched_user_locations 11-06-2026 (1).xlsx"
if (!file.exists(xlsx_file)) {
  stop("Excel locations file not found: ", xlsx_file)
}

df <- read.xlsx(xlsx_file)

# Helper function to guess US timezone based on longitude bounds
get_timezone <- function(lon, name) {
  if (lon < -115) {
    return("US/Pacific")
  } else if (lon < -102) {
    return("US/Mountain")
  } else if (lon < -87) {
    return("US/Central")
  } else {
    return("US/Eastern")
  }
}

# Ensure output directory exists and clean old generated files
out_dir <- "modules/config/roosts"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

built_in_files <- c("north_bethesda_klwx.R", "north_bethesda_tiad.R", "shirlington_va.R")
all_files <- list.files(out_dir, full.names = TRUE)
to_delete <- all_files[!basename(all_files) %in% built_in_files]
if (length(to_delete) > 0) {
  unlink(to_delete)
  message("Cleaned up ", length(to_delete), " separate roost configuration files.")
}

message("Generating roost configurations to custom_roost_presets.R...")

# List of locations to skip because they are already built-in
skip_names <- c("North Bethesda", "Shirlington, Arlington, VA")

custom_file_lines <- c(
  "# =============================================================================",
  "# custom_roost_presets.R — Consolidation of all custom roost presets",
  "# ============================================================================="
)

generated_count <- 0

for (i in 1:nrow(df)) {
  loc_name <- trimws(df$name[i])
  
  # Check if we should skip this location
  if (loc_name %in% skip_names) {
    message("Skipping built-in roost: ", loc_name)
    next
  }
  
  lat <- df$lat[i]
  lon <- df$lon[i]
  radar <- trimws(df$nearest_radar[i])
  
  # Determine timezone
  tz <- get_timezone(lon, loc_name)
  
  # Generate R preset list block
  preset_block <- c(
    "",
    paste0("ROOST_PRESETS[[\"", loc_name, "\"]] <- list("),
    paste0("  name                       = \"", loc_name, "\","),
    paste0("  lat                        = ", round(lat, 5), ","),
    paste0("  lon                        = ", round(lon, 5), ","),
    paste0("  timezone                   = \"", tz, "\","),
    paste0("  radars                     = c(\"", radar, "\"),"),
    paste0("  radar_id                   = \"", radar, "\""),
    "  ",
    "  # Roost-specific detection overrides (Optional)",
    "  # elevation                  = 0.5, # Defaults: WSR-88D (Starts with K) -> 0.5, TDWR (Starts with T) -> 0.3",
    "  # CONTRAST_THRESHOLD         = 0.80,",
    "  # MAP_DBZH_TRANSPARENT_BELOW = 5,",
    "  # CORRIDOR_DENSITY_THRESHOLD = 0.05",
    ")"
  )
  
  custom_file_lines <- c(custom_file_lines, preset_block)
  generated_count <- generated_count + 1
}

# Write custom presets file
writeLines(custom_file_lines, con = "modules/config/roosts/custom_roost_presets.R")

message("Successfully consolidated ", generated_count, " roost presets in roosts/custom_roost_presets.R.")
