# =============================================================================
# bootstrap.R — Environment setup and module loading
# =============================================================================

load_required_packages <- function() {
  packages <- c("bioRad", "vol2birdR", "ggplot2", "geosphere", "sp", "ggspatial", "prettymapr", "rosm")
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }
}

source_project_modules <- function() {
  # Source all .R files in the modules directory except bootstrap.R and pipeline.R
  # (pipeline.R is sourced separately in main.R/app.R)
  module_files <- list.files("modules", recursive = TRUE, pattern = "\\.R$", full.names = TRUE)
  
  # Normalize paths for Windows/Unix compatibility
  module_files <- normalizePath(module_files, winslash = "/", mustWork = TRUE)
  
  # Exclude bootstrap, pipeline, custom presets to avoid circularity, double sourcing, or automatic execution
  exclude <- c(
    "modules/pipeline/bootstrap.R",
    "modules/pipeline/pipeline.R",
    "modules/config/roosts/custom_roost_presets.R"
  )
  
  # Ensure the exclude list has full normalized paths
  exclude_normalized <- normalizePath(file.path(getwd(), exclude), winslash = "/", mustWork = FALSE)
  
  module_files <- module_files[!(module_files %in% exclude_normalized)]
  
  for (f in module_files) {
    source(f)
  }
}
