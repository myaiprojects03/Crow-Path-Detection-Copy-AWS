# install_deps.R
# Script to install R dependencies for the Crow Path Detection project

# 1. SETUP USER LIBRARY
user_lib <- Sys.getenv("R_LIBS_USER")
if (nchar(user_lib) == 0) {
  user_lib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library",
                        paste(R.version$major, substr(R.version$minor, 1, 1), sep = "."))
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

# 2. INSTALL BIOCONDUCTOR MANAGER (Needed for rhdf5)
if (!require("BiocManager", quietly = TRUE)) {
  message("Installing BiocManager...")
  install.packages("BiocManager", repos = "https://cloud.r-project.org", lib = user_lib)
}

# 3. INSTALL RHDF5 (Critical for reading radar files)
if (!require("rhdf5", quietly = TRUE)) {
  message("Installing rhdf5 from Bioconductor...")
  BiocManager::install("rhdf5", update = FALSE, ask = FALSE, lib = user_lib)
}

# 4. INSTALL CRAN PACKAGES
packages <- c("bioRad", "vol2birdR", "ggplot2", "geosphere", "sp", "ggspatial", "prettymapr", "rosm",
              "suncalc", "openxlsx", "aws.s3")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = user_lib)
  } else {
    message(paste("Package already installed:", pkg))
  }
}

invisible(lapply(packages, install_if_missing))

# 5. MISTNET CHECK
if (require("bioRad", quietly = TRUE)) {
  mistnet_installed <- system.file("mistnet_nexrad.pt", package = "vol2birdR") != ""
  if (!mistnet_installed) {
    message("MistNet model not installed. Run bioRad::install_mistnet() manually if needed.")
  } else {
    message("MistNet model is ready.")
  }
}

message("All dependencies (including Bioconductor rhdf5) checked.")
