# =============================================================================
# batch_config.R — Batch Operations Configurations
# =============================================================================

#Default run set to batch for the AWS run.
RUN_MODE <- "batch" 
ACTIVE_ROOST_PRESET <- "Shirlington VA" # e.g., "North Bethesda KLWX", "North Bethesda TIAD", "Shirlington VA", 
                                             # or any custom preset in custom_roost_presets.R 
BATCH_START_MMDD         <- "11-01" # Format: MM-DD
BATCH_END_MMDD           <- "11-02" # Format: MM-DD
BATCH_EVENT              <- "sunset"
BATCH_START_YEAR         <- 2025L
BATCH_END_YEAR           <- 2025L
BATCH_START_OFFSET_MIN   <- -90L
BATCH_END_OFFSET_MIN     <- +30L
BATCH_MAX_FILES_PER_DATE <- 3000L
BATCH_OUTPUT_FILE        <- "output/batch_results.xlsx"
BATCH_OVERWRITE_OUTPUT   <- TRUE

# Automated Storage & Cleanup Settings (For AWS Runs)
CLEAN_OUTPUT_ON_START <- TRUE
CLEAN_PVOL_AFTER_RUN  <- TRUE
PRESERVE_OUTPUT_FILES <- c("batch_results.xlsx")