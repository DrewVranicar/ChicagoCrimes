# R/run_pipeline.R
library(data.table)
library(lubridate)
library(jsonlite)

source("R/download_data.R")
source("R/create_csv_datasets.R")
source("R/create_json_datasets.R")
source("R/metadata.R")

OUTPUT_DIR <- "data"
MAX_RECORDS <- NULL   # set to 50000 for testing

options(timeout = 300)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

run_full_pipeline <- function(
  output_dir = OUTPUT_DIR,
  create_json = TRUE,
  max_records = MAX_RECORDS
) {

  cat("\n=== CHICAGO CRIME DATA PIPELINE ===\n")
  start_time <- Sys.time()

  timeseries_file <- file.path(output_dir, "chicago_crimes_timeseries.csv")

  download_timeseries_data(
    output_file = timeseries_file,
    max_records = max_records
  )

  create_csv_datasets(timeseries_file, output_dir)

  if (create_json) {
    create_json_datasets(timeseries_file, output_dir)
  }

  generate_metadata(timeseries_file, output_dir)

  cat("\nPIPELINE COMPLETE\n")
  cat("Duration:",
      round(difftime(Sys.time(), start_time, units = "mins"), 2),
      "minutes\n")
}
