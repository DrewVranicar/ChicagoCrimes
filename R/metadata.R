# R/metadata.R
library(data.table)
library(jsonlite)

generate_metadata <- function(data_file, output_dir) {

  dt <- fread(data_file)

  meta <- list(
    last_updated = Sys.time(),
    total_records = nrow(dt),
    date_range = list(
      start = min(dt$date),
      end = max(dt$date)
    )
  )

  write_json(
    meta,
    file.path(output_dir, "metadata.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  cat("✓ Metadata written\n")
}
