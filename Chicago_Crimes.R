# Chicago Crime Data - AUTOMATED UPDATE PIPELINE (PRODUCTION MODE)
# Downloads ALL Chicago crime data safely using pagination

library(data.table)
library(lubridate)
library(jsonlite)

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "data"
API_LIMIT <- 50000
MAX_RECORDS <- NULL   # NULL = download ALL data

options(timeout = 300) # CI-safe timeout

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# =============================================================================
# MAIN PIPELINE
# =============================================================================

run_full_pipeline <- function(output_dir = OUTPUT_DIR,
                              create_json = TRUE,
                              max_records = MAX_RECORDS) {

  cat("\n=== CHICAGO CRIME DATA PIPELINE (PRODUCTION MODE) ===\n")
  cat("Max records:", ifelse(is.null(max_records), "ALL", max_records), "\n")
  cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  start_time <- Sys.time()

  # STEP 1: DOWNLOAD
  cat("STEP 1: DOWNLOADING DATA\n")
  timeseries_file <- file.path(output_dir, "chicago_crimes_timeseries.csv")

  download_timeseries_data(
    output_file = timeseries_file,
    max_records = max_records
  )

  # STEP 2: CSV AGGREGATIONS
  cat("\nSTEP 2: CREATING CSV DATASETS\n")
  create_viz_datasets(timeseries_file, output_dir)

  # STEP 3: JSON FILES
  if (create_json) {
    cat("\nSTEP 3: CREATING JSON DATASETS\n")
    create_json_datasets(timeseries_file, output_dir)
  }

  # STEP 4: METADATA
  cat("\nSTEP 4: GENERATING METADATA\n")
  generate_metadata(timeseries_file, output_dir)

  duration <- round(
    as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2
  )

  cat("\nPIPELINE COMPLETE\n")
  cat("Duration:", duration, "minutes\n")
  cat("Output directory:", output_dir, "\n\n")
}

# =============================================================================
# DOWNLOAD FUNCTION (FULL PAGINATION, CI-SAFE)
# =============================================================================

download_timeseries_data <- function(output_file, max_records = NULL) {

  base_url <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv"
  select_cols <- "?$select=date,primary_type,arrest,domestic"

  offset <- 0
  total_downloaded <- 0
  first_chunk <- TRUE

  repeat {

    url <- paste0(
      base_url,
      select_cols,
      "&$limit=", format(API_LIMIT, scientific = FALSE),
      "&$offset=", format(offset, scientific = FALSE)
    )

    cat("  Downloading chunk at offset",
        format(offset, big.mark = ","), "...")

    success <- tryCatch({

      chunk <- fread(url, showProgress = FALSE)

      if (nrow(chunk) == 0) {
        cat(" Done!\n")
        return(FALSE)
      }

      # Clean immediately
      chunk[, date := ymd_hms(date, quiet = TRUE)]
      chunk[, arrest := arrest == "true"]
      chunk[, domestic := domestic == "true"]
      chunk[, primary_type := trimws(primary_type)]

      fwrite(chunk, output_file, append = !first_chunk)

      total_downloaded <- total_downloaded + nrow(chunk)
      cat(" ✓", format(nrow(chunk), big.mark = ","), "records\n")

      first_chunk <- FALSE
      offset <- offset + API_LIMIT

      if (!is.null(max_records) &&
          total_downloaded >= max_records) return(FALSE)

      if (nrow(chunk) < API_LIMIT) return(FALSE)

      Sys.sleep(0.4)
      TRUE

    }, error = function(e) {
      cat("\n  ⚠ Download failed at offset",
          format(offset, big.mark = ","), "\n")
      cat("  Reason:", e$message, "\n")
      FALSE
    })

    if (!success) break
  }

  cat("\n  ✓ Total downloaded:",
      format(total_downloaded, big.mark = ","), "records\n")
  cat("  ✓ File size:",
      round(file.size(output_file) / 1024^2, 1), "MB\n")

  return(total_downloaded)
}

# =============================================================================
# CSV DATASETS
# =============================================================================

create_viz_datasets <- function(data_file, output_dir) {

  dt <- fread(data_file, showProgress = FALSE)

  dt[, year_month := floor_date(date, "month")]
  dt[, year := year(date)]
  dt[, hour := hour(date)]
  dt[, wday := wday(date, label = TRUE, abbr = FALSE)]

  monthly <- dt[, .N, by = year_month][order(year_month)]
  setnames(monthly, c("date", "crime_count"))
  fwrite(monthly, file.path(output_dir, "monthly_total.csv"))

  top_types <- dt[, .N, by = primary_type][order(-N)][1:10]$primary_type
  monthly_by_type <- dt[primary_type %in% top_types,
                        .N,
                        by = .(year_month, primary_type)]
  setnames(monthly_by_type, c("date", "crime_type", "crime_count"))
  fwrite(monthly_by_type,
         file.path(output_dir, "monthly_by_type.csv"))

  cutoff <- max(dt$date) - years(2)
  daily <- dt[date >= cutoff,
              .N,
              by = .(date = as.Date(date))]
  setnames(daily, c("date", "crime_count"))
  fwrite(daily, file.path(output_dir, "daily_recent.csv"))

  yearly <- dt[, .N, by = year][order(year)]
  setnames(yearly, c("year", "crime_count"))
  fwrite(yearly, file.path(output_dir, "yearly_total.csv"))

  cat("  ✓ CSV datasets created\n")
}

# =============================================================================
# JSON DATASETS
# =============================================================================

create_json_datasets <- function(data_file, output_dir) {

  dt <- fread(data_file, showProgress = FALSE)

  dt[, year_month := floor_date(date, "month")]
  dt[, year := year(date)]

  monthly <- dt[, .N, by = year_month][order(year_month)]
  write_json(
    list(labels = format(monthly$year_month, "%Y-%m"),
         data = monthly$N),
    file.path(output_dir, "monthly_total.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  yearly <- dt[, .N, by = year][order(year)]
  write_json(
    list(labels = yearly$year, data = yearly$N),
    file.path(output_dir, "yearly_total.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  cat("  ✓ JSON datasets created\n")
}

# =============================================================================
# METADATA
# =============================================================================

generate_metadata <- function(data_file, output_dir) {

  dt <- fread(data_file, showProgress = FALSE)

  top_crimes <- head(dt[, .N, by = primary_type][order(-N)], 10)

  metadata <- list(
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    total_records = nrow(dt),
    date_range = list(
      start = format(min(dt$date), "%Y-%m-%d"),
      end = format(max(dt$date), "%Y-%m-%d")
    ),
    top_crimes = lapply(1:nrow(top_crimes), function(i) {
      list(
        crime_type = top_crimes$primary_type[i],
        count = top_crimes$N[i]
      )
    })
  )

  write_json(
    metadata,
    file.path(output_dir, "metadata.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  cat("  ✓ Metadata written\n")
}

# =============================================================================
# ENTRY POINT
# =============================================================================

cat("\nPRODUCTION MODE ENABLED\n")
cat("Run with:\n")
cat("  run_full_pipeline()\n\n")
