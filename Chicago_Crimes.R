# Chicago Crime Data - AUTOMATED UPDATE PIPELINE FOR GITHUB PAGES
# This script downloads, cleans, and creates visualization-ready datasets
# Designed to run on a schedule (GitHub Actions, cron job, etc.)

library(data.table)
library(lubridate)
library(jsonlite)  # For creating JSON output for web visualizations

# Configuration
OUTPUT_DIR <- "data"  # Where to save output files for GitHub Pages
API_LIMIT <- 50000
MAX_RECORDS <- NULL  # NULL = all data, set number for testing

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# =============================================================================
# MAIN PIPELINE FUNCTION - Run this for automated updates
# =============================================================================

run_full_pipeline <- function(output_dir = OUTPUT_DIR, 
                              create_json = TRUE,
                              max_records = MAX_RECORDS) {
  
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  CHICAGO CRIME DATA - AUTOMATED UPDATE PIPELINE             ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  start_time <- Sys.time()
  
  # Step 1: Download data
  cat("═══ STEP 1: DOWNLOADING DATA ═══\n")
  timeseries_file <- file.path(output_dir, "chicago_crimes_timeseries.csv")
  download_timeseries_data(output_file = timeseries_file, 
                           max_records = max_records)
  
  # Step 2: Create CSV aggregations
  cat("\n═══ STEP 2: CREATING CSV AGGREGATIONS ═══\n")
  create_viz_datasets(data_file = timeseries_file, output_dir = output_dir)
  
  # Step 3: Create JSON files for web visualizations
  if (create_json) {
    cat("\n═══ STEP 3: CREATING JSON FILES FOR WEB ═══\n")
    create_json_datasets(data_file = timeseries_file, output_dir = output_dir)
  }
  
  # Step 4: Generate metadata
  cat("\n═══ STEP 4: GENERATING METADATA ═══\n")
  generate_metadata(data_file = timeseries_file, output_dir = output_dir)
  
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)
  
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  PIPELINE COMPLETE!                                          ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  cat("Total time:", duration, "minutes\n")
  cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("\nAll files saved to:", output_dir, "\n")
  cat("Ready to commit and push to GitHub Pages!\n\n")
}

# =============================================================================
# DOWNLOAD FUNCTION
# =============================================================================

download_timeseries_data <- function(output_file, max_records = NULL) {
  
  base_url <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv"
  select_cols <- "?$select=date,primary_type,arrest,domestic"
  offset <- 0
  total_downloaded <- 0
  first_chunk <- TRUE
  
  repeat {
    url <- paste0(base_url, select_cols, "&$limit=", API_LIMIT, "&$offset=", offset)
    
    cat("  Downloading chunk at offset", format(offset, big.mark = ","), "...")
    
    tryCatch({
      chunk <- fread(url, showProgress = FALSE)
      
      if (nrow(chunk) == 0) {
        cat(" Done!\n")
        break
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
      
      if (!is.null(max_records) && total_downloaded >= max_records) break
      if (nrow(chunk) < API_LIMIT) break
      
      Sys.sleep(0.3)
      
    }, error = function(e) {
      cat("\n  Error:", e$message, "\n")
      break
    })
  }
  
  cat("\n  ✓ Total downloaded:", format(total_downloaded, big.mark = ","), "records\n")
  cat("  ✓ File size:", round(file.size(output_file) / 1024^2, 1), "MB\n")
  
  return(total_downloaded)
}

# =============================================================================
# CREATE CSV DATASETS FOR VISUALIZATION
# =============================================================================

create_viz_datasets <- function(data_file, output_dir = OUTPUT_DIR) {
  
  cat("  Loading data...\n")
  dt <- fread(data_file, showProgress = FALSE)
  
  if (is.character(dt$date)) {
    dt[, date := ymd_hms(date, quiet = TRUE)]
  }
  
  dt[, year_month := floor_date(date, "month")]
  dt[, year := year(date)]
  dt[, hour := hour(date)]
  dt[, wday := wday(date, label = TRUE, abbr = FALSE)]
  
  # 1. Monthly totals
  cat("  Creating monthly aggregation...\n")
  monthly <- dt[, .N, by = year_month][order(year_month)]
  setnames(monthly, c("date", "crime_count"))
  fwrite(monthly, file.path(output_dir, "monthly_total.csv"))
  
  # 2. Monthly by crime type (top 10)
  cat("  Creating monthly by crime type...\n")
  top_types <- dt[, .N, by = primary_type][order(-N)][1:10]$primary_type
  monthly_by_type <- dt[primary_type %in% top_types, 
                        .N, 
                        by = .(year_month, primary_type)][order(year_month)]
  setnames(monthly_by_type, c("date", "crime_type", "crime_count"))
  fwrite(monthly_by_type, file.path(output_dir, "monthly_by_type.csv"))
  
  # 3. Daily (last 2 years)
  cat("  Creating daily aggregation...\n")
  cutoff_date <- max(dt$date, na.rm = TRUE) - years(2)
  daily <- dt[date >= cutoff_date, 
              .N, 
              by = .(date = as.Date(date))][order(date)]
  setnames(daily, c("date", "crime_count"))
  fwrite(daily, file.path(output_dir, "daily_recent.csv"))
  
  # 4. Yearly by crime type
  cat("  Creating yearly by crime type...\n")
  yearly_by_type <- dt[, .N, by = .(year, primary_type)][order(year, -N)]
  setnames(yearly_by_type, c("year", "crime_type", "crime_count"))
  fwrite(yearly_by_type, file.path(output_dir, "yearly_by_type.csv"))
  
  # 5. Hourly pattern
  cat("  Creating hourly patterns...\n")
  hourly <- dt[, .N, by = hour][order(hour)]
  setnames(hourly, c("hour", "crime_count"))
  fwrite(hourly, file.path(output_dir, "hourly_pattern.csv"))
  
  # 6. Day of week pattern
  cat("  Creating day-of-week patterns...\n")
  wday_pattern <- dt[, .N, by = wday]
  setnames(wday_pattern, c("day_of_week", "crime_count"))
  fwrite(wday_pattern, file.path(output_dir, "dayofweek_pattern.csv"))
  
  # 7. Year over year comparison (for trend analysis)
  cat("  Creating year-over-year comparison...\n")
  yearly <- dt[, .N, by = year][order(year)]
  setnames(yearly, c("year", "crime_count"))
  fwrite(yearly, file.path(output_dir, "yearly_total.csv"))
  
  cat("  ✓ All CSV files created!\n")
}

# =============================================================================
# CREATE JSON DATASETS FOR WEB VISUALIZATIONS (Chart.js, D3.js, etc.)
# =============================================================================

create_json_datasets <- function(data_file, output_dir = OUTPUT_DIR) {
  
  cat("  Loading data for JSON export...\n")
  dt <- fread(data_file, showProgress = FALSE)
  
  if (is.character(dt$date)) {
    dt[, date := ymd_hms(date, quiet = TRUE)]
  }
  
  dt[, year_month := floor_date(date, "month")]
  dt[, year := year(date)]
  dt[, hour := hour(date)]
  dt[, wday := wday(date, label = TRUE, abbr = FALSE)]
  
  # 1. Monthly totals JSON
  monthly <- dt[, .N, by = year_month][order(year_month)]
  monthly_json <- list(
    labels = format(monthly$year_month, "%Y-%m"),
    data = monthly$N
  )
  write_json(monthly_json, file.path(output_dir, "monthly_total.json"), 
             auto_unbox = TRUE, pretty = TRUE)
  
  # 2. Monthly by crime type JSON (top 10)
  top_types <- dt[, .N, by = primary_type][order(-N)][1:10]$primary_type
  monthly_by_type <- dt[primary_type %in% top_types, 
                        .N, 
                        by = .(year_month, primary_type)]
  
  # Reshape for stacked chart format
  monthly_wide <- dcast(monthly_by_type, year_month ~ primary_type, value.var = "N", fill = 0)
  
  datasets <- lapply(top_types, function(crime_type) {
    list(
      label = crime_type,
      data = as.numeric(monthly_wide[[crime_type]])
    )
  })
  
  monthly_type_json <- list(
    labels = format(monthly_wide$year_month, "%Y-%m"),
    datasets = datasets
  )
  write_json(monthly_type_json, file.path(output_dir, "monthly_by_type.json"),
             auto_unbox = TRUE, pretty = TRUE)
  
  # 3. Yearly totals JSON
  yearly <- dt[, .N, by = year][order(year)]
  yearly_json <- list(
    labels = yearly$year,
    data = yearly$N
  )
  write_json(yearly_json, file.path(output_dir, "yearly_total.json"),
             auto_unbox = TRUE, pretty = TRUE)
  
  # 4. Hourly pattern JSON
  hourly <- dt[, .N, by = hour][order(hour)]
  hourly_json <- list(
    labels = hourly$hour,
    data = hourly$N
  )
  write_json(hourly_json, file.path(output_dir, "hourly_pattern.json"),
             auto_unbox = TRUE, pretty = TRUE)
  
  # 5. Day of week JSON
  wday_pattern <- dt[, .N, by = wday]
  wday_json <- list(
    labels = as.character(wday_pattern$wday),
    data = wday_pattern$N
  )
  write_json(wday_json, file.path(output_dir, "dayofweek_pattern.json"),
             auto_unbox = TRUE, pretty = TRUE)
  
  cat("  ✓ All JSON files created!\n")
}

# =============================================================================
# GENERATE METADATA (last update time, record counts, etc.)
# =============================================================================

generate_metadata <- function(data_file, output_dir = OUTPUT_DIR) {
  
  dt <- fread(data_file, showProgress = FALSE)
  
  if (is.character(dt$date)) {
    dt[, date := ymd_hms(date, quiet = TRUE)]
  }
  
  metadata <- list(
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    total_records = nrow(dt),
    date_range = list(
      start = format(min(dt$date, na.rm = TRUE), "%Y-%m-%d"),
      end = format(max(dt$date, na.rm = TRUE), "%Y-%m-%d")
    ),
    top_crimes = head(dt[, .N, by = primary_type][order(-N)], 10),
    data_source = "Chicago Data Portal - https://data.cityofchicago.org/"
  )
  
  write_json(metadata, file.path(output_dir, "metadata.json"), 
             auto_unbox = TRUE, pretty = TRUE)
  
  cat("  ✓ Metadata saved\n")
  cat("  ✓ Last updated:", metadata$last_updated, "\n")
  cat("  ✓ Total records:", format(metadata$total_records, big.mark = ","), "\n")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║  CHICAGO CRIME - AUTOMATED GITHUB PAGES PIPELINE            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("To run the full pipeline:\n")
cat("  run_full_pipeline()\n\n")

cat("For testing (100k records):\n")
cat("  run_full_pipeline(max_records = 100000)\n\n")

cat("Files will be saved to the '", OUTPUT_DIR, "/' directory\n", sep = "")
cat("Commit and push to GitHub to update your dashboard!\n\n")

# Uncomment to run immediately:
# run_full_pipeline()

# Or for testing:
# run_full_pipeline(max_records = 100000)

