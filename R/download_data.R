# R/download_data.R
library(data.table)
library(lubridate)

API_LIMIT <- 50000

download_timeseries_data <- function(output_file, max_records = NULL) {

  base_url <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv"
  select_cols <- "?$select=date,primary_type,arrest,domestic"

  offset <- 0
  total <- 0
  first_chunk <- TRUE

  repeat {

    url <- paste0(
      base_url, select_cols,
      "&$limit=", API_LIMIT,
      "&$offset=", offset
    )

    cat("Downloading offset", format(offset, big.mark = ","), "...")

    success <- tryCatch({

      chunk <- fread(url, showProgress = FALSE)
      if (nrow(chunk) == 0) return(FALSE)

      chunk[, date := ymd_hms(date)]
      chunk[, arrest := arrest == "true"]
      chunk[, domestic := domestic == "true"]

      fwrite(chunk, output_file, append = !first_chunk)

      total <- total + nrow(chunk)
      cat("✓", nrow(chunk), "\n")

      offset <- offset + API_LIMIT
      first_chunk <- FALSE

      if (!is.null(max_records) && total >= max_records) return(FALSE)
      if (nrow(chunk) < API_LIMIT) return(FALSE)

      Sys.sleep(0.4)
      TRUE

    }, error = function(e) {
      cat("FAILED:", e$message, "\n")
      FALSE
    })

    if (!success) break
  }

  cat("✓ Total downloaded:", format(total, big.mark = ","), "\n")
}
