# R/create_json_datasets.R
library(data.table)
library(lubridate)
library(jsonlite)

create_json_datasets <- function(data_file, output_dir) {

  dt <- fread(data_file)

  dt[, `:=`(
    year = year(date),
    month = month(date),
    hour = hour(date),
    wday = wday(date),
    year_month = floor_date(date, "month")
  )]

  ## Monthly total
  monthly <- dt[, .N, by = year_month][order(year_month)]
  write_json(
    list(labels = format(monthly$year_month, "%Y-%m"),
         data = monthly$N),
    file.path(output_dir, "monthly_total.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  ## Monthly by type
  top_types <- dt[, .N, by = primary_type][order(-N)][1:7]$primary_type
  mtype <- dt[primary_type %in% top_types,
              .N, by = .(year_month, primary_type)]

  labels <- sort(unique(mtype$year_month))

  datasets <- lapply(top_types, function(ct) {
    list(
      label = ct,
      data = sapply(labels, function(l)
        mtype[primary_type == ct & year_month == l]$N %||% 0)
    )
  })

  write_json(
    list(labels = format(labels, "%Y-%m"),
         datasets = datasets),
    file.path(output_dir, "monthly_by_type.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  ## Arrest rate
  arrest <- dt[, .(rate = round(mean(arrest) * 100, 2)),
               by = year_month][order(year_month)]

  write_json(
    list(labels = format(arrest$year_month, "%Y-%m"),
         data = arrest$rate),
    file.path(output_dir, "arrest_rate_trend.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  ## Crime concentration
  top5 <- dt[, .N, by = primary_type][order(-N)][1:5]
  other <- nrow(dt) - sum(top5$N)

  write_json(
    list(
      labels = c(top5$primary_type, "Other"),
      data = c(top5$N, other)
    ),
    file.path(output_dir, "crime_concentration.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  cat("✓ JSON datasets created\n")
}
