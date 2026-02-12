# R/create_csv_datasets.R
library(data.table)
library(lubridate)

create_csv_datasets <- function(data_file, output_dir) {

  dt <- fread(data_file)
  dt[, `:=`(
    year = year(date),
    month = month(date),
    hour = hour(date),
    wday = wday(date, label = TRUE, abbr = FALSE),
    year_month = floor_date(date, "month")
  )]

  fwrite(dt[, .N, by = year_month],
         file.path(output_dir, "monthly_total.csv"))

  fwrite(dt[, .N, by = year],
         file.path(output_dir, "yearly_total.csv"))

  fwrite(dt[, .N, by = hour],
         file.path(output_dir, "hourly_pattern.csv"))

  fwrite(dt[, .N, by = wday],
         file.path(output_dir, "dayofweek_pattern.csv"))

  heatmap <- dcast(dt[, .N, by = .(hour, wday)],
                   hour ~ wday, fill = 0)
  fwrite(heatmap,
         file.path(output_dir, "hour_day_heatmap.csv"))

  cat("✓ CSV datasets created\n")
}
