library(readxl)
library(dplyr)
library(lubridate)

read_inf <- function(path) {
  read_excel(path, sheet = "4. spot curve", skip = 3, .name_repair = "unique_quiet") |>
    rename(date = 1) |>
    filter(!is.na(date)) |>
    mutate(date = if (is.numeric(date)) as.Date(date, origin = "1899-12-30") else as.Date(date))
}

raw_dir       <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/glcinflationmonthedata/"
processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"

inf_1979_2015 <- read_inf(paste0(raw_dir, "GLCInf19792015.xlsx"))
inf_2016_2024 <- read_inf(paste0(raw_dir, "GLCInf20162024.xlsx"))
inf_2025_now  <- read_inf(paste0(raw_dir, "GLCInf2025present.xlsx"))

inflation <- bind_rows(inf_2025_now, inf_2016_2024, inf_1979_2015) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(date) |>
  mutate(month = floor_date(date, "month")) |>
  relocate(month, .after = date)

range(inflation$date)
tail(inflation[, c("date", "month", "5", "10", "20")])

inflation |>
  filter(date >= as.Date("1985-01-31")) |>
  summarise(missing_5 = sum(is.na(`5`)), missing_10 = sum(is.na(`10`)))

print(head(inflation[, c("date", "month", "5", "10", "20")], 20))

# ---- Save ----
saveRDS(inflation, paste0(processed_dir, "glc_inflation_monthly.rds"))
