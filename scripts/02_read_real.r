library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# ---- Read ----
read_real <- function(path) {
  read_excel(path, sheet = "4. spot curve", skip = 3, .name_repair = "unique_quiet") |>
    rename(date = 1) |>
    filter(!is.na(date)) |>
    mutate(date = if (is.numeric(date)) as.Date(date, origin = "1899-12-30") else as.Date(date))
}

raw_dir       <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/glcrealmonthedata/"
processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"

real_1979_2015 <- read_real(paste0(raw_dir, "GLCReal19792015.xlsx"))
real_2016_2024 <- read_real(paste0(raw_dir, "GLCReal20162024.xlsx"))
real_2025_now  <- read_real(paste0(raw_dir, "GLCReal2025present.xlsx"))

# ---- Clean ----
real <- bind_rows(real_2025_now, real_2016_2024, real_1979_2015) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(date) |>
  mutate(month = floor_date(date, "month")) |>
  relocate(month, .after = date) |>
  filter(if_any(-c(date, month), ~ !is.na(.x)))

# ---- Checks ----
print(range(real$date))
print(head(real[, c("date", "month", "5", "10", "20")]))
print(tail(real[, c("date", "month", "5", "10", "20")]))
print(real |>
        filter(date >= as.Date("1985-01-31")) |>
        summarise(missing_5 = sum(is.na(`5`)), missing_10 = sum(is.na(`10`))))

# ---- Save ----
saveRDS(real, paste0(processed_dir, "glc_real_monthly.rds"))

