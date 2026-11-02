library(readxl)


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# ---- Read ----
read_nom <- function(path) {
  read_excel(path, sheet = "4. spot curve", skip = 3, .name_repair = "unique_quiet") |>
    rename(date = 1) |>
    filter(!is.na(date)) |>
    mutate(date = if (is.numeric(date)) as.Date(date, origin = "1899-12-30") else as.Date(date))
}

raw_dir       <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/glcnominalmonthedata/"
processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"

nom_1970_2015 <- read_nom(paste0(raw_dir, "GLCNom19702015.xlsx"))
nom_2016_2024 <- read_nom(paste0(raw_dir, "GLCNom20162024.xlsx"))
nom_2025_now  <- read_nom(paste0(raw_dir, "GLCNom2025present.xlsx"))

# ---- Clean ----
nominal <- bind_rows(nom_2025_now, nom_2016_2024, nom_1970_2015) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(date) |>
  mutate(month = floor_date(date, "month")) |>
  relocate(month, .after = date) |>
  filter(if_any(-c(date, month), ~ !is.na(.x)))

# ---- Checks ----
print(range(nominal$date))
print(head(nominal[, c("date", "month", "0.5", "2", "10")]))
print(tail(nominal[, c("date", "month", "0.5", "2", "10")]))
print(nominal |>
        filter(date >= as.Date("1997-03-31")) |>
        summarise(missing_0.5 = sum(is.na(`0.5`)), missing_2 = sum(is.na(`2`)), missing_10 = sum(is.na(`10`))))
# ---- Save ----

saveRDS(nominal, paste0(processed_dir, "glc_nominal_monthly.rds"))
