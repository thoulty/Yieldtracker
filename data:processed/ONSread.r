library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

raw_dir       <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/Indexmeasures/"
processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"

# ---- Find a file by name (.xls or .xlsx) ----
find_file <- function(name) {
  f <- list.files(raw_dir, pattern = paste0("^", name, "\\.xlsx?$"), full.names = TRUE)
  if (length(f) != 1) stop("Expected one file called ", name, ".xls or .xlsx in ", raw_dir)
  f
}

# ---- Read one ONS Excel file, keeping monthly rows only ----
read_ons <- function(path) {
  read_excel(path, col_names = c("period", "value"), col_types = "text") |>
    filter(str_detect(period, "^\\d{4} [A-Z]{3}$")) |>
    transmute(month = as.Date(parse_date_time(period, "Y b")),
              value = as.numeric(value))
}

rpi  <- read_ons(find_file("rpi"))  |> rename(rpi = value)
cpi  <- read_ons(find_file("cpi"))  |> rename(cpi = value)
cpih <- read_ons(find_file("cpih")) |> rename(cpih = value)

# ---- Combine ----
ons_inflation <- rpi |>
  left_join(cpi,  by = "month") |>
  left_join(cpih, by = "month") |>
  arrange(month)

# ---- Checks ----
print(range(ons_inflation$month))
print(tail(ons_inflation))

saveRDS(ons_inflation, paste0(processed_dir, "ons_inflation_monthly.rds"))