library(dplyr)
library(readxl)

raw_dir       <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/surveys/"
processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"

ias_path <- paste0(raw_dir, "ias.xlsx")   # change to .xls if that's the extension

raw    <- read_excel(ias_path, sheet = "LONG-RUN", col_names = FALSE, col_types = "text")
labels <- raw[[1]]

# ---- Survey dates: row 4, Excel serials ----
dates <- as.Date(suppressWarnings(as.numeric(unlist(raw[4, -1]))), origin = "1899-12-30")

# ---- Median answer for a question: first "Median" row below the question label ----
get_median <- function(q_pattern) {
  q_row <- which(grepl(q_pattern, labels))[1]
  if (is.na(q_row)) stop("Question not found: ", q_pattern)
  med_rows <- which(labels == "Median")
  med_row  <- med_rows[med_rows > q_row][1]
  suppressWarnings(as.numeric(unlist(raw[med_row, -1])))
}

ias <- tibble(
  month        = dates,
  hh_perceived = get_median("^Q\\.\\s*1\\b"),    # perceived inflation, last 12 months
  hh_1y        = get_median("^Q\\.\\s*2a\\b"),   # expected, next 12 months
  hh_2y        = get_median("^Q\\.\\s*2b\\b"),   # expected, 12 months after that
  hh_5y        = get_median("^Q\\.\\s*2c\\b")    # expected, in five years' time
) |>
  filter(!is.na(month)) |>
  mutate(
    mode    = if_else(month >= as.Date("2020-05-01"), "online", "face-to-face"),
    savanta = month >= as.Date("2026-08-01")
  )

# ---- Checks ----
print(range(ias$month))
print(head(ias))
print(tail(ias))
print(ias |> filter(!is.na(hh_5y)) |> slice(1) |> pull(month))   # first 5-year reading

# ---- Save ----
saveRDS(ias, paste0(processed_dir, "ias_expectations.rds"))

library(stringr)

# ---- Decision Maker Panel (firms) ----
dmp_path <- paste0(raw_dir, "dmp.xlsx")

raw_dmp <- read_excel(dmp_path, sheet = "Special Qs - CPI Inflation", col_names = FALSE,
                      col_types = "text", .name_repair = "unique_quiet")

# Period labels like "Nov-16 to Jan-17": take the first and last month
periods <- unlist(raw_dmp[2, -(1:2)])
months  <- str_extract_all(periods, "[A-Z][a-z]{2}-\\d{2}")
to_date <- function(x) as.Date(paste0("01-", x), format = "%d-%b-%y")

dmp <- tibble(
  period_start   = to_date(sapply(months, function(m) m[1])),
  period_end     = to_date(sapply(months, function(m) m[length(m)])),
  firm_perceived = suppressWarnings(as.numeric(unlist(raw_dmp[3, -(1:2)]))),
  firm_1y        = suppressWarnings(as.numeric(unlist(raw_dmp[4, -(1:2)]))),
  firm_3y        = suppressWarnings(as.numeric(unlist(raw_dmp[5, -(1:2)])))
) |>
  filter(!is.na(period_start), !is.na(firm_1y))

print(dmp)

saveRDS(dmp, paste0(processed_dir, "dmp_expectations.rds"))