library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(ggplot2)

raw_dir     <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:raw/glcinflationmonthedata/"
figures_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/output/figures/"

# ---- Read the inflation forward curve ----
read_fwd <- function(path) {
  read_excel(path, sheet = "2. fwd curve", skip = 3, .name_repair = "unique_quiet") |>
    rename(date = 1) |>
    filter(!is.na(date)) |>
    mutate(date = if (is.numeric(date)) as.Date(date, origin = "1899-12-30") else as.Date(date))
}

fwd <- bind_rows(
  read_fwd(paste0(raw_dir, "GLCInf2025present.xlsx")),
  read_fwd(paste0(raw_dir, "GLCInf20162024.xlsx"))
) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(date)

# ---- Pick observation dates: before the reform, after it, and now ----
obs_dates <- c(as.Date("2018-12-31"), as.Date("2021-12-31"),
               as.Date("2023-12-31"), max(fwd$date))

# Latest available curve on or before each observation date
sel_dates <- do.call(c, lapply(obs_dates, function(d) max(fwd$date[fwd$date <= d])))
print(sel_dates)

curves <- fwd |>
  filter(date %in% sel_dates) |>
  pivot_longer(-date, names_to = "maturity", values_to = "fwd") |>
  mutate(maturity = as.numeric(maturity),
         horizon  = decimal_date(date) + maturity,     # calendar year the forward refers to
         curve    = factor(format(date, "%b %Y"), levels = format(sel_dates, "%b %Y"))) |>
  filter(!is.na(fwd), horizon <= 2045)

# ---- Chart ----
p5 <- ggplot(curves, aes(x = horizon, y = fwd, colour = curve)) +
  geom_vline(xintercept = 2030.1, linetype = "dashed", colour = "grey40") +
  geom_line() +
  annotate("text", x = 2030.3, y = max(curves$fwd), label = "RPI aligns with CPIH (Feb 2030)",
           hjust = 0, size = 3, colour = "grey30") +
  labs(
    title = "Market-implied inflation by future year",
    subtitle = "Instantaneous RPI inflation forwards, plotted by the calendar year they refer to",
    x = "Calendar year", y = "Per cent", colour = "Curve observed",
    caption = "Source: Bank of England, author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p5)
ggsave(paste0(figures_dir, "05_reform_check.png"), p5, width = 8, height = 5, dpi = 300)