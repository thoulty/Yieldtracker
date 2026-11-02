library(dplyr)
library(tidyr)
library(ggplot2)

processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"
figures_dir   <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/output/figures/"

ias          <- readRDS(paste0(processed_dir, "ias_expectations.rds"))
dmp          <- readRDS(paste0(processed_dir, "dmp_expectations.rds"))
inf_measures <- readRDS(paste0(processed_dir, "inflation_measures.rds"))

start <- as.Date("2009-01-01")

# ---- Combine into one long dataset ----
plot_data <- bind_rows(
  inf_measures |> filter(month >= start) |>
    transmute(date = month, value = inf_5y5y, series = "Markets: 5y5y implied (RPI)"),
  ias |> filter(month >= start, !is.na(hh_5y)) |>
    transmute(date = month, value = hh_5y, series = "Households: 5 years ahead"),
  dmp |>
    transmute(date = period_start + (period_end - period_start) / 2,   # midpoint of period
              value = firm_3y, series = "Firms: 3 years ahead (CPI)")
) |>
  mutate(series = factor(series, levels = c("Markets: 5y5y implied (RPI)",
                                            "Households: 5 years ahead",
                                            "Firms: 3 years ahead (CPI)")))

# ---- Chart ----
p4 <- ggplot(plot_data, aes(x = date, y = value, colour = series)) +
  geom_hline(yintercept = 2, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = as.Date(c("2020-05-01", "2026-08-01")),
             linetype = "dotted", colour = "grey60") +
  geom_line(data = filter(plot_data, series == "Markets: 5y5y implied (RPI)")) +
  geom_line(data = filter(plot_data, series != "Markets: 5y5y implied (RPI)"), linewidth = 0.4) +
  geom_point(data = filter(plot_data, series != "Markets: 5y5y implied (RPI)"), size = 1.6) +
  labs(
    title = "Long-run inflation expectations: markets, households, and firms",
    subtitle = "Per cent; dashed line marks the 2% CPI target; dotted lines mark household survey method changes",
    x = NULL, y = "Per cent", colour = NULL,
    caption = "Source: Bank of England, Bank of England/Ipsos/Savanta Inflation Attitudes Survey,\nDecision Maker Panel, author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "vertical")

print(p4)
ggsave(paste0(figures_dir, "04_survey_expectations.png"), p4, width = 8, height = 5, dpi = 300)

# ---- Summary table ----
latest <- tibble(
  measure = c("Markets: 5y5y implied (RPI)", "Households: 5 years ahead", "Firms: 3 years ahead (CPI)"),
  latest  = c(last(inf_measures$inf_5y5y), last(ias$hh_5y), last(dmp$firm_3y)),
  peak_2022_23 = c(
    max(inf_measures$inf_5y5y[inf_measures$month >= as.Date("2022-01-01") & inf_measures$month < as.Date("2024-01-01")]),
    max(ias$hh_5y[ias$month >= as.Date("2022-01-01") & ias$month < as.Date("2024-01-01")], na.rm = TRUE),
    max(dmp$firm_3y[dmp$period_start < as.Date("2024-01-01")])
  )
)
print(latest)