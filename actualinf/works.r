library(dplyr)
library(tidyr)
library(ggplot2)

processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"
figures_dir   <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/output/figures/"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

actual <- readRDS(paste0(processed_dir, "ons_inflation_monthly.rds")) |>
  mutate(wedge = rpi - cpi)

inf_measures <- readRDS(paste0(processed_dir, "inflation_measures.rds"))

# ---- How big is the RPI-CPI wedge? ----
wedge_summary <- actual |>
  filter(!is.na(wedge)) |>
  mutate(period = case_when(
    month < as.Date("2010-01-01") ~ "1989-2009",
    month < as.Date("2020-01-01") ~ "2010-2019",
    TRUE                          ~ "2020-present"
  )) |>
  group_by(period) |>
  summarise(mean_wedge = mean(wedge), median_wedge = median(wedge), .groups = "drop")

print(wedge_summary)

# ---- Chart: actual inflation vs long-run market expectations ----
p3 <- actual |>
  filter(month >= as.Date("1989-01-01")) |>
  select(month, rpi, cpi) |>
  left_join(inf_measures |> select(month, inf_5y5y), by = "month") |>
  pivot_longer(-month, names_to = "series", values_to = "rate") |>
  mutate(series = factor(series, levels = c("rpi", "cpi", "inf_5y5y"),
                         labels = c("RPI inflation (actual)", "CPI inflation (actual)",
                                    "5y5y implied RPI inflation (market)"))) |>
  ggplot(aes(x = month, y = rate, colour = series)) +
  geom_hline(yintercept = 2, linetype = "dashed", colour = "grey50") +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Actual UK inflation vs long-run market expectations",
    subtitle = "Annual rates; dashed line marks the 2% CPI target",
    x = NULL, y = "Per cent", colour = NULL,
    caption = "Source: ONS, Bank of England, author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "vertical")

print(p3)
ggsave(paste0(figures_dir, "03_actual_vs_expected.png"), p3, width = 8, height = 5, dpi = 300)