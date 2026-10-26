library(dplyr)
library(tidyr)
library(ggplot2)

processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"
figures_dir   <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/output/figures/"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

inflation <- readRDS(paste0(processed_dir, "glc_inflation_monthly.rds"))

# ---- Build measures ----
# Spot rates are continuously compounded, so the 5y5y forward is exact:
# (10 x 10-year rate - 5 x 5-year rate) / 5
inf_measures <- inflation |>
  transmute(
    date, month,
    inf_5y   = `5`,
    inf_10y  = `10`,
    inf_5y5y = (10 * `10` - 5 * `5`) / 5
  ) |>
  filter(!is.na(inf_5y), !is.na(inf_10y))

saveRDS(inf_measures, paste0(processed_dir, "inflation_measures.rds"))

print(tail(inf_measures))

# ---- Chart ----
p1 <- inf_measures |>
  pivot_longer(c(inf_5y, inf_10y, inf_5y5y), names_to = "measure", values_to = "rate") |>
  mutate(measure = factor(measure,
                          levels = c("inf_5y", "inf_10y", "inf_5y5y"),
                          labels = c("5-year", "10-year", "5y5y forward"))) |>
  ggplot(aes(x = date, y = rate, colour = measure)) +
  geom_line() +
  labs(
    title = "UK market-implied inflation",
    subtitle = "RPI-based, from nominal and index-linked gilts, month-end",
    x = NULL, y = "Per cent", colour = NULL,
    caption = "Source: Bank of England, author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p1)
ggsave(paste0(figures_dir, "01_implied_inflation.png"), p1, width = 8, height = 5, dpi = 300)