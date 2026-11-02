library(dplyr)
library(tidyr)
library(ggplot2)

processed_dir <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/data:processed/"
figures_dir   <- "/Users/theohoult/Documents/YieldTrackingProject/Yieldtracker/output/figures/"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

nominal   <- readRDS(paste0(processed_dir, "glc_nominal_monthly.rds"))
real      <- readRDS(paste0(processed_dir, "glc_real_monthly.rds"))
inflation <- readRDS(paste0(processed_dir, "glc_inflation_monthly.rds"))

# ---- 10-year panel ----
y10 <- nominal |>
  select(month, nominal = `10`) |>
  inner_join(real      |> select(month, real = `10`),      by = "month") |>
  inner_join(inflation |> select(month, inflation = `10`), by = "month")

# ---- Episodes (start month to end month) ----
episodes <- tribble(
  ~episode,                   ~start,        ~end,
  "Global financial crisis",  "2008-06-01",  "2009-03-01",
  "Brexit referendum",        "2016-05-01",  "2016-08-01",
  "Covid shock",              "2020-01-01",  "2020-04-01",
  "2022 inflation surge",     "2021-12-01",  "2022-08-01",
  "Mini-budget",              "2022-08-01",  "2022-09-01",
  "Mid-2026 sell-off",        "2026-06-01",  "2026-09-01"
) |>
  mutate(across(c(start, end), as.Date))

# ---- Decomposition ----
decomp <- episodes |>
  left_join(y10, by = c("start" = "month")) |>
  left_join(y10, by = c("end" = "month"), suffix = c("_start", "_end")) |>
  transmute(
    episode,
    d_nominal   = nominal_end   - nominal_start,
    d_real      = real_end      - real_start,
    d_inflation = inflation_end - inflation_start
  ) |>
  mutate(episode = factor(episode, levels = episodes$episode))

print(decomp)
saveRDS(decomp, paste0(processed_dir, "decomposition_episodes.rds"))

# ---- Chart ----
p2 <- decomp |>
  pivot_longer(c(d_real, d_inflation), names_to = "component", values_to = "change") |>
  mutate(component = factor(component, levels = c("d_real", "d_inflation"),
                            labels = c("Real yield", "Implied inflation"))) |>
  ggplot(aes(x = episode, y = change, fill = component)) +
  geom_col() +
  geom_point(data = decomp, aes(x = episode, y = d_nominal),
             inherit.aes = FALSE, size = 2.5) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(
    title = "What drove moves in 10-year gilt yields?",
    subtitle = "Change in 10-year nominal yield (dot) split into real and inflation components",
    x = NULL, y = "Percentage points", fill = NULL,
    caption = "Source: Bank of England, author's calculations"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1))

print(p2)
ggsave(paste0(figures_dir, "02_yield_decomposition.png"), p2, width = 8, height = 5, dpi = 300)