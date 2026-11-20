# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(pigoar2026)
library(purrr)

theme_set(
  theme_light() +
    theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 20, hjust = .5),
      axis.text.y = element_text(size = 18),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16, face = "bold")
    )
)

set.seed(2027)

# load data --------------------------------------------------------------
ctf_dynamic <- cliaretl::closeness_to_frontier_dynamic |>
  # only retain countries
  filter(country_group == 0) |>
  mutate(
    year = as.integer(year),
    income_group = forcats::fct_relevel(
      income_group,
      c(
        "High income",
        "Upper middle income",
        "Lower middle income",
        "Low income"
      )
    )
  )

compiled_indicators <- fs::path_package(
  "extdata",
  "compiled_indicators.rds",
  package = "cliaretl"
) |>
  readr::read_rds()

dictionary <- cliaretl::db_variables

# ILOSTAT labour income share of GDP
# cliaretl::labor_income carries two distinct series for CHN and IND with no
# label identifying the headline estimate; averaging keeps one row per
# country-year so these countries are not double-counted
labor_income <- cliaretl::labor_income |>
  group_by(country_code, year = as.integer(year)) |>
  summarise(
    labor_income_share = mean(labor_income, na.rm = TRUE),
    .groups = "drop"
  )

cliar_raw <- compiled_indicators |>
  left_join(
    labor_income,
    by = c("country_code", "year")
  )

cliar_ctf <- ctf_dynamic |>
  left_join(
    labor_income,
    by = c("country_code", "year")
  ) |>
  left_join(
    pigoar2026::budget_execution |>
      mutate(year = as.integer(year)) |>
      # classify_quantile() labels the highest values "Strong", so the
      # absolute deviation from 100% execution is negated to make
      # higher = better, in line with the closeness-to-frontier indicators
      mutate(
        budget_execution_rate = -abs(budget_execution_rate - 100)
      ),
    by = c("country_code", "year")
  )

cluster_names <- c(
  "hrm" = "vars_hrm_avg",
  "pub_sector_corruption" = "vdem_core_v2x_pubcorr",
  "open_budget" = "ibp_obs_obi",
  "budget_execution" = "budget_execution_rate"
)

# visualize --------------------------------------------------------------
plot_labor_income <- function(data, var) {
  min_year <- data |>
    select(all_of(c(var, "year"))) |>
    na.omit() |>
    pull(year) |>
    min()

  quantile_baseline <- data |>
    filter(year == min_year) |>
    classify_quantile(var, "year", threshold = "tercile") |>
    select(country_code, quantile_indicator)

  data |>
    left_join(quantile_baseline, by = "country_code") |>
    filter(
      !is.na(quantile_indicator) &
        between(year, min_year, 2024)
    ) |>
    group_by(quantile_indicator, year) |>
    summarise(
      labor_income_share = mean(labor_income_share, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ggplot(
      aes(year, labor_income_share, color = quantile_indicator)
    ) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.5) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      name = "Global Level",
      na.value = "grey60"
    ) +
    scale_x_continuous(
      breaks = scales::breaks_width(2),
      labels = scales::label_number(big.mark = "")
    ) +
    scale_y_continuous(
      breaks = scales::breaks_width(5),
      limits = c(30, 60)
    ) +
    theme(legend.position = "bottom") +
    labs(x = "Year", y = "Labor income as a share of GDP (%)")
}

cluster_letters <- letters[seq_along(cluster_names)]

labor_income_share_plots <-purrr::map(
    cluster_names,
    \(var) plot_labor_income(cliar_ctf, var)
  ) |>
    purrr::set_names(
      sprintf(
        "fig_1%s_labor_income_share_vs_%s_trend.png",
        cluster_letters,
        names(cluster_names)
      )
    )

purrr::iwalk(
  labor_income_share_plots,
  \(plot, filename) {
    ggsave(
      filename = here::here("analysis", "chapter_1", "figs", filename),
      plot = plot,
      width = 10,
      height = 10,
      dpi = 300,
      bg = "white"
    )
  }
)
