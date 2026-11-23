# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(pigoar2026)
library(purrr)

source(
  here::here("R", "plot.R")
)

source(
  here::here("R", "regression.R")
)

set_default_theme()

set.seed(2027)

# read-in data -----------------------------------------------------------
credit_rating_average <- pigoar2026::credit_rating |>
  filter(
    year %in% 2020:2024
  ) |>
  group_by(country_code) |>
  summarise(
    credit_rating_mean = mean(credit_rating, na.rm = TRUE)
  )

wdi_outcomes <- cliaretl::wdi_indicators |>
  filter(
    year %in% 2020:2024
  ) |>
  group_by(country_code) |>
  summarise(
    gdp_per_capita = mean(log(wdi_nygdppcapppkd), na.rm = TRUE),
    poverty_gap_215 = mean(wdi_sipovlmicgp, na.rm = TRUE),
    gdp_growth = mean(wdi_nygdpmktpkdzg, na.rm = TRUE),
    unemployment_rate = mean(wdi_sluemtotlnezs, na.rm = TRUE),
    literacy_rate = mean(wdi_seadtlitrzs, na.rm = TRUE),
    mortality_rate = mean(wdi_shdynmort, na.rm = TRUE)
  )

labor_income_average <- pigoar2026::labor_income |>
  filter(
    year %in% 2020:2024
  ) |>
  group_by(country_code) |>
  summarise(
    labor_income = mean(labor_income, na.rm = TRUE)
  )

# cliar correlation
cliar_correlation <- cliaretl::closeness_to_frontier_static |>
  left_join(
    credit_rating_average,
    by = c("country_code")
  ) |>
  left_join(
    wdi_outcomes,
    by = c("country_code")
  ) |>
  left_join(
    labor_income_average,
    by = c("country_code")
  ) |>
  left_join(
    bready,
    by = c("country_code")
  ) |>
  mutate(
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

# join with bready topics
bready_topic_correlation <- cliaretl::closeness_to_frontier_static |>
  left_join(
    pigoar2026::bready_topic |>
      select(
        country_code,
        topic,
        pillar_1_overall,
        pillar_2_overall,
        pillar_3_overall
      ),
    by = "country_code"
  )

# analyze ----------------------------------------------------------------
# correlation between institutional clusters and development outcomes
institutional_clusters <- c(
  "vars_hrm_avg",
  "vars_pfm_avg",
  "vars_digital_avg",
  "vars_anticorruption_avg",
  "vars_transp_avg"
)

names(institutional_clusters) <- c(
  "Public Human Resource Management",
  "Public Financial Management",
  "Information systems",
  "Integrity",
  "Transparency and Accountability"
)

institutional_clusters <- institutional_clusters |>
  tibble::enframe(
    name = "x_lab",
    value = "x_val"
  )

outcomes <- c(
  "Credit Rating" = "credit_rating_mean",
  "Logged GDP per capita (Constant International Dollars)" = "gdp_per_capita",
  "Poverty Gap ($3.65 a day)" = "poverty_gap_215",
  "Labor income" = "labor_income",
  "Infant mortality rate (logged)" = "mortality_rate",
  "Public Services for Businesses" = "pillar_2_public_services",
  "Operational Efficiency" = "pillar_3_operational_efficiency"
) |>
  tibble::enframe(
    name = "y_lab",
    value = "y_val"
  )

# generate all possible combinations between clusters and outcomes
cartesian_product <- tidyr::crossing(
  institutional_clusters |> select(x_val),
  outcomes |> select(y_val)
) |>
  left_join(institutional_clusters) |>
  left_join(outcomes)

# generate plots
fig_cartesian_product <- cartesian_product |>
  filter(
    y_val == "credit_rating_mean",
    x_val != "vars_pfm_avg"
  )

correlation_plots <- purrr::pmap(
  fig_cartesian_product,
  function(x_val, y_val, x_lab, y_lab) {
    plot <- ggplot_correlation(
      data = cliar_correlation |> filter(!is.na(income_group)),
      x = x_val,
      y = y_val,
      group = "income_group"
    ) +
      scale_y_continuous(
        labels = function(x) stringr::str_wrap(x, width = 15)
      ) +
      labs(
        x = paste0(x_lab, " (2020-2024)"),
        y = y_lab
      ) +
      guides(
        color = guide_legend(
          "Income Group",
          nrow = 2
        )
      )

    if (y_val == "poverty_gap_215" | y_val == "mortality_rate") {
      plot <- plot +
        scale_y_log10()
    }

    plot
  }
)

# save plots
purrr::walk2(
  correlation_plots,
  seq_len(nrow(fig_cartesian_product)),
  ~ ggplot2::ggsave(
    filename = file.path(
      "analysis/chapter_1/figs",
      sprintf(
        "fig_cor_%s_vs_%s.png",
        gsub("\\s+", "_", fig_cartesian_product$y_val[.y]),
        gsub("\\s+", "_", fig_cartesian_product$x_val[.y])
      )
    ),
    plot = .x,
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white"
  )
)

# distribution of outcomes by institutional level ------------------------
# topic score averages the three B-READY pillars so each topic is one segment
bready_topic_score <- pigoar2026::bready_topic |>
  mutate(
    topic_score = rowMeans(
      pick(pillar_1_overall, pillar_2_overall, pillar_3_overall),
      na.rm = TRUE
    ),
    topic = paste0(
      "bready_",
      stringr::str_replace_all(stringr::str_to_lower(topic), "\\s+", "_")
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = country_code,
    names_from = topic,
    values_from = topic_score
  )

outcome_distribution <- cliaretl::closeness_to_frontier_static |>
  # only retain countries
  filter(country_group == 0) |>
  select(
    country_code,
    all_of(institutional_clusters[["x_val"]])
  ) |>
  left_join(
    wdi_outcomes,
    by = "country_code"
  ) |>
  left_join(
    labor_income_average,
    by = "country_code"
  ) |>
  left_join(
    bready_topic_score,
    by = "country_code"
  )

distribution_outcomes <- c(
  "GDP per capita, PPP (logged)" = "gdp_per_capita",
  "Poverty gap at $3.65 a day (%)" = "poverty_gap_215",
  "Unemployment rate (%)" = "unemployment_rate",
  "Adult literacy rate (%)" = "literacy_rate",
  "Under-5 mortality rate (per 1,000 live births)" = "mortality_rate",
  "Labor income share of GDP (%)" = "labor_income"
)

outcome_distribution_plots <- institutional_clusters |>
  # the static PFM average covers only 6 countries, too few to split into levels
  filter(x_val != "vars_pfm_avg") |>
  mutate(
    plot = purrr::map2(
      x_val,
      x_lab,
      \(var, legend_title) {
        plot_quantile_outcomes(
          outcome_distribution,
          var = var,
          outcomes = distribution_outcomes,
          threshold = "tercile",
          legend_title = legend_title
        )
      }
    ),
    filename = sprintf(
      "fig_2%s_outcome_distribution_by_%s.png",
      letters[row_number()],
      stringr::str_remove_all(x_val, "^vars_|_avg$")
    )
  )

purrr::walk2(
  outcome_distribution_plots[["plot"]],
  outcome_distribution_plots[["filename"]],
  \(plot, filename) {
    ggsave(
      filename = here::here("analysis", "chapter_1", "figs", filename),
      plot = plot,
      width = 12,
      height = 20,
      dpi = 300,
      bg = "white"
    )
  }
)

# regression of outcomes on institutional levels -------------------------
population_average <- pigoar2026::population |>
  filter(
    year %in% 2020:2024
  ) |>
  group_by(country_code) |>
  summarise(
    log_population = mean(log(total_population), na.rm = TRUE)
  )

regression_clusters <- institutional_clusters |>
  filter(x_val != "vars_pfm_avg") |>
  mutate(
    level_var = paste0(x_val, "_level")
  )

# terciles are computed over every country with cluster data, before rows with
# missing outcomes are dropped by lm(), so levels match those in figure 2
regression_data <- purrr::reduce(
  regression_clusters[["x_val"]],
  \(data, var) {
    data |>
      classify_quantile(var, threshold = "tercile") |>
      mutate(
        # Weak is the reference category, so coefficients read as the gap
        # relative to the weakest tercile
        "{var}_level" := factor(
          quantile_indicator,
          levels = c("Weak", "Emerging", "Strong")
        )
      ) |>
      select(-quantile_indicator)
  },
  .init = outcome_distribution
) |>
  left_join(
    population_average,
    by = "country_code"
  )

regression_controls <- c("gdp_per_capita", "log_population")

# treat B-READY separately
regression_outcomes <- distribution_outcomes[
  which(distribution_outcomes == "unemployment_rate"):which(distribution_outcomes == "labor_income")
]

# each cluster enters its own model because the clusters are collinear
regression_results <- regression_clusters |>
  tidyr::expand_grid(
    outcome = unname(regression_outcomes)
  ) |>
  mutate(
    coefficients = purrr::map2(
      outcome,
      level_var,
      \(outcome, level_var) {
        fit_outcome_model(
          regression_data,
          outcome = outcome,
          predictors = c(level_var, regression_controls)
        )
      }
    )
  ) |>
  select(x_val, x_lab, coefficients) |>
  tidyr::unnest(coefficients) |>
  mutate(
    level = stringr::str_extract(term, "(?<=_level)(Emerging|Strong)$"),
    outcome_label = factor(
      outcome,
      levels = unname(regression_outcomes),
      labels = names(regression_outcomes)
    )
  )

fig_regression_coefficients <- regression_results |>
  filter(!is.na(level)) |>
  mutate(
    x_lab = factor(x_lab, levels = rev(regression_clusters[["x_lab"]])),
    # Strong is listed first so it plots above Emerging within each cluster
    level = factor(level, levels = c("Strong", "Emerging"))
  ) |>
  ggplot(
    aes(
      x = estimate,
      xmin = conf_low,
      xmax = conf_high,
      y = x_lab,
      color = level
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_pointrange(
    position = position_dodge(width = 0.5, reverse = TRUE),
    size = 0.5,
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Emerging" = "goldenrod2",
      "Strong" = "forestgreen"
    ),
    breaks = c("Emerging", "Strong"),
    name = "Level (vs. Weak)"
  ) +
  scale_y_discrete(
    labels = function(x) stringr::str_wrap(x, width = 20)
  ) +
  facet_wrap(
    vars(outcome_label),
    ncol = 2,
    scales = "free_x",
    labeller = label_wrap_gen(width = 35)
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(hjust = 0, color = "grey10"),
    strip.background = element_blank()
  ) +
  labs(
    x = "Difference relative to Weak tercile (95% CI)",
    y = NULL,
    caption = paste(
      "One model per institutional cluster. Controls: logged GDP per capita",
      "(PPP) and logged population, 2020-2024 averages."
    )
  )

ggsave(
  filename = here::here(
    "analysis",
    "chapter_1",
    "figs",
    "fig_3_outcome_regression_coefficients.png"
  ),
  plot = fig_regression_coefficients,
  width = 12,
  height = 16,
  dpi = 300,
  bg = "white"
)

# regression table: one row group per cluster model
regression_term_labels <- c(
  "Emerging" = "Emerging",
  "Strong" = "Strong",
  "gdp_per_capita" = "Logged GDP per capita (PPP)",
  "log_population" = "Logged population"
)

regression_coefficient_rows <- regression_results |>
  mutate(
    term = coalesce(level, term)
  ) |>
  filter(term %in% names(regression_term_labels)) |>
  mutate(
    term_label = regression_term_labels[term],
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      .default = ""
    ),
    cell = sprintf("%.2f%s<br>(%.2f)", estimate, stars, std_error)
  )

regression_fit_rows <- regression_results |>
  distinct(x_val, x_lab, outcome_label, n_obs, r_squared) |>
  mutate(
    Observations = as.character(n_obs),
    `R-squared` = sprintf("%.2f", r_squared)
  ) |>
  tidyr::pivot_longer(
    cols = c(Observations, `R-squared`),
    names_to = "term_label",
    values_to = "cell"
  )

regression_table <- bind_rows(
  regression_coefficient_rows,
  regression_fit_rows
) |>
  mutate(
    x_lab = factor(x_lab, levels = regression_clusters[["x_lab"]]),
    term_label = factor(
      term_label,
      levels = c(regression_term_labels, "Observations", "R-squared")
    )
  ) |>
  arrange(x_lab, term_label) |>
  mutate(
    across(c(x_lab, term_label), as.character)
  ) |>
  select(x_lab, term_label, outcome_label, cell) |>
  tidyr::pivot_wider(
    names_from = outcome_label,
    values_from = cell
  ) |>
  gt::gt(
    rowname_col = "term_label",
    groupname_col = "x_lab"
  ) |>
  gt::text_transform(
    locations = gt::cells_body(),
    fn = \(x) purrr::map(x, gt::html)
  ) |>
  gt::tab_header(
    title = "Development outcomes and institutional levels"
  ) |>
  gt::tab_source_note(
    paste(
      "OLS with standard errors in parentheses. Each row group is a separate",
      "model per outcome. Reference category: Weak tercile of the",
      "institutional cluster. Outcomes and controls are 2020-2024 averages.",
      "* p < 0.1, ** p < 0.05, *** p < 0.01."
    )
  )

dir.create(
  here::here("analysis", "chapter_1", "tables"),
  showWarnings = FALSE
)

gt::gtsave(
  regression_table,
  filename = here::here(
    "analysis",
    "chapter_1",
    "tables",
    "tab_1_outcome_regressions.html"
  )
)
