set_default_theme <- function() {
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
}


#' Classify a variable into quantiles, considering baseline year.
#'
#' @param .data A data frame or tibble.
#' @param var Character string. Column name of the numeric variable to classify.
#' @param threshold Type of threshold to use. Defaults to first two quartiles and above the median.
#' Either "default" or "tercile".
#'
#' @return The input data frame with an additional factor column
#'   \code{quantile_indicator}.
#'
#' @importFrom dplyr mutate between case_when
#' @importFrom forcats fct_relevel
#'
#' @details  Adds a \code{quantile_indicator} column classifying each row as
#' \code{"Weak"} (below 25th percentile), \code{"Emerging"} (25th–50th), or
#' \code{"Strong"} (above 50th). If \code{threshold = "tercile"}, the
#' classification is based on terciles instead of quartiles. When \code{.data}
#' has a \code{year} column, only the baseline (earliest) year is classified;
#' cross-sectional data without \code{year} is classified as is.
#'
#' @export
classify_quantile <- function(.data, var, threshold = c("default", "tercile")) {
  type_threshold <- match.arg(threshold)

  if (!("country_code" %in% colnames(.data))) {
    stop("The input data must contain a 'country_code' column.")
  }

  cutoffs <- switch(
    type_threshold,
    default = c(0.25, 0.5),
    tercile = c(1 / 3, 2 / 3)
  )

  if ("year" %in% colnames(.data)) {
    .data <- .data |>
      filter(year == min(year))
  }

  .data |>
    mutate(
      quantile_indicator = case_when(
        .data[[var]] <
          quantile(.data[[var]], cutoffs[[1]], na.rm = TRUE) ~ "Weak",
        between(
          .data[[var]],
          quantile(.data[[var]], cutoffs[[1]], na.rm = TRUE),
          quantile(.data[[var]], cutoffs[[2]], na.rm = TRUE)
        ) ~ "Emerging",
        .data[[var]] >
          quantile(.data[[var]], cutoffs[[2]], na.rm = TRUE) ~ "Strong"
      ),
      quantile_indicator = forcats::fct_relevel(
        .data[["quantile_indicator"]],
        c("Strong", "Emerging", "Weak")
      )
    )
}

#' Plot the distribution of countries across outcomes by institutional level
#'
#' Horizontal variant of \code{pigoar2026::plot_quantile()}. Countries are
#' classified as Weak/Emerging/Strong on an institutional variable and plotted
#' as jittered circles along each outcome, with one horizontal segment per
#' outcome. Large filled circles mark the mean of each level and a dashed line
#' marks the global average.
#'
#' @param .data A cross-sectional data frame with one row per country. Must
#'   contain \code{country_code}, \code{var}, and the columns in \code{outcomes}.
#' @param var Character string. Column name of the institutional variable used
#'   for the Weak/Emerging/Strong classification.
#' @param outcomes Named character vector. Values are outcome column names,
#'   names are their labels. Segments are drawn in this order.
#' @param threshold Passed to \code{classify_quantile()}.
#' @param legend_title Character string. Title of the color legend.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' outcome_distribution |>
#'   plot_quantile_outcomes(
#'     var = "vars_hrm_avg",
#'     outcomes = c("Labor income share of GDP (%)" = "labor_income"),
#'     threshold = "tercile",
#'     legend_title = "Public Human Resource Management"
#'   )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter select group_by summarise mutate all_of
#' @importFrom tidyr pivot_longer
#'
#' @export
plot_quantile_outcomes <- function(
  .data,
  var,
  outcomes,
  threshold = c("default", "tercile"),
  legend_title = NULL
) {
  threshold <- match.arg(threshold)

  # classify before pivoting so quantiles are computed over countries rather
  # than over country-outcome pairs, which would overweight countries with
  # fuller outcome coverage
  data_long <- .data |>
    classify_quantile(var, threshold = threshold) |>
    filter(!is.na(.data[["quantile_indicator"]])) |>
    select(
      all_of(
        c("country_code", "quantile_indicator", unname(outcomes))
      )
    ) |>
    tidyr::pivot_longer(
      cols = all_of(unname(outcomes)),
      names_to = "outcome",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    mutate(
      outcome = factor(
        .data[["outcome"]],
        levels = unname(outcomes),
        labels = names(outcomes)
      )
    )

  level_means <- data_long |>
    group_by(.data[["outcome"]], .data[["quantile_indicator"]]) |>
    summarise(value = mean(.data[["value"]]), .groups = "drop")

  global_means <- data_long |>
    group_by(.data[["outcome"]]) |>
    summarise(value = mean(.data[["value"]]), .groups = "drop")

  data_long |>
    ggplot(
      aes(x = .data[["value"]], y = 0)
    ) +
    geom_vline(
      aes(xintercept = .data[["value"]]),
      data = global_means,
      linetype = "dashed",
      linewidth = 0.8,
      color = "grey40"
    ) +
    geom_point(
      aes(color = .data[["quantile_indicator"]]),
      position = position_jitter(seed = 42, width = 0, height = 0.3),
      shape = 1,
      size = 4,
      stroke = 1,
      alpha = 0.4
    ) +
    geom_point(
      aes(fill = .data[["quantile_indicator"]]),
      data = level_means,
      shape = 21,
      size = 9,
      color = "grey20",
      stroke = 1.5
    ) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      aesthetics = c("color", "fill"),
      name = legend_title,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.5, 0.5),
      breaks = NULL
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6)
    ) +
    # outcomes are on different units, so each segment needs its own x scale;
    # facet_grid() can only free x across columns, hence facet_wrap()
    facet_wrap(
      vars(.data[["outcome"]]),
      ncol = 1,
      scales = "free_x",
      labeller = label_wrap_gen(width = 60)
    ) +
    guides(
      color = guide_legend(
        override.aes = list(shape = 21, size = 7, stroke = 1.5, alpha = 1)
      )
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(hjust = 0, color = "grey10"),
      strip.background = element_blank(),
      panel.background = element_rect(fill = "grey96", color = NA),
      axis.ticks.y = element_blank(),
      # free x scales put a tick label at the panel edge, which clips without
      # extra room on the right
      plot.margin = margin(10, 30, 10, 10)
    ) +
    labs(x = NULL, y = NULL)
}
