
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
#' @param quantile_group Character vector. Column name(s) that define groups
#'   within which quantiles are computed.
#' @param threshold Type of threshold to use. Defaults to first two quartiles and above the median. 
#' Either "default" or "tercile".
#'
#' @return The input data frame with an additional factor column
#'   \code{quantile_indicator}.
#'
#' @importFrom dplyr group_by mutate ungroup across all_of between case_when
#' @importFrom forcats fct_relevel
#' 
#' @details  Adds a \code{quantile_indicator} column classifying each row as
#' \code{"Weak"} (below 25th percentile), \code{"Emerging"} (25th–50th), or
#' \code{"Strong"} (above 50th), computed within groups defined by
#' \code{quantile_group}. If \code{threshold = "tercile"}, the classification is based on terciles instead of quartiles.
#'
#' @export
classify_quantile <- function(.data, var, threshold = c("default", "tercile")){
  type_threshold <- match.arg(threshold)

  # use the resolved single value from match.arg() for validation
  if (!(type_threshold %in% c("default", "tercile"))) {
    stop("Threshold must be either default or tercile.")
  }

  if(!("country_code" %in% colnames(.data) & "year" %in% colnames(.data))){
    stop("The input data must contain 'country_code' and 'year' columns.")
  }

  if(type_threshold == "default"){
    data_quantile <- .data |>
      # consider only baseline year for quantile classification
      filter(
          year == min(year)
      ) |>
      mutate(
        quantile_indicator = case_when(
          .data[[var]] < quantile(.data[[var]], c(0.25), na.rm = TRUE) ~ "Weak",
          between(
            .data[[var]],
            quantile(.data[[var]], c(0.25), na.rm = TRUE),
            quantile(.data[[var]], c(0.5), na.rm = TRUE)
          ) ~ "Emerging",
          .data[[var]] > quantile(.data[[var]], c(0.5), na.rm = TRUE) ~ "Strong"
        )
      ) |>
      ungroup()
  }else if(type_threshold == "tercile"){
    data_quantile <- .data |>
      # consider only baseline year for quantile classification
      filter(
        year == min(year)
      ) |>
      mutate(
        quantile_indicator = case_when(
          .data[[var]] < quantile(.data[[var]], c(1/3), na.rm = TRUE) ~ "Weak",
          between(
            .data[[var]],
            quantile(.data[[var]], c(1/3), na.rm = TRUE),
            quantile(.data[[var]], c(2/3), na.rm = TRUE)
          ) ~ "Emerging",
          .data[[var]] > quantile(.data[[var]], c(2/3), na.rm = TRUE) ~ "Strong"
        )
      ) |>
      ungroup()
  }
  
  
  data_quantile <- data_quantile |>
    mutate(
      quantile_indicator = forcats::fct_relevel(
        .data[["quantile_indicator"]],
        c("Strong", "Emerging", "Weak")
      )
    )

  data_quantile
}