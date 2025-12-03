#' Integrates Mean and SD to a single column
#'
#' @param data #lipidomics file
#'
#' @returns A data.frame/tibble.
#' #export
#'
#' #examples
create_table_descriptive_stats <- function(data) {
    data |>
      dplyr::group_by(metabolite) |>
      dplyr::summarise(across(value, list(mean = mean, sd = sd))) |>
      dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 1))) |>
      dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
      dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD)
  }

#' Creating a histogram of variables
#'
#' @param data #lipodomics data
#'
#' @returns # a plot/histogram object
#' #export
#'
#' #examples
create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(
      ggplot2::aes(x = value)
    ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_minimal()
}

#' Cleaning of data (1 variable to have 1 mean value)
#'
#' @param data #The lipidomics data
#'
#' @returns A data frame
#' #export
#'
#' #examples
clean <- function(data){
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}

#' Preprocessiing data
#'
#' @param data Lipodomics
#'
#' @returns A data frame
#' #export
#'
#' #examples
preprocess <- function(data){
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}

#' Applying GLM to all metabolite data
#'
#' @param data # Metabolite column
#' @param model #GLM
#'
#' @returns # Model output
#' #export
#'
#' #examples
fit_model <- function(data, model) {
  glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = everything()
    )
}

#' Model results for variable cholesterol
#'
#' @param data #Lipidomics
#'
#' @returns # Model calcs
#' #export
#'
#' #examples
create_model_results <- function(data) {
  data |>
    dplyr::filter(metabolite == "Cholesterol") |>
    preprocess() |>
    fit_model(class ~ value)
}

