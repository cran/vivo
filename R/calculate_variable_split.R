#' Internal Function for Split Points for Selected Variables
#'
#' This function calculate candidate splits for each selected variable.
#' For numerical variables splits are calculated as percentiles
#' (in general uniform quantiles of the length \code{grid_points}).
#' For all other variables splits are calculated as unique values.
#'
#'
#' @param data validation dataset. Is used to determine distribution of observations.
#' @param variables names of variables for which splits shall be calculated
#' @param grid_points number of points used for response path
#'
#' @return A named list with splits for selected variables
#' @importFrom stats predict
#' @importFrom stats quantile
#' @author Przemyslaw Biecek
#'
#' @note This function is a copy of \code{calculate_varaible_split()} from \code{ingredients} package with small change.
#'
#'@export
#'



calculate_variable_split <- function(data, variables = colnames(data), grid_points = 101) {
  variable_splits <- lapply(variables, function(var) {
    selected_column <- data[,var]
    if (is.numeric(selected_column)) {
      seq(from = min(selected_column), to = max(selected_column), length.out = grid_points)
    } else {
      sort(unique(selected_column))
    }
  })
  names(variable_splits) <- variables
  variable_splits
}
