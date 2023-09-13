#' Aggregate mean of polarization measures on a Country-Year Level
#' @param data Dataset with respondents as observation units
#' @param new_var_name Name of the new variable
#' @param polarization_var Variable with individual polarization scores
#' @return Dataset with mean polarization scores for Country-Year units
#' @export
mean_aggregate <- function(data, polarization_var){

  dataset_string <- deparse(substitute(data))

  data <- aggregate(data[[polarization_var]] ~ data[["country"]] + data[["year"]],
                    data = data,
                    FUN = function(x) mean(x))

  colnames(data) <- c("country", "year", "pol_score")
  data[["measure"]] <- paste0(polarization_var)
  data[["dataset"]] <- dataset_string

  return(data)
}
