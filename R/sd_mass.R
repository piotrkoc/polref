#' Aggregate standard deviation of individual level scores on a Country-Year Level
#' @param data Dataset with respondents as observation units
#' @param new_var_name Name of the new variable
#' @param score_var Variable with individual scores to compute SD from
#' @param country_var Country variable in the dataset
#' @param year_var Year variable in the dataset
#' @return Dataset with polarization scores for Country-Year units
#' @export
sd_mass <- function(data, score_var){

  dataset_string <- deparse(substitute(data))

  data <- aggregate(data[[score_var]] ~ data[["country"]] + data[["year"]],
                    data = data,
                    FUN = function(x) sd(x))

  colnames(data) <- c("country", "year", "pol_score")
  data[["measure"]] <- "sd_mass"
  data[["dataset"]] <- dataset_string

  return(data)
}
