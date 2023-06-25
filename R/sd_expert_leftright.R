#' Standard deviation of expert party evaluations
#' @param data Dataset with expert lr expert evaluations as columns
#' @return Dataset with mean polarization scores for Country-Year units
#' @export
sd_expert_leftright <- function(data){

  leftright_expert_parties <- c("leftright_expert_party_A", "leftright_expert_party_B", "leftright_expert_party_C", "leftright_expert_party_D", "leftright_expert_party_E", "leftright_expert_party_F", "leftright_expert_party_G", "leftright_expert_party_H", "leftright_expert_party_I")

  dataset_string <- deparse(substitute(data))

  data <- data %>%
    select(country_full, year, leftright_expert_party_A:leftright_expert_party_I) %>%
    unique()

  data["pol_score"] <- apply(data[, leftright_expert_parties], 1, sd, na.rm = TRUE)

  data <- data[, !names(data) %in% leftright_expert_parties]

  colnames(data) <- c("country", "year", "pol_score")
  data[["measure"]] <- "sd(expert lr)"
  data[["dataset"]] <- dataset_string

  return(data)
}
