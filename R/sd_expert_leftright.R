#' Standard deviation of expert party evaluations
#' @param data Dataset with expert lr expert evaluations as columns
#' @return Dataset with mean polarization scores for Country-Year units
#' @export
sd_expert_leftright <- function(dataset){

  leftright_expert_parties <- c("leftright_expert_party_A", "leftright_expert_party_B", "leftright_expert_party_C", "leftright_expert_party_D", "leftright_expert_party_E", "leftright_expert_party_F", "leftright_expert_party_G", "leftright_expert_party_H", "leftright_expert_party_I")

  dataset_string <- deparse(substitute(dataset))

  dataset <- dataset %>%
    select(country, year, leftright_expert_party_A:leftright_expert_party_I) %>%
    unique()

  dataset["pol_score"] <- apply(dataset[, leftright_expert_parties], 1, sd, na.rm = TRUE)

  dataset <- dataset[, !names(dataset) %in% leftright_expert_parties]

  colnames(dataset) <- c("country", "year", "pol_score")
  dataset[["measure"]] <- "sd(expert lr)"
  dataset[["dataset"]] <- dataset_string

  return(dataset)
}
