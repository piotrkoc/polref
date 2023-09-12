#' CSES Polarisation Index
#'
#' @param dataset
#'
#' @return Country/Year dataset with CSES Polar. Index
#' @export
#'
#' @examples
cses_polarisation_index <- function(dataset){
  # Mean LeftRight-Scores for each party by country/year

  dataset_string <- deparse(substitute(dataset))
  dataset <- replace_voteshare(dataset)

  # Apply Sample Weights
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    leftright_party <-  paste0("leftright_party_", letter)

    dataset[[leftright_party]] <- dataset[[leftright_party]]*dataset[["sample_weight"]]
  }

  dataset <- dataset %>%
    filter(freedom_house < 3) %>%
    select(leftright_party_A:leftright_party_I, country, year, voteshare_lower_party_A:voteshare_lower_party_I) %>%
    pivot_longer(leftright_party_A:leftright_party_I, names_to = "party", values_to = "leftright_score") %>%
    group_by(country, year, party) %>%
    mutate(mean_leftright_score = mean(leftright_score, na.rm = TRUE)) %>%
    select(-leftright_score) %>%
    #filter(!is.nan(mean_leftright_score)) %>%
    ungroup(country, year, party) %>%
    unique() %>%
    pivot_wider(names_from = party,
                values_from = mean_leftright_score)

  # Sum of Vote Share
  vars_voteshare_lower_party <- c("voteshare_lower_party_A", "voteshare_lower_party_B", "voteshare_lower_party_C", "voteshare_lower_party_D", "voteshare_lower_party_E", "voteshare_lower_party_F", "voteshare_lower_party_G", "voteshare_lower_party_H", "voteshare_lower_party_I")
  dataset["totalvote"] <- rowSums(dataset[, vars_voteshare_lower_party], na.rm = TRUE)
  dataset["rmean"] <- 0

  # For each party, do:
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    leftright_party <- paste0("leftright_party_", letter)
    rivi <- paste0("rivi_", letter) # vote * leftright
    rmean_party <- paste0("rmean_", letter) # rmean for each party

    # vote * leftright
    dataset[[rivi]] <- ifelse(!is.na(dataset[[leftright_party]]) & !is.na(dataset[[voteshare_lower_party]]),
                              dataset[[leftright_party]]*dataset[[voteshare_lower_party]], NA)
    # (vote * leftright) sum of all voteshares
    dataset[[rmean_party]] <- ifelse(!is.na(dataset[[rivi]]),
                                     dataset[[rivi]]/dataset[["totalvote"]], NA)
    # Add these scores together to get the weighted average Left-Right score
    dataset[["rmean"]] <- ifelse(!is.na(dataset[[rmean_party]]),
                                 dataset[["rmean"]] + dataset[[rmean_party]], dataset[["rmean"]])

    dataset[["rmean"]] <- ifelse(dataset[["rmean"]] > 0,
                                 dataset[["rmean"]], NA)
  }

  # For each party, do:
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    leftright_party <- paste0("leftright_party_", letter)
    ri_rmean <- paste0("ri_rmean_", letter) # leftright - rmean

    # Left-Right party score - Mean of Left-Right
    dataset[[ri_rmean]] <- ifelse(!is.na(dataset[[leftright_party]]) & !is.na(dataset[["rmean"]]),
                                  dataset[[leftright_party]] - dataset[["rmean"]], NA)
    # Divide by 5, Square, multiply with party vote share
    dataset[[ri_rmean]] <- ifelse(!is.na(dataset[[ri_rmean]]) &!is.na(dataset[[voteshare_lower_party]]),
                                  (((dataset[[ri_rmean]])/5)^2)*dataset[[voteshare_lower_party]], NA)

  }

  # Sum all party scores, take the root
  vars_ri_rmean <- c("ri_rmean_A", "ri_rmean_B", "ri_rmean_C", "ri_rmean_D", "ri_rmean_E", "ri_rmean_F", "ri_rmean_G", "ri_rmean_H", "ri_rmean_I")
  dataset[["polarisation_index"]] <- sqrt(rowSums(dataset[, vars_ri_rmean], na.rm = TRUE))
  dataset[["polarisation_index"]] <- ifelse(dataset[["polarisation_index"]] > 0,
                                            dataset[["polarisation_index"]], NA)

  dataset <- dataset %>% select(country, year, polarisation_index)
  colnames(dataset) <- c("country", "year", "pol_score")
  dataset[["measure"]] <- "polarisation_index"
  dataset[["dataset"]] <- dataset_string
  # leftright_party_A:leftright_party_I, voteshare_lower_party_A:voteshare_lower_party_I

  # Some scores are exactly the same, some are slightly different (rounding & weighting issues), but some are completely off. This remains when introducing sample weights
  # Example data sometimes says "Presidential election", however, there is a score for example for France 2002 or US 2012 which we onyl get when using presidential election scores.
  # Dalton sometimes says "No LR score", when data is actually available (e.g. Belgium 1999). Not sure, why he says that.

  return(dataset)
}
