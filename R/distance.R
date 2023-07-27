#' Calculate weighted and unweighted Distance measure
#' @param dataset Imported Dataset with adapted Varnames, originally CSES IMD
#' @return Dataset with added variables distance and distance_wgt
#' @export
distance <- function(dataset){

  backup_dataset <- dataset

  dataset <- dataset %>%
    # missing values lower house election vote shares
    mutate_at(
      vars(voteshare_lower_party_A:voteshare_lower_party_I),
      .funs = list(~case_when(
        . == 0 ~ NA,
        . >= 70 ~ NA,
        TRUE ~ .
      ))
    ) %>%

    # missing values like-dislike scores
    mutate_at(
      vars(likedislike_party_A:likedislike_party_I),
      .funs = list(~case_when(
        . >= 11 ~ NA,
        TRUE ~ .
      ))
    ) %>%

    # missing values upper house vote shares
    mutate_at(
      vars(voteshare_upper_party_A:voteshare_upper_party_I),
      .funs = list(~case_when(
        . == 0 ~ NA,
        . >= 70 ~ NA,
        TRUE ~ .
      ))
    ) %>%

    # missing values presidential vote shares
    mutate_at(
      vars(voteshare_president_party_A:voteshare_president_party_I),
      .funs = list(~case_when(
        . == 0 ~ NA,
        . >= 70 ~ NA,
        TRUE ~ .
      ))
    )

  # replace lower house scores with presidential scores where relevant
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    voteshare_president_party <- paste0("voteshare_president_party_", letter)

    dataset[[voteshare_lower_party]] <- ifelse(
      is.na(dataset[[voteshare_lower_party]]) & !is.na(dataset[[voteshare_president_party]]),
      dataset[[voteshare_president_party]],
      dataset[[voteshare_lower_party]])

  }

  vars_likedislike_party <- c("likedislike_party_A", "likedislike_party_B", "likedislike_party_C", "likedislike_party_D", "likedislike_party_E", "likedislike_party_F", "likedislike_party_G", "likedislike_party_H", "likedislike_party_I")
  vars_voteshare_lower_party <- c("voteshare_lower_party_A", "voteshare_lower_party_B", "voteshare_lower_party_C", "voteshare_lower_party_D", "voteshare_lower_party_E", "voteshare_lower_party_F", "voteshare_lower_party_G", "voteshare_lower_party_H", "voteshare_lower_party_I")

  dataset["partynumber"] <- apply(dataset[vars_likedislike_party], MARGIN = 1, function(x) sum(!is.na(x)))
  dataset["votenumber"] <- apply(dataset[vars_voteshare_lower_party], MARGIN = 1, function(x) sum(!is.na(x)))
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    likedislike_party <- paste0("likedislike_party_", letter)
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    complete <- paste0("complete", letter)

    dataset[[complete]] <- ifelse(
      !is.na(dataset[[likedislike_party]]) & !is.na(dataset[[voteshare_lower_party]]),
      1,
      0)
  }

  vars_complete <- c("completeA", "completeB", "completeC", "completeD", "completeE", "completeF", "completeG", "completeH", "completeI")

  dataset$completenumber <- ifelse(dataset$votenumber != 0 & dataset$partynumber != 0,
                                   rowSums(dataset[, vars_complete]),
                                   NA)
  dataset$completenumber <- ifelse(dataset$partynumber == 0,
                                   0,
                                   dataset$completenumber)
  dataset$completenumber <- ifelse(dataset$votenumber == 0,
                                   0,
                                   dataset$completenumber)

  ## Weighted
  # Correct Vote Shares
  dataset["sumvotes"] <- ifelse(dataset[, "votenumber"] != 0, rowSums(dataset[, vars_voteshare_lower_party], na.rm = TRUE), NA)

  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    complete <- paste0("complete", letter)

    dataset[["sumvotes"]] <- ifelse(
      dataset[[complete]] != 1 & !is.na(dataset[[voteshare_lower_party]]),
      dataset[["sumvotes"]] - dataset[[voteshare_lower_party]],
      dataset[["sumvotes"]])
  }

  ### Rounding differences from here on
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    vote <- paste0("vote", letter)

    dataset[[vote]] <- dataset[[voteshare_lower_party]]/dataset[["sumvotes"]]

  }

  # Identify party column with highest like score
  dataset$mostlikedparty <- colnames(dataset[vars_likedislike_party])[max.col(replace(dataset[vars_likedislike_party], is.na(dataset[vars_likedislike_party]), -Inf),ties.method="first")]
  dataset <- dataset %>%
    # Remove most liked party when the values are NA
    mutate(mostlikedparty = case_when(
      endsWith(mostlikedparty, "A") & is.na(likedislike_party_A) ~ NA,
      endsWith(mostlikedparty, "B") & is.na(likedislike_party_B) ~ NA,
      endsWith(mostlikedparty, "C") & is.na(likedislike_party_C) ~ NA,
      endsWith(mostlikedparty, "D") & is.na(likedislike_party_D) ~ NA,
      endsWith(mostlikedparty, "E") & is.na(likedislike_party_E) ~ NA,
      endsWith(mostlikedparty, "F") & is.na(likedislike_party_F) ~ NA,
      endsWith(mostlikedparty, "G") & is.na(likedislike_party_G) ~ NA,
      endsWith(mostlikedparty, "H") & is.na(likedislike_party_H) ~ NA,
      endsWith(mostlikedparty, "I") & is.na(likedislike_party_I) ~ NA,
      TRUE ~ mostlikedparty
    )) %>%
    # Add score of the most liked party
    mutate(mostlikedpartyscore = case_when(
      endsWith(mostlikedparty, "A") ~ likedislike_party_A,
      endsWith(mostlikedparty, "B") ~ likedislike_party_B,
      endsWith(mostlikedparty, "C") ~ likedislike_party_C,
      endsWith(mostlikedparty, "D") ~ likedislike_party_D,
      endsWith(mostlikedparty, "E") ~ likedislike_party_E,
      endsWith(mostlikedparty, "F") ~ likedislike_party_F,
      endsWith(mostlikedparty, "G") ~ likedislike_party_G,
      endsWith(mostlikedparty, "H") ~ likedislike_party_H,
      endsWith(mostlikedparty, "I") ~ likedislike_party_I,
      TRUE ~ NA
    ))

  # Like-Minus-Like scores for each party
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    likeminuslike <- paste0("likeminuslike_", letter)
    likedislike_party <- paste0("likedislike_party_", letter)

    # includes the inparty at this point
    dataset[[likeminuslike]] <- (dataset[[likedislike_party]] - dataset[["mostlikedpartyscore"]])^2

  }

  # Square Root of the sum of all likeminuslike scores divided by the number of parties
  vars_likeminuslike <- c("likeminuslike_A", "likeminuslike_B", "likeminuslike_C", "likeminuslike_D", "likeminuslike_E", "likeminuslike_F", "likeminuslike_G", "likeminuslike_H", "likeminuslike_I")
  dataset$distance <- sqrt(rowSums(dataset[vars_likeminuslike], na.rm = TRUE)/dataset$partynumber)

  # Weighted Like-Minus-Like Scores
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    likeminuslike_wgt <- paste0("likeminuslike_", letter, "_wgt")
    likedislike_party <- paste0("likedislike_party_", letter)
    vote_party <- paste0("vote", letter)

    # includes the inparty at this point
    dataset[[likeminuslike_wgt]] <- dataset[[vote_party]]*(dataset[[likedislike_party]] - dataset[["mostlikedpartyscore"]])^2

  }

  # Square Root of the sum of all weighted likeminuslike scores
  vars_likeminuslike_wgt <- c("likeminuslike_A_wgt", "likeminuslike_B_wgt", "likeminuslike_C_wgt", "likeminuslike_D_wgt", "likeminuslike_E_wgt", "likeminuslike_F_wgt", "likeminuslike_G_wgt", "likeminuslike_H_wgt", "likeminuslike_I_wgt")
  dataset$distance_wgt <- sqrt(rowSums(dataset[vars_likeminuslike_wgt], na.rm = TRUE))

  backup_dataset$distance <- dataset$distance
  backup_dataset$distance_wgt <- dataset$distance_wgt

  dataset <- backup_dataset

  return(dataset)

}
