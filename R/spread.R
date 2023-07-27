#' Calculate weighted and unweighted Wagner measure
#' @param dataset Imported Dataset with adapted Varnames, originally CSES IMD
#' @return Dataset with added variables affpol and affpolwgt (plus helper variables)
#' @export
spread <- function(dataset){

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


  ## Affective Polarization}
  dataset["meanlike"] <- rowMeans(dataset[, vars_likedislike_party], na.rm = TRUE)

  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    affpol <- paste0("affpol", letter)
    likedislike_party <- paste0("likedislike_party_", letter)

    dataset[[affpol]] <- (dataset[[likedislike_party]] - dataset[["meanlike"]])^2

  }

  vars_affpol <- c("affpolA",
                   "affpolB",
                   "affpolC",
                   "affpolD",
                   "affpolE",
                   "affpolF",
                   "affpolG",
                   "affpolH",
                   "affpolI")

  dataset["sumaffpol"] <- rowSums(dataset[, vars_affpol], na.rm = TRUE)
  dataset["affpol"] <- sqrt(dataset[["sumaffpol"]]/dataset[["partynumber"]])

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

  ### Rounding differences after from here on
  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
    vote <- paste0("vote", letter)

    dataset[[vote]] <- dataset[[voteshare_lower_party]]/dataset[["sumvotes"]]

  }


  dataset[["allvotes"]] <- rowSums(dataset[, c("voteA", "voteB", "voteC", "voteD", "voteE", "voteF", "voteG", "voteH", "voteI")], na.rm = TRUE)

  dataset$helpA <- dataset$likedislike_party_A*dataset$voteA
  dataset$helpB <- dataset$likedislike_party_B*dataset$voteB
  dataset$helpC <- dataset$likedislike_party_C*dataset$voteC
  dataset$helpD <- dataset$likedislike_party_D*dataset$voteD
  dataset$helpE <- dataset$likedislike_party_E*dataset$voteE
  dataset$helpF <- dataset$likedislike_party_F*dataset$voteF
  dataset$helpG <- dataset$likedislike_party_G*dataset$voteG
  dataset$helpH <- dataset$likedislike_party_H*dataset$voteH
  dataset$helpI <- dataset$likedislike_party_I*dataset$voteI

  dataset$weightedmeanlike <- ifelse(
    dataset$completenumber != 0,
    rowSums(dataset[, c("helpA", "helpB", "helpC", "helpD", "helpE", "helpF", "helpG", "helpH", "helpI")], na.rm = TRUE),
    NA
  )

  dataset$affpolAwgt <- dataset$voteA * ((dataset$likedislike_party_A - dataset$weightedmeanlike)^2)
  dataset$affpolBwgt <- dataset$voteB * ((dataset$likedislike_party_B - dataset$weightedmeanlike)^2)
  dataset$affpolCwgt <- dataset$voteC * ((dataset$likedislike_party_C - dataset$weightedmeanlike)^2)
  dataset$affpolDwgt <- dataset$voteD * ((dataset$likedislike_party_D - dataset$weightedmeanlike)^2)
  dataset$affpolEwgt <- dataset$voteE * ((dataset$likedislike_party_E - dataset$weightedmeanlike)^2)
  dataset$affpolFwgt <- dataset$voteF * ((dataset$likedislike_party_F - dataset$weightedmeanlike)^2)
  dataset$affpolGwgt <- dataset$voteG * ((dataset$likedislike_party_G - dataset$weightedmeanlike)^2)
  dataset$affpolHwgt <- dataset$voteH * ((dataset$likedislike_party_H - dataset$weightedmeanlike)^2)
  dataset$affpolIwgt <- dataset$voteI * ((dataset$likedislike_party_I - dataset$weightedmeanlike)^2)

  dataset$affpolwgt <- ifelse(
    dataset$completenumber != 0,
    rowSums(dataset[, c("affpolAwgt", "affpolBwgt", "affpolCwgt", "affpolDwgt", "affpolEwgt", "affpolFwgt", "affpolGwgt", "affpolHwgt", "affpolIwgt")], na.rm = TRUE),
    NA
  )

  dataset$affpolwgt <- sqrt(dataset$affpolwgt)

  dataset$affpolwgt <- ifelse(
    dataset$completenumber == 1,
    NA,
    dataset$affpolwgt
  )

  backup_dataset$spread <- dataset$affpol
  backup_dataset$spread_wgt <- dataset$affpolwgt

  dataset <- backup_dataset

  return(dataset)

}
