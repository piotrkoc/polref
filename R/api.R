#' API measure
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
api <- function(dataset){

  dataset_string <- deparse(substitute(dataset))

  # Replace lower house vote shares with presidential voteshares, if missing
  dataset <- replace_voteshare(dataset)

  # Apply Sample Weights
  #for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
  #  likedislike <-  paste0("likedislike_party_", letter)
#
  #  dataset[[likedislike]] <- dataset[[likedislike]]*dataset[["sample_weight"]]
  #}

  dataset <- dataset %>%
    select(country, year, inparty_represent, likedislike_party_A:likedislike_party_I, voteshare_lower_party_A:voteshare_lower_party_I, id_party_A:id_party_I) %>%
    # Party-Like-Dislike means per country & year
    group_by(country, year, inparty_represent) %>%
    mutate(likedislike_party_A = mean(likedislike_party_A, na.rm = TRUE)) %>%
    mutate(likedislike_party_B = mean(likedislike_party_B, na.rm = TRUE)) %>%
    mutate(likedislike_party_C = mean(likedislike_party_C, na.rm = TRUE)) %>%
    mutate(likedislike_party_D = mean(likedislike_party_D, na.rm = TRUE)) %>%
    mutate(likedislike_party_E = mean(likedislike_party_E, na.rm = TRUE)) %>%
    mutate(likedislike_party_F = mean(likedislike_party_F, na.rm = TRUE)) %>%
    mutate(likedislike_party_G = mean(likedislike_party_G, na.rm = TRUE)) %>%
    mutate(likedislike_party_H = mean(likedislike_party_H, na.rm = TRUE)) %>%
    mutate(likedislike_party_I = mean(likedislike_party_I, na.rm = TRUE)) %>%
    unique() %>%
    ungroup(country, year, inparty_represent) %>%
    mutate(inparty = case_when(
      inparty_represent == id_party_A ~ "A",
      inparty_represent == id_party_B ~ "B",
      inparty_represent == id_party_C ~ "C",
      inparty_represent == id_party_D ~ "D",
      inparty_represent == id_party_E ~ "E",
      inparty_represent == id_party_F ~ "F",
      inparty_represent == id_party_G ~ "G",
      inparty_represent == id_party_H ~ "H",
      inparty_represent == id_party_I ~ "I",
      TRUE ~ NA
    )) %>%
    # Filter out parties without a letter
    filter(!is.na(inparty))

  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    likedislike <-  paste0("likedislike_party_", letter)
    voteshare <-  paste0("voteshare_lower_party_", letter)

    # get inparty like
    dataset[dataset$inparty == letter, "inparty_like"] <- dataset[dataset$inparty == letter, likedislike]
    # and inparty voteshare
    dataset[dataset$inparty == letter, "inparty_vote"] <- dataset[dataset$inparty == letter, voteshare]

    # All parties except inparty, mean out-party like
    ## First, define all parties
    vars_likedislike_party <- c("likedislike_party_A", "likedislike_party_B", "likedislike_party_C", "likedislike_party_D", "likedislike_party_E", "likedislike_party_F", "likedislike_party_G", "likedislike_party_H", "likedislike_party_I")
    ## Then remove individual party from that list
    vars_likedislike_party <- vars_likedislike_party[vars_likedislike_party != likedislike]
    ## Insert mean of all remaining parties
    dataset[dataset$inparty == letter, "mean_outparty_like"] <- rowMeans(dataset[dataset$inparty == letter, vars_likedislike_party], na.rm = TRUE)

    # Same for outparty voteshares
    ## First, define all parties
    vars_voteshare_lower_party <- c("voteshare_lower_party_A", "voteshare_lower_party_B", "voteshare_lower_party_C", "voteshare_lower_party_D", "voteshare_lower_party_E", "voteshare_lower_party_F", "voteshare_lower_party_G", "voteshare_lower_party_H", "voteshare_lower_party_I")
    ## Then remove individual party from that list
    vars_voteshare_lower_party <- vars_voteshare_lower_party[vars_voteshare_lower_party != voteshare]
    ## Insert mean of all remaining parties
    dataset[dataset$inparty == letter, "sum_outparty_votes"] <- rowSums(dataset[dataset$inparty == letter, vars_voteshare_lower_party], na.rm = TRUE)

  }

  for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
    likedislike_outparty <-  paste0("likedislike_party_", letter)
    APn <-  paste0("APn_", letter)
    voteshare_outparty <- paste0("voteshare_lower_party_", letter)

    # Inparty-Like minus Outparty-Like, weighted by Outparty-Voteshare relative to sum of all outparty votes
    dataset[[APn]] <- (dataset[["inparty_like"]] - dataset[[likedislike_outparty]])*(dataset[[voteshare_outparty]]/dataset[["sum_outparty_votes"]])

  }

  # Sum up all weighted outparty scores by party
  vars_APn <- c("APn_A", "APn_B", "APn_C", "APn_D", "APn_E", "APn_F", "APn_G", "APn_H", "APn_I")
  dataset[["APn"]] <- rowSums(dataset[, vars_APn], na.rm = TRUE)

  # Weight by Inparty Voteshare
  dataset[["API"]] <- dataset[["APn"]]*(dataset[["inparty_vote"]]/(dataset[["inparty_vote"]] + dataset[["sum_outparty_votes"]]))

  # Sum up weighted scores of all parties
  dataset <- dataset %>%
    group_by(country, year) %>%
    filter(APn > 0 & !is.na(API)) %>%
    summarise(pol_score = sum(API, na.rm = TRUE))

  dataset[["measure"]] <- "api"
  dataset[["dataset"]] <- dataset_string
  dataset <- dataset[, c("country", "year", "pol_score", "dataset", "measure")]

  return(dataset)

}
