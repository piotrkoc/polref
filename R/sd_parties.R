#' SD of Party Perception
#'
#' @param dataset Dataframe
#' @param measure Individual level variable without party suffix
#'
#' @return Dataset wih individual level sd score
#' @export
#'
#' @examples cses <- sd_parties(cses, "leftright")
sd_parties <- function(dataset, measure){

  # take measure and form variable names
  all_parties <- c("_party_A", "_party_B", "_party_C", "_party_D", "_party_E", "_party_F", "_party_G", "_party_H", "_party_I")
  all_parties <- paste(measure, all_parties, sep="")

  dataset["partynumber"] <- apply(dataset[all_parties], MARGIN = 1, function(x) sum(!is.na(x)))
  dataset["sd_parties"] <- apply(dataset[, all_parties], 1, sd, na.rm = TRUE)

  # All Inf to NA
  #dataset[sapply(dataset, is.infinite)] <- NA

  return(dataset)

}
