#' Range
#'
#' @param data Dataframe
#' @param measure Individual level variable without party suffix
#'
#' @return Dataset wih individual level range score
#' @export
#'
#' @examples cses_5 <- range(cses_5, "leftright")
range <- function(data, measure){

  # take measure and form variable names
  all_parties <- c("_party_A", "_party_B", "_party_C", "_party_D", "_party_E", "_party_F", "_party_G", "_party_H", "_party_I")
  all_parties <- paste(measure, all_parties, sep="")

  # Turn off warnings due to "Inf" warning in apply
  defaultW <- getOption("warn")
  options(warn = -1)

  # Min and Max ratings of parties
  data["min_party"] <- apply(data[, all_parties], 1, min, na.rm = TRUE)
  data["max_party"] <- apply(data[, all_parties], 1, max, na.rm = TRUE)

  # All Inf to NA
  data[sapply(data, is.infinite)] <- NA

  # Warnigns to previous state
  options(warn = defaultW)

  data["range"] <- data["max_party"] - data["min_party"]

  return(data)

}
