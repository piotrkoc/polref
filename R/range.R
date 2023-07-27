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

  # Only apply max function when not all values are NA, thanks to coffeinjunky@stackoverflow
  my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

  # Min and Max ratings of parties
  data["min_party"] <- apply(data[, all_parties], 1, my.min)
  data["max_party"] <- apply(data[, all_parties], 1, my.max)

  # All Inf to NA
  #data[sapply(data, is.infinite)] <- NA

  data["range"] <- data["max_party"] - data["min_party"]

  return(data)

}
