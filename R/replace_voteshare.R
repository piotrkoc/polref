#' Replace party vote share with president vote share where relevant
#'
#' @param dataset
#'
#' @return dataset with adapted voteshare measure
#' @export
#'
#' @examples cses_5 <- replace_voteshare(cses_5)
replace_voteshare <- function(dataset){

# replace lower house scores with presidential scores where relevant
for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
  voteshare_lower_party <- paste0("voteshare_lower_party_", letter)
  voteshare_president_party <- paste0("voteshare_president_party_", letter)

  dataset[[voteshare_lower_party]] <- ifelse(
    is.na(dataset[[voteshare_lower_party]]) & !is.na(dataset[[voteshare_president_party]]),
    dataset[[voteshare_president_party]],
    dataset[[voteshare_lower_party]])

}

return(dataset)

}
