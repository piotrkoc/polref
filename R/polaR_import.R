#' Import polaR datasets
#' Import the CSES Integrated Dataset.
#' Takes .dta files, renames and formats package-relevant variables.
#'
#' @param path File path string to a .dta file
#' @param source The source of the data, e.g. CSES 5
#' @param keep_all_ess defaults to TRUE, keep all variables in the ESS or reduce the dataset size to be able to handle the data
#'
#' @return CSES IMD dataset that is ready for use with other functions
#'
#' @example
#' @export
polaR_import <- function(source, path, keep_all = TRUE){
  dataset <- read_stata(path) %>%
    zap_labels()

  # Rename Variables into corresponding package vars, see polaR:::var_dict
  dataset <- rename_variables(dataset, paste(source))

  # Code Year variables
  dataset <- code_years(dataset)

  # reformat missings to NA
  dataset <- recode_missings(dataset, source)

  dataset <- country_codes(dataset, source)


  if (keep_all == FALSE){
    # ESS is too big, filtering only variables occurring in the dataset
      dataset <- dataset %>%
        select(starts_with(var_dict$name_dict))
  }

  return(dataset)

}
