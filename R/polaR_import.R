#' Import polaR datasets
#' Import the CSES Integrated Dataset.
#' Takes .dta files, renames and formats package-relevant variables.
#'
#' @param path File path string to a .dta file
#' @param source The source of the data, e.g. CSES 5
#'
#' @return CSES IMD dataset that is ready for use with other functions
#'
#' @example
#' @export
polaR_import <- function(source, path){
  dataset <- read_stata(path) %>%
    zap_labels()

  # Rename Variables into corresponding package vars, see polaR:::var_dict
  dataset <- rename_variables(dataset, paste(source)) %>%
    mutate(year = format(year, format = "%Y"))

  # reformat missings to NA
  dataset <- recode_missings(dataset)

  dataset <- country_codes(dataset, source)

  # ESS is too big, filtering only variables occurring in the dataset
  if (source == "ess"){
    dataset <- dataset %>%
      select(starts_with(var_dict$name_dict))
  }

  return(dataset)

}
