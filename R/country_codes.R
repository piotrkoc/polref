#' Re-codes country names according to country_dict
#'
#' @param dataset A dataframe of one of the package's dataset
#' @param source String of dataset, to be found in var_dict
#'
#' @return Dataframe with added country variables
#' @export
#'
#' @examples country_codes(df, "ches")
country_codes <- function (dataset, source){

  if (as.character(source) %in% colnames(country_dict)){

    dataset[["country_iso2c"]] <- NA

    for(x in c(1:nrow(country_dict))){

        dataset <- dataset %>%
          mutate(country_iso2c = case_when(
            country_orig == country_dict[[x, source]] ~ country_dict[[x, "country_iso2c"]],
            TRUE ~ country_iso2c

          ))
    }
  }

  dataset[["country"]] <- countrycode(dataset[["country_iso2c"]],
                                           origin = "iso2c",
                                           destination = "country.name",
                                           custom_match = c('XK' = 'Kosovo',
                                                            'GDR' = 'German Democratic Republic',
                                                            'NIE' = 'Northern Ireland'))

  return(dataset)
}
