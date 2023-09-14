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

  if("country_year_id" %in% colnames(dataset)){
  # (for now only) CSES differs between Wallonia & Flanders, this needs to be reflected in the country var
  # Creating helper var for Belgian region:
  dataset <- dataset %>%
    mutate(belgium = case_when(
      startsWith(country_year_id, "BELF") ~ "Flanders",
      startsWith(country_year_id, "BELW") ~ "Wallonia",
      TRUE ~ NA
    ))}

  if (as.character(source) %in% colnames(country_dict)){

    dataset[["country"]] <- NA

    for(x in c(1:nrow(country_dict))){

        dataset <- dataset %>%
          mutate(country = case_when(
            country_orig == country_dict[[x, source]] ~ country_dict[[x, "country"]],
            TRUE ~ country
          ))

        if("belgium" %in% colnames(dataset)){
        # Replace Belgium with its region where applicable
        dataset <- dataset %>%
          mutate(country = case_when(
            country == "Belgium" & belgium == "Flanders" ~ "Belgium (Flanders)",
            country == "Belgium" & belgium == "Wallonia" ~ "Belgium (Wallonia)",
            country == "Belgium" & is.na(belgium) ~ "Belgium",
            TRUE ~ country
          ))}

    }
  } else {print("Countries not defined in `country_dict`")}

  return(dataset)
}
