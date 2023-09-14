code_years <- function (dataset){

  # Convert full interview dates in POSIX format to just years
  if(is.POSIXt(dataset$YYYY)){
    dataset <- dataset %>%
      mutate(YYYY = year(YYYY))
  }

  # Create empty election date variable for datasets without
  if("edate" %in% colnames(dataset)){
    dataset <- dataset %>% mutate(edate = case_when(
      !is.na(edate) ~ paste(edate),
      TRUE ~ NA))
  } else {
    dataset$edate <- NA
  }

  # If there's an election date, this function takes the exact date, as there are cases where two elections happen in the same year
  # For every other case, 01. January is used as the date variable
  dataset <- dataset %>%
    mutate(year = case_when(
      !is.na(edate) ~ paste(edate),
      !is.na(YYYY) ~ paste(YYYY, "-01-01"),
      TRUE ~ NA
      )) %>%
    mutate(year = ymd(year))

  return(dataset)

}
