#' Recode missings to NA on project-relevant variables in a dataset. Function
#' creates list of variables itself using global 'var_dict'
#' @param dataset .df object
#' @return DF with recoded missings
#' @export
recode_missings <- function(dataset, source){


  # Goes through every variable name in var_dict,
  # changes all colnames that start with said name
  # (This is being done due to a range of variables having suffixes like _party_A:I)
  for (var in var_dict[2]){

    var <- as.character(var)

    dataset <- dataset %>%
      mutate_at(vars(starts_with(paste(var))),
              ~ case_when(
                . == 999988 ~ NA,
                . == 999989 ~ NA,
                . == 999990 ~ NA,
                . == 999991 ~ NA,
                . == 999992 ~ NA,
                . == 999995 ~ NA,
                . == 999996 ~ NA,
                . == 999997 ~ NA,
                . == 999998 ~ NA,
                . == 999999 ~ NA,
                . == 9999 ~ NA,
                . == 999 ~ NA,
                . == 998 ~ NA,
                . == 997 ~ NA,
                . == 996 ~ NA,
                . == 995 ~ NA,
                . == 99 ~ NA,
                . == 98 ~ NA,
                . == 97 ~ NA,
                . == 96 ~ NA,
                . == 95 ~ NA,
                TRUE ~ .
              ))
  }

  if (grepl("cses", source)){
    dataset <- dataset %>%
      # missing values lower house election vote shares
      mutate_at(
        vars(voteshare_lower_party_A:voteshare_lower_party_I),
        .funs = list(~case_when(
          . == 0 ~ NA,
          . >= 70 ~ NA,
          TRUE ~ .
        ))
      ) %>%

      # missing values like-dislike scores
      mutate_at(
        vars(likedislike_party_A:likedislike_party_I),
        .funs = list(~case_when(
          . >= 11 ~ NA,
          TRUE ~ .
        ))
      ) %>%

      # missing values upper house vote shares
      mutate_at(
        vars(voteshare_upper_party_A:voteshare_upper_party_I),
        .funs = list(~case_when(
          . == 0 ~ NA,
          . >= 70 ~ NA,
          TRUE ~ .
        ))
      ) %>%

      # missing values presidential vote shares
      mutate_at(
        vars(voteshare_president_party_A:voteshare_president_party_I),
        .funs = list(~case_when(
          . == 0 ~ NA,
          . >= 70 ~ NA,
          TRUE ~ .
        ))
      )

  }

  return(dataset)
}
