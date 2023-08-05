#' Rename Variables
#' Renames selected variables in a dataframe to correspond to the package convention, see var_dict
#' @param dataset A dataframe of one of the package's dataset
#' @param source String of dataset, to be found in var_dict
#'
#' @return dataframe with renamed variables
#' @export
#'
#' @examples rename_variables(df, "cses_5")
rename_variables <- function(dataset, source){

  # Get list of dataset variables and corresponding names in our datastructure
  var_dict <- var_dict[c("name_dict", paste(source))]

  # Rename each variable, if existant, to our variable name
  for(x in c(1:nrow(var_dict))){
    if(var_dict[[x,2]] != ""){
      #dataset[paste(var_dict[x,2])] <- dataset[paste(var_dict[x,2])]
      colnames(dataset) <- sub(var_dict[x,2], var_dict[x,1], colnames(dataset))
    }}

  return(dataset)
}
