#' Convert selected columns of a dataframe to \code{factor}
#'
#' This function converts user-selected columns of a dataframe to class \code{factor}.
#'
#' @param data a dataframe.
#' @param variables a vector of character strings naming the columns of \code{data} to be converted to
#' class \code{factor}.
#' @return a dataframe with the selected columns of \code{data} (provided by \code{variables} argument)
#' converted to class \code{factor}.
#' @export
#' @examples
#' data(fruitfly)
#' str(fruitfly)
#' newfruitfly <- toFactor(fruitfly, variables = c("ID", "Partners", "Type"))
#' # Check selected variables have been converted to variables
#' str(newfruitfly)

toFactor <- function(data, variables){

  for(varName in variables)data[[varName]] <- factor(data[[varName]])
  return(data)

}

