#' A wrapper function for \code{tapply} which returns a dataframe
#'
#' Applies a function to each group of values within each unique combination of factor levels, but
#' returns a dataframe rather than the multi-way array returned by \code{tapply}.
#'
#' @param data a dataframe
#' @param y a data vector in \code{data}
#' @param factors vector of one or more factor names (character strings)
#' @param FUN the function to be applied, or NULL. In the case of functions like '+', '%*%', etc.,
#' the function name must be backquoted or quoted. If FUN is NULL, tapply returns a vector which can
#' be used to subscript the multi-way array tapply normally produces.
#' @return a dataframe, including a column for each factor in \code{factors} and a column for each
#' value returned by \code{FUN} when applied to \code{y}
#' @details
#' This function computes the mean and standard error of the observations in a vector.
#' NAs are removed before the calculations are performed.
#' @seealso \code{\link{meanse}}, \code{\link{tapply}}
#' @export
#' @examples
#' data(splitBlockData)
#'
#' # For each unique combination of the levels of Disease and Organ:
#'
#' # 1. Calculate means and their SEs
#' groupSummaries(splitBlockData, y="logAUC", factors=c("Disease", "Organ"), FUN=meanse)
#
#' # 2. Get quartiles
#' groupSummaries(splitBlockData, y="logAUC", factors=c("Disease", "Organ"),
#'   FUN=function(x)quantile(x, probs=c(0.25, 0.5, 0.75)))

groupSummaries <- function(data, y, factors, FUN){

  if(length(factors)==1) index <- data[factors]
  else                   index <- apply(data[factors], 1, paste, collapse=":")
  if(missing(FUN)) stop("Function must be supplied to FUN!", .call=FALSE)
  FUN <- if (!is.null(FUN)) match.fun(FUN)
  summaryStatsList <- tapply(data[[y]], index, FUN, simplify = FALSE)
  summaryTable  <- do.call('rbind', summaryStatsList)
  if(ncol(summaryTable)==1) colnames(summaryTable) <- deparse(substitute(FUN))
  groupsTable <- do.call("rbind", strsplit(rownames(summaryTable), split = ":"))
  colnames(groupsTable) <- factors
  summaryTable <- data.frame(groupsTable, summaryTable, check.names = FALSE)
  rownames(summaryTable) <- NULL

  summaryTable
}
