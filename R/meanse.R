#' Calculate mean and standard error
#'
#' This function calculates the mean and standard error of the data in a vector 'x'.
#'
#' @param x is a vector of observations
#' @return a vector of two elements, the mean and standard error of observations in x
#' @author Katya Ruggiero
#' @details
#' This function computes the mean and standard error of the observations in a vector.
#' NAs are removed before the calculations are performed.
#' @seealso \code{\link{mean}}, \code{\link{sd}}
#' @importFrom stats sd
#' @export

meanse <- function(x){

  m <- mean(x, na.rm=TRUE)
  s <- sd(x, na.rm=TRUE)
  n <- length(x)
  se <- s/sqrt(n)

  c(Mean=m, SE=s)

}

