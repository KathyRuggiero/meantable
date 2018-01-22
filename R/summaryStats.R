#' @importFrom stats sd median
summaryStats <- function(x){

  missing <- sum(is.na(x))
  n <- length(x)-missing
  sem <- sd(x, na.rm=TRUE)/sqrt(n)
  data.frame(Minimum=min(x, na.rm=TRUE),
             Median=median(x, na.rm=TRUE),
             Maximum=max(x, na.rm=TRUE),
             Mean=mean(x, na.rm=TRUE),
             SE=sem, N=n, Missing=missing)
}
