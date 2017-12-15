#' Constructs a table of correlations for each unique combination of factor levels
#'
#' This function calculates pairwise correlations between two or more variables by groups, where
#' groups may be defined as either the levels of a single treatment factor or factorial treatment
#' combinations.
#'
#' @param data is a dataframe containing both measurement and categorical variables.
#' @param measure.vars is a character vector containing the names of the measurement variables in
#' \code{data} for which pairwise correlations will be calculated.
#' @param factors is a character vector containing the names of the categorical variables in
#' \code{data} from which groups are created.
#' @param useObs an optional character string giving a method for computing correlation
#' in the presence of missing values. This must be (an abbreviation of) one of the strings
#' \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or
#' \code{"pairwise.complete.obs"} (default).
#' @param useMethod a character string indicating which correlation coefficient (or covariance)
#' is to be computed. One of \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"},
#' can be abbreviated.
#' @return A dataframe containing \eqn{r} rows and \eqn{c} columns, where \eqn{r} equals the product
#' of the number of levels in each group variable and \eqn{c} is the sum of the number of variables
#' in \code{factors} and \code{choose(length(measure.vars), 2)}, i.e. the number of pairwise
#' correlations that can be computed from the set of measurement variables.
#' @export
#' @examples
#' data(fruitfly)
#' groupCor(fruitfly, factors=c("Type"), measure.vars=c("Longevity", "Thorax"))
#' groupCor(fruitfly, factors=c("Partners", "Type"), measure.vars=c("Longevity", "Thorax", "Sleep"))


groupCor <- function(data, measure.vars, factors, useObs = "pairwise.complete.obs",
                     useMethod = c("pearson", "kendall", "spearman"))
{

  object <- data[,factors]
  if(length(factors)==1) groupLevels <- list(object)
  else                      groupLevels <- as.list(object)
  byGroup.dfList <- split(data, groupLevels)

  # Check and remove non-existent groups as might arise in unbalanced designs
  trueGroups <- do.call('c',lapply(byGroup.dfList, function(x) nrow(x)==0))
  byGroup.dfList[trueGroups] <- NULL

  resultsList <- lapply(byGroup.dfList, function(x) cor(x[,measure.vars],
                                                        use=useObs, method = useMethod))
  corPair <- makeComparisonNames(resultsList[[1]])
  cor.mat <- do.call('rbind', lapply(resultsList, function(x)x[upper.tri(t(x))]))
  dimnames(cor.mat) <- list(NULL, corPair)
  groupVar.mat <- t(do.call('cbind', strsplit(names(resultsList), "[.]")))
  colnames(groupVar.mat) <- factors
  data.frame(groupVar.mat, cor.mat)

}
