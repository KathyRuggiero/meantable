#' Produce publication-ready post-hoc test summary table
#'
#' This function cleans post-hoc test summary tables created by \code{\link{makeSummaryTable}} and processed
#' by \code{\link{getFixedLevelComparisons}} and \code{\link{comparisonNames2df}} of redundant columns, and enables
#' rows to be sorted according to user's preferences.
#'
#' @param data dataframe created by \code{\link{makeSummaryTable}} whose \code{Comparison} column has been
#' disaggregated using \code{comparisonNames2df}.
#' @param eqLevelFactors character vector of names of those factors which have equal levels in every
#' pairwise treatment comparison in \code{data} dataframe.
#' @param contrFactor name of factor with different values (levels) in the two factorial treatment
#' combinations being contrasted.
#' @param contrSymbol character string to be used when forming the \code{Comparison} character
#' strings from values in the two columns of \code{data} corresponding to \code{contrFactor}
#' @param rowSort.by character vector containing column names containing the values by which the rows of the
#' resulting dataframe are to be sorted by.
#' @return A dataframe with redundant columns (i.e. those corresponding to the variable names
#' supplied to \code{eqLevelFactors}) removed, and in which the values in each row of the two columns
#' corresponding to \code{contrFactor} are concatenated to form a \code{Comparison} column.
#' @author Katya Ruggiero
#'
#' @seealso \code{\link{makeSummaryTable}} \code{\link{getFixedLevelComparisons}}
#' \code{\link{comparisonNames2df}}
#' @export
#' @examples
#' library(predictmeans)
#' library(nlme)
#' Oats$nitro <- factor(Oats$nitro)
#' fm <- lme(yield ~ nitro*Variety, random=~1|Block/Variety, data=Oats)
#'
#' # library(lme4)
#' # fm <- lmer(yield ~ nitro*Variety+(1|Block/Variety), data=Oats)
#'
#' # carry out post-hoc tests for pairwise comparisons of means between
#' #pairs of nitr x Variety treatment combinations
#' pm <- predictmeans(fm, "nitro:Variety", pairwise=TRUE, plot=FALSE)
#'
#' # create summary table of post-hoc test results
#' oats.tab <- makeSummaryTable(pm)
#'
#' # keep only those rows of the table in which the pairs of means in each
#' # comparison differ only the levels of one of the two factors
#' keepRows <- getFixedLevelComparisons(oats.tab$Comparison, sepChar = ":")
#' subOats.tab <- oats.tab[keepRows, ]
#' # 36 rows dropped from original table, i.e.
#' nrow(oats.tab); nrow(subOats.tab)
#'
#' # disaggregate the Comparison column into its constituent factors
#' subOats.tab2 <- comparisonNames2df(subOats.tab[,1], split.at = ":",
#'                                    varNames = c("nitro", "Variety") )
#' subOats.tab2 <- data.frame(subOats.tab2, subOats.tab[,-1])
#'
#' # let's just keep the rows in which the levels of Variety1 != Variety2
#' nitroContr.tab <- subOats.tab2[whichComparison(subOats.tab2[,1:4], contrFactor = "nitro"), ]
#' dim(nitroContr.tab)
#'
#' # tidy up table
#' publishSummaryTable(nitroContr.tab, eqLevelFactors = "Variety", contrFactor="nitro")
#'
#' # separate treatment names in contrast with "vs"
#' publishSummaryTable(nitroContr.tab, eqLevelFactors = "Variety", contrFactor="nitro",
#'                     contrSymbol = " vs ")
#'
#' # sort by Variety
#' publishSummaryTable(nitroContr.tab, eqLevelFactors = "Variety", contrFactor="nitro",
#'                     contrSymbol = " vs ", rowSort.by = "Variety")

publishSummaryTable <- function(data, eqLevelFactors, contrFactor, contrSymbol = "-", rowSort.by){

  dataColNames <- gsub("[1-2]$", "", names(data))
  eqLevelFactors.colNumbers <- match(eqLevelFactors, dataColNames)
  nFactors <- length(eqLevelFactors) + 1
  dropColumns <- -c( 1 : (2*(nFactors)))
  contrColNumbers <- grep(contrFactor, dataColNames)

  tableOut <- data.frame(data[,eqLevelFactors.colNumbers],
                         Comparison = apply(data[ , contrColNumbers] , 1 , paste ,
                                            collapse = contrSymbol),
                         data[,dropColumns])
  names(tableOut)[1:(nFactors-1)] <- dataColNames[eqLevelFactors.colNumbers]

  if(!missing(rowSort.by)){

    rowSort.by <- if(length(rowSort.by) == 1) parse(text=paste("order(", rowSort.by,  ")"))
                  else parse(text=paste("order(", paste(rowSort.by, collapse = " ,"),  ")"))
    tableOut <- with(tableOut, tableOut[eval(parse(text = rowSort.by)),])

  }

  tableOut

}
