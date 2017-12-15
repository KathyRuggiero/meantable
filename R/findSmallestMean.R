#' Finds lowest value among predicted means of a set of treatment groups.
#'
#' The function was primarily created to be used (called) by \link{makeLSDlist} to create
#' one of the two dataframes needed by \link{meansLSDplot} to generate plots of means with LSD
#' bars.
#'
#' @param data dataframe containing columns which are factors and one column containing
#' the predicted means for each factorial treatment combination (defined by the values in each
#' row across the set of factors in the dataframe).
#' @param y character string of column name in \code{data} containing means.
#' @param eqLevelFactors character vector of columns names in \code{data}
#' corresponding to those factors whose levels are constant (i.e. have the same value) in the
#' pair of factorial treatment combinations being compared.
#' @return A dataframe comprising a column for each factor in \code{eqLevelFactors} and a column
#' for the minimum mean for each factorial treatment combination given by the levels of the factors
#' in \code{eqLevelFactors}.
#' @author Katya Ruggiero
#' @details
#' @export
#' @importFrom nlme lme
#' @importFrom lme4 lmer
#' @importFrom predictmeans predictmeans
#' @importFrom reshape melt
#' @importFrom reshape cast
#' @examples
#' library(nlme)
#' library(predictmeans)
#' Oats$nitro <- factor(Oats$nitro)
#' fm <- lme(yield ~ nitro*Variety, random=~1|Block/Variety, data=Oats)
#' # library(lme4)
#' # fm <- lmer(yield ~ nitro*Variety+(1|Block/Variety), data=Oats)
#' pm <- predictmeans(fm, "nitro:Variety", pairwise=TRUE, plot=FALSE)
#'
#' # generate dataframe of means for each factorial treatment combination
#' predMeans.pm <- means2df(pm)
#' findSmallestMean(predMeans.pm, y = "Mean", eqLevelFactors = "Variety")

findSmallestMean <- function(data, y, eqLevelFactors){

  molten.df <- melt(data, id.vars=eqLevelFactors, measure.vars=y)
  castingFormula <- as.formula(paste(paste(eqLevelFactors, collapse = " + "), "~ variable"))
  minMean.df <- cast(molten.df, castingFormula, fun.aggregate = min)
  names(minMean.df)[ncol(minMean.df)] <- "minMean"

  return(minMean.df)

}
