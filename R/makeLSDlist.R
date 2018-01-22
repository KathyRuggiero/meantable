#' Creates list of objects required by \code{meansLSDplot} to generate plots of means with LSD
#' (Least Significant Difference) bar(s).
#'
#' @param meansData must contain a column for each treatment factor, the contents of which are their
#' respective levels, and a column containing the predicted mean for each treatment (or factorial
#' treatment combination in the case of multi-factor experiments).
#' @param y name of column (as character string) in \code{meansData} containing predicted means.
#' @param x name of column (as character string) in \code{meansData} containing factor to be
#' plotted on x-axis when generating \code{meansLSDplot}.
#' @param LSD either a single numeric value (usually used when the LSD is the same for all pairwise
#' comparisons of means) or a dataframe. See \code{Details} below.
#' @param eqLevelFactors character vector of names of those factors which have equal levels in every
#' pairwise treatment comparison in the \code{LSD} dataframe. Only required if \code{LSD} is a
#' dataframe.
#' @param lsdVar name of column (as character string) containing least significant differences when
#' dataframe is provided to \code{LSD} argument. Only required if \code{LSD} is a dataframe.
#' @param edgeWidth a value from 0 to 0.5 used to control width of the edges of the LSD bar,
#' i.e. the horizontal lines at the top and bottom of the bar.
#' LSD line. A value of zero (default) means no edges are plotted.
#' @return A list with the following objects:
#' \item{meansData}{the same dataframe provided to the current function's \code{meansData}
#' argument, but with the factor provided to the \code{x} argument converted to type numeric.}
#' \item{xLimits}{a numeric vector of length two providing limits of the x-axis. See
#' \code{\link[ggplot2]{scale_x_continuous}}.}
#' \item{xBreaks}{a numeric vector of tick mark and label positions on the x-axis. See
#' \code{\link[ggplot2]{scale_x_continuous}}.}
#' \item{xLabels}{a character vector giving labels (must be same length as \code{xBreaks}). See
#' \code{\link[ggplot2]{scale_x_continuous}}.}
#' \item{barData}{a dataframe with twice the number of rows as there are factorial treatment
#' combinations for the factors named in \code{eqLevelFactors} \code{length()}}
#' \item{barData}{a dataframe containing a column for each factor in \code{eqLevelFactors},
#' if supplied, plus two other columns named \code{lsdRange} and \code{x} which
#' contain the (x, y) co-ordinates of the LSD bar.}
#' \item{edgeData}{a dataframe containing a column for each factor in \code{eqLevelFactors},
#' if supplied, plus three other columns named \code{lsdBottom}, \code{lsdTop} and \code{x} which
#' contain the (x, y) co-ordinates of the bottom and top edges of the LSD bar. The width of these
#' edges may be controlled by providing a value to the function's \code{edgeWidth} argument.}
#' @author Katya Ruggiero
#' @details
#' Either a single numeric value or dataframe may be provided to the \code{LSD} argument. A single
#' numeric value may be provided when the \emph{same} LSD value applies to all pairwise
#' comparisons of means, e.g., as would be the case for a completely randomised design or a
#' complete block design. When row and/or column facetting variables are desired in the means-LSD
#' bar plot, providing a single LSD value results in \code{barData} and \code{edgeData} dataframes
#' that yield the same vertical positioning of the LSD bar across all panels in the plot. More
#' specifically, all LSD bars start at the minimum value across all predicted means. It is possible
#' to generate \code{barData} and \code{edgeData} dataframes which yield LSD bars that start at the
#' minimum value across predicted means \emph{within} facetting variable. This requires that the
#' \code{LSD} argument is provided with a dataframe. An example of how to do this is presented
#' in the Examples section below.
#'
#' More generally, providing dataframes to the \code{LSD} argument is particularly useful for
#' multi-factor experiments in which the magnitude of the LSD differs substantially between the
#' levels of one or more factors. The \code{LSD} dataframe allows for LSD bars of different lengths
#' to plotted panel defined by row and/or column facetting variables. Examples are presented below.
#'
#' @export
#' @importFrom reshape melt
#' @examples
#' library(nlme)
#' library(predictmeans)
#' Oats$nitro <- factor(Oats$nitro)
#' fm <- lme(yield ~ nitro*Variety, random=~1|Block/Variety, data=Oats)
#' # library(lme4)
#' # fm <- lmer(yield ~ nitro*Variety+(1|Block/Variety), data=Oats)
#' pm <- predictmeans(fm, "nitro:Variety", pairwise=TRUE, plot=FALSE)
#' # library(reshape)
#' predMeans.pm <- means2df(pm)
#' avgLSD <- pm$LSD["Aveg.LSD"]
#' makeLSDlist(predMeans.pm, y="Mean", x="nitro", LSD=avgLSD)
#'
#' # fruitfly data: factorial with added control
#' data(fruitfly)
#' # -- set up factors
#' newfruitfly <- transform(fruitfly, Type = factor(Type, levels=c(9,0,1),
#'                                    labels=c("Control", "Newly pregnant", "Virgin")))
#' newfruitfly$Partners <- factor(newfruitfly$Partners)
#' newfruitfly$Control <- factor(ifelse(newfruitfly$Type=="Control", "Yes", "No"))
#' # -- fit model
#' fruitfly.lm <- lm(Longevity ~ Control/(Partners*Type), data=newfruitfly)
#' # -- perform post-hoc tests
#' fruitfly.pm <- predictmeans(fruitfly.lm, modelterm = "Control:Partners:Type",
#'                             pairwise = TRUE, plot = FALSE)
#' # -- get table of predicted means
#' fruitfly.m2df <- means2df(fruitfly.pm)
#' # -- create post-hoc tests summary table
#' fruitfly.tab <- makeSummaryTable(fruitfly.pm)
#' # -- replace single comparison name with individual columns
#' ffCompNames.df <- comparisonNames2df(fruitfly.tab$Comparison, split.at=":",
#'                                      varNames = c("Control", "Partners", "Type"))
#' newfruitfly.tab <- data.frame(ffCompNames.df, fruitfly.tab[,-1])
#' makeLSDlist(fruitfly.m2df, y="Mean", x="Type", eqLevelFactors = "Partners",
#'             LSD=8.29, edgeWidth = 0.05)

makeLSDlist <- function(meansData, y, x, LSD, eqLevelFactors, lsdVar, edgeWidth=0)
{

  ##################################
  # if variable to be plotted on x-axis is a factor, convert it to numeric.
  # this is needed to set up x-axis to allow for inclusion of LSD bar
  #browser()
  if(is.factor(meansData[[x]])){

    xLabels <- levels(meansData[[x]])
    xValues <- 1:length(xLabels)
    meansData[[x]] <- as.numeric(meansData[[x]])

  }
  else xLabels <- xValues <- sort(unique(meansData[[x]]))
  xLabels <- c("LSD", xLabels)

  # set up:
  # 1. start and end values of x-axis (xLimits), i.e. position of left-
  #    and right-most values of x-axis in each panel
  # 2. position of the LSD bar at the mid-point between xLimits[1]
  #    (i.e. the start of plot at LHS) and first plotting character
  #    min(as.numeric(meansData[[x]]))
  xRange <- range(meansData[[x]])
  pad <- 0.3*diff(xRange)
  xLimits <- c(xRange[1]-pad, xRange[2]+pad/2)  # controls panel width
  xLSD <- mean(c(xLimits[1], xRange[1]))        # controls LSD x-axis position
  xBreaks <- c(xLSD, xValues)                   # controls x-axis tick-mark positions
  ##################################

  if(missing(LSD)) stop("You must provide value(s) for LSD argument!", call. = FALSE)
  else if(length(LSD)==1){  # do this only if a single value is provided to LSD arg

    # create an edge data frame with:
    startLSDbar <- min(meansData[[y]])  # a column that has yBottom = rep(startLSDbar,2),
    endLSDbar <- startLSDbar + LSD      # a column that has yTop = rep(endLSDbar,2),
    # and an x = c(LHS, RHS) value
    lsdBar.df <- data.frame(matrix(c(startLSDbar, xLSD, endLSDbar,
                                     xLSD), nrow = 2, byrow = TRUE))
    names(lsdBar.df) <- c("lsdRange", "x")
    if(edgeWidth>0.5){

      edgeWidth <- 0.5
      cat("Maximum value of edgeWidth exceeded. Reset to 0.5.\n")
    }
    #browser()
    # see comments above about edge dataframe
    # the following lsdEdge.df replaces the commented out one below it
    lsdEdge.df <- data.frame(lsdBottom = rep(startLSDbar,2),
                             lsdTop = rep(endLSDbar,2),
                             x = c(xLSD-edgeWidth, xLSD+edgeWidth))
    #lsdEdge.df <- data.frame(rep(c(startLSDbar, endLSDbar), each = 2),
    #                         rep(c(xLSD-edgeWidth, xLSD+edgeWidth), times = 2))
    # notice i have moved the following further up, but it no longer gives names to lsdEdge.df
    #names(lsdBar.df) <- names(lsdEdge.df) <- c(y, x)

  }
  else if(is.data.frame(LSD)){

    # check group arg has been provided with factor name
    #if(missing(group)) stop("You must provide factor name for group argument!", call. = FALSE)
    if(missing(lsdVar)) stop("Value of 'lsdVar' is missing!", call. = FALSE)
    if(missing(eqLevelFactors)) stop("Value(s) of 'eqLevelFactors' missing!", call. = FALSE)
    minMean.df <- findSmallestMean(meansData, y, eqLevelFactors=eqLevelFactors)
    if(!setequal(intersect(names(LSD), eqLevelFactors), eqLevelFactors))
      stop("Names in eqLevelFactors do not match colum names in LSD!")
    if(!setequal(intersect(names(meansData), eqLevelFactors), eqLevelFactors))
      stop("Names in eqLevelFactors do not match colum names in meansData!")

    # construct dataframe for plotting LSD bar
    lsdBar.df <- merge(minMean.df, LSD[, c(eqLevelFactors, lsdVar)])
    lsdBar.df$endLSDbar <- apply(lsdBar.df[,setdiff(names(lsdBar.df), eqLevelFactors)], 1, sum)
    lsdEdge.df <- lsdBar.df[, names(lsdBar.df) != lsdVar]
    names(lsdEdge.df)[-(1:length(eqLevelFactors))] <- c("lsdBottom", "lsdTop")
    nRow <- nrow(lsdEdge.df)
    lsdEdge.df <- lsdEdge.df[rep(1:nRow, each=2),]
    lsdEdge.df$x <- rep(c(xLSD - edgeWidth, xLSD + edgeWidth), times = nRow)
    molten.df <- melt(lsdBar.df[, names(lsdBar.df)!=lsdVar], id.vars=eqLevelFactors)
    molten.df <- molten.df[,names(molten.df)!="variable"]
    names(molten.df)[ncol(molten.df)] <- "lsdRange"
    molten.df$x <- xLSD
    tmp <-  paste("order(", paste(eqLevelFactors, collapse = ","), ")")
    lsdBar.df <- molten.df[with(molten.df, eval(parse(text=tmp))),]

  }

  return(list(meansData = as.data.frame(meansData), xBreaks=xBreaks,
              xLabels=xLabels, xLimits=xLimits,
              barData = lsdBar.df, edgeData = lsdEdge.df))

}
