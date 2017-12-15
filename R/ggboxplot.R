#' Wrapper for ggplot2's boxplot geometry
#'
#' This function generates side-by-side boxplots for either one factor or a pair of factors,
#' defaulting to my preferred theme parameter values, i.e. white background and larger (than the
#' usual) default axis and tick mark label font sizes.
#'
#' @param data a dataframe.
#' @param y name of response variable.
#' @param x name of factor (explanatory variable) to plotted on the x-axis
#' @param facets character vector of factor names whose levels define a matrix of row and/or column
#' panels in the plot. This vector must be of length 2. Thus, if only one facet factor is needed,
#' fill the vector with the character string \code{"."} (see Examples below). The order of the
#' factor names, one of which may be the character string \code{"."}, determine whether the levels
#' are displayed in rows or columns, with first element (factor name) corresponding to row panels
#' and the second to column panels.
#' @param yLabel y-axis label (character string)
#' @param xLabel x-axis label (character string)
#' @param axisLabelSize font size of x- and y-axis labels. Currently, these cannot be of different
#' sizes.
#' @param tickMarkLabelSize font size of x- and y-axis tick mark labels. Currently, these cannot be
#' of different sizes.
#' @param xTickLabelAngle angle (0, 45 or 90; default 0) of x-axis tick labels
#' @param xStripTextSize x strip text size in pts
#' @param yStripTextSize y strip text size in pts
#' @return Generates one- (default) and two-way boxplots. A one-way boxplot is generated when
#' \code{facet = NULL} (default). A two-way boxplot is generated when \code{facet} is supplied
#' with a character string of a factor name.
#' @return Plot aesthetics, themes, etc. may be saved in an R object so that other
#' layers may be added.
#' @author Katya Ruggiero
#' @details This function is a wrapper to generate side-by-side boxplots for either a single factor
#' or a pair of factors.
#' @seealso \code{\link[ggplot2]{ggplot}} \code{\link[ggplot2]{aes_string}} \code{\link[ggplot2]{facet_wrap}}
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#'
#' # With default axis and tick mark label font sizes
#' data(crdData)
#' ggBoxplot(crdData, y = "logAUC", x="Surgery")
#'
#' # With larger axis and tick mark label font sizes
#' ggBoxplot(crdData, y = "logAUC", x="Surgery", axisLabelSize=40,
#'    tickMarkLabelSize=30)
#'
#' # Two facets for a factorial experiment, default layout of facets
#' data(splitBlockData)
#' newsbData <- transform(splitBlockData, Organ = factor(Organ,
#'   levels = c("innerLV", "outerLV"),
#'   labels = c("Inner Left Ventricle", "Outer Left Ventricle")))
#' ggBoxplot(newsbData, y = "logAUC", x="Disease", facets=c("Organ", "."),
#'           xStripTextSize = 16)
#'
#'
#' # longevity of male fruitflies
#' data(fruitfly)
#' newfruitfly <- toFactor(fruitfly, variables = c("Partners", "Type"))
#' ggBoxplot(newfruitfly, y = "Longevity", x = "Partners", facets = c("Type", "."))
#'
#' # re-label Type of partner, and change x- and y-axis labels
#' newfruitfly <- transform(newfruitfly, Type = factor(Type, levels=c(9,0,1),
#'    labels=c("Control", "Newly pregnant", "Virgin")))
#' ggBoxplot(newfruitfly, y = "Longevity", x = "Partners", facets = c("Type", "."),
#'           xLabel = "Number of partners", yLabel = "Lifespan (days)")
#'
#'
#' # bugs
#' data(bugs)
#' newbugs <- toFactor(bugs, variables = "Time")
#' newbugs$logCells <- log10(newbugs$Cells + 1)
#' ggBoxplot(newbugs, y = "logCells", x="Time", facets=c("State", "Bacteria"))
#' ggBoxplot(newbugs, y = "logCells", x="State", facets=c("Time", "Bacteria"),
#'           tickMarkLabelSize=10, yStripTextSize = 10)


ggBoxplot <- function(data, y, x, facets, xLabel, yLabel, axisLabelSize=14,
                      tickMarkLabelSize=12, xTickLabelAngle = 0,
                      xStripTextSize = 12, yStripTextSize = 12)
{
  p <- ggplot(data, mapping = aes_string(y = y, x = x)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title = element_text(size = axisLabelSize),
          axis.text = element_text(size = tickMarkLabelSize),
          strip.text.x = element_text(size = xStripTextSize),
          strip.text.y = element_text(size = yStripTextSize),
          legend.title = element_text(size=16))

  if(xTickLabelAngle != 0){

    p <- if(xTickLabelAngle == 45)
      p + theme(axis.text.x = element_text(angle=xTickLabelAngle, hjust=1, vjust=1))
    else if(xTickLabelAngle == 90)
      p + theme(axis.text.x = element_text(angle=xTickLabelAngle, hjust=1, vjust=0.5))
  }

  if(!missing(xLabel)) p <- p + xlab(xLabel)
  if(!missing(yLabel)) p <- p + ylab(yLabel)
  if (!missing(facets)){

    if(length(facets) > 2) stop("Currently only 2 facets allowed!", call.=FALSE)
    p <- p + facet_grid(reformulate(facets[1], facets[2]), drop = TRUE, scales="free_x")

  }
  p

}

