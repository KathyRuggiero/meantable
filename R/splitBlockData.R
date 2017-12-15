#' Simulated data set for a split-block design
#'
#' A dataset containing the log-transformed protein abundances
#' in 6 rats randomly assigned to one of 2 treatment groups:
#' Healthy or Diabetic. Two organs were harvested from each animal,
#' namely the inner and outer left ventricle (LV) wall.
#'
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{Disease}{disease status, either Healthy or Diabetic}
#'   \item{Organ}{harvested tissue, namely the inner and outer LV}
#'   \item{Animal}{Rat ID}
#'   \item{Sample}{an arbitrary label (either 1 or 2) to different the two samples taken from
#'   each rat.}
#'   \item{logAUC}{log-transformed area under the curve, a proxy for log-abundance of protein.}
#' }
"splitBlockData"
