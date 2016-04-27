############################################################################
## For simplicity, suppose a distance matrix D is given.
## The similarity matrix W is calculated at each temperature T.
## The diagonal of W are all 0.
############################################################################
#' \code{GetSim} get similarity matrix from a distance matrix
#' @param D A distance matrix
#' @param T Temperature. \code{\link{temperatureSample}}
#' @details the similarity matrix is calculated at each temperature T.
#' @export

GetSim <- function(D, T) {
  W <- exp(-D/T)
  diag(W) <- 0
  return(W)
}
