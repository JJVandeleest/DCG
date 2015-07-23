############################################################################
## For simplicity, suppose a distance matrix D is given.
## The similarity matrix W is calculated at each temperature T.
## The diagonal of W are all 0.
############################################################################


GetSim <- function(D, T) {
  W <- exp(-D/T)
  diag(W) <- 0
  return(W)
}
