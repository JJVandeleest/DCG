#' generate tree plots for each ensemble matrix
#' \code{plotCLUSTERS} plot all cluster trees
#'
#' @param EnsList a list in which elements are ensemble matrices.
#' @param mfrow A vector of the form \code{c(nr, nc)} passed to \code{\link{par}}.
#' @param mar plotting parameters with useful defaults (\code{\link{par}})
#' @param line plotting parameters with useful defaults (\code{\link{par}})
#' @param cex plotting parameters with useful defaults (\code{\link{par}})
#' @param ... further plotting parameters
#' @return a \code{pdf} file in the working directory containing all tree plots.
#' @details When deciding parameters for mfrow,
#' one should take into considerations size of the plotting device and number of cluster plots.
#' For example, there are 20 cluster plots, mfrow can be set to \code{c(4, 5)} or \code{c(2, 10)}
#' depending on the size and shape of the plotting area.
#' @export
#' @examples
#' Sim <- as.simMat(myData)
#' temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random')
#' \dontrun{
#' # for illustration only. skip CRAN check because it ran forever.
#' Ens_list <- getEnsList(Sim, temperatures, MaxIt = 1000, m = 5)
#' }
#' \dontshow{
#' # for CRAN check only
#' Ens_list <- getEnsList(Sim, temperatures, MaxIt = 5, m = 5)
#' }
#' plotCLUSTERS(EnsList = Ens_list, mfrow = c(2, 10), mar = c(1, 1, 1, 1))
#'





plotCLUSTERS <- function(EnsList, mfrow,
                         mar = c(1, 1, 1, 1),
                         line = -1.5,
                         cex = 0.5, ...){
  # set arrangement
  op <- par(mfrow = mfrow,
            mar = mar)

  for (i in 1:length(EnsList)){
    plotTree(EnsList[[i]])
    mtext(paste0("Ens", i), 4)
  }

  par(op)

}



#' generate tree plots for selected ensemble matrix
#' \code{plotTrees} plot one cluster tree
#' @param EnsList a list in which elements are ensemble matrices.
#' @param index an integer. index of which ensemble matrix you want to plot.
#' @param ... plotting parameters passed to \code{\link{par}}.
#' @return a tree plot
#' @export

plotTheCluster <- function(EnsList, index, ...){
  plotTree(EnsList[[index]])
}




plotTree <- function(ens, ...){
  tree <- hclust(as.dist(1-ens))
  plot(tree)
}
