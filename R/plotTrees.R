#' generate tree plots for each ensemble matrix
#' \code{plotTrees} plot eigen values into a ".pdf" file.
#'
#' @param ensList a list in which elements are ensemble matrices.
#' @param name a character vector of length 1. name of the exported .pdf file.
#' @return a pdf file in the working directory containing all tree plots.
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
#' plotCLUSTERS(Ens_list, name = "manyTrees")
#'
#'



plotCLUSTERS <- function(EnsList, mfrow,
                         mar = c(2, 2, 2, 2),
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
#' \code{plotTrees} plot eigen values into a ".pdf" file.
#'
#' @param ensList a list in which elements are ensemble matrices.
#' @param index an integer. index of which ensemble matrix you want to plot
#' @return a tree plot
#' @export

plotTheCluster <- function(EnsList, index, ...){
  plotTree(EnsList[[index]])
}




plotTree <- function(ens, ...){
  tree <- hclust(as.dist(1-ens))
  plot(tree)
}
