#' plot eigenvalues
#' \code{plotMultiEigenvalues} plot eigen values into a ".pdf" file.
#'
#' @param Ens_list a list in which elements are numeric vectors representing eigenvalues.
#' @param mfrow A vector of the form \code{c(nr, nc)} passed to \code{\link{par}}.
#' @param mar plotting parameters with useful defaults (\code{\link{par}})
#' @param line plotting parameters with useful defaults (\code{\link{par}})
#' @param cex plotting parameters with useful defaults (\code{\link{par}})
#' @param ... further plotting parameters
#' @details \code{mfrow} determines the arrangement of multiple plots. It takes the form of
#' \code{c(nr, nc)} with the first parameter being the number of rows and
#' the second parameter being the number of columns. When deciding parameters for mfrow,
#' one should take into considerations size of the plotting device and number of plots.
#' For example, there are 20 plots, mfrow can be set to \code{c(4, 5)} or \code{c(2, 10)}
#' depending on the size and shape of the plotting area.
#' @return a \code{pdf} file in the working directory containing all eigenvalue plots
#'
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
#'
#' plotMultiEigenvalues(Ens_list = Ens_list, mfrow = c(10, 2), mar = c(1, 1, 1, 1))
#'
#' @export

plotMultiEigenvalues <- function(Ens_list,
                                 mfrow,
                                 mar = c(2, 2, 2, 2),
                                 line = -1.5,
                                 cex = 0.5, ...) {
   eigenvalue_list <- getEigenvalueList(Ens_list)
   op <- par(mfrow = mfrow,
            mar = mar) # set arrangement

  for (i in 1:length(eigenvalue_list)){
    Eigenvalues = eigenvalue_list[[i]]
    n = length(Eigenvalues)
    COLOR = rep("black",n)
    COLOR[which(Eigenvalues > 0)] = "red"
    plot(Eigenvalues,
         type = "b",
         pch = 20,
         col = COLOR,
         cex = cex,
         ylab = "Normalized eigenvalues",
         main = paste0("Eigen-plot", " ", "Ens", i))
  }
  par(op)
}











plotEigenValue <- function(Eigenvalues, ...) {
  n = length(Eigenvalues)
  COLOR = rep("black",n)
  COLOR[which(Eigenvalues > 0)] = "red"
  plot(Eigenvalues,
       type = "b",
       pch = 20,
       col = COLOR,
       cex = 0.5,
       ylab = "Normalized eigenvalues",
       main = "Eigen-plot")
}
