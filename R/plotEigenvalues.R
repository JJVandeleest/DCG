#' plot eigenvalues
#' \code{plotMultiEigenvalues} plot eigen values into a ".pdf" file.
#'
#' @param eigenvalue_list a list in which elements are numeric vectors representing eigenvalues.
#' @param name a character vector of length 1. name of the exported pdf
#' @return a pdf file in the working directory containing all eigenvalue plots
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
#' plotMultiEigenvalues(Ens_list, mfrow = c(10, 2), mar = c(1, 1, 1, 1))
#'
#' @export

plotMultiEigenvalues <- function(Ens_list, mfrow, mar = c(2, 2, 2, 2), line = -1.5, cex = 0.5, ...) {
   eigenvalue_list <- DCG:::getEigenvalueList(Ens_list)
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
