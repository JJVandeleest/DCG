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
#' eigenvalue_list <- getEigenvalueList(Ens_list)
#' plotMultiEigenvalues(eigenvalue_list, "eigenvaluePlots")



plotMultiEigenvalues <- function(eigenvalue_list, name = "eigenvaluePlots"){
  if(!(is.character(name))){
    name <- as.character(name)
    warning("file name was converted to a character vector.")
  }
  file = paste0("./", name, ".pdf")
  pdf(file = file, onefile = TRUE)
  for (i in 1:length(eigenvalue_list)){
    plotEigenValue(eigenvalue_list[[i]])
    mtext(paste0("Ens", i), 4)
  }
  dev.off()
  message(paste0("eigen value plots can be found at ", sQuote(file)))
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
