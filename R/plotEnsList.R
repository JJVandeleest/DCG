#' plot a list of ensemble matrices
#' \code{getEigenvalueList} generate eigenvalue plots corresponding to each ensemble matrix in ens_list
#' @param EnsList a list of ensemble matrices. It is the output of the function \code{getEnsList}.
#' @return a pdf file in the working directory containing all eigenvalue plots
#' #' @examples
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
#' plotEnsList(Ens_list, name = "eigenvalue_plots")
#' @seealso \code{getEnsList}
#' @export

plotEnsList <- function(EnsList, name = "eigenvaluePlots") {
  eigenvalue_list <- getEigenvalueList(Ens_list)
  plotMultiEigenvalues(eigenvalue_list, name)
}
