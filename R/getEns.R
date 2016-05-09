
#' generating a list of ensemble matrices based on the similarity matrix and temperatures
#' \code{getEnsList} get ensemble matrices from given similarity matrix at all temperatures
#'
#' @param simMat a similarity matrix
#' @param temperatures temperatures selected
#' @param MaxIt number of iterations for regulated random walks
#' @param m maxiumnum number of time a node can be visited during random walks
#'
#' @details This step is crucial in finding community structure based on the similarity matrix of the social network.
#' For each \code{temperatures}, the similarity matrix was taken to the power of \code{temperature} as saved as a new similarity matrix.
#' This allows the random walk to explore the similarity matrix at various variations.
#' Random walks are then performed in similarity matrices of various temperatures.
#' In order to prevent random walks being stucked in a locale, the parameter \code{m} was set (to \code{5} by default) to remove a node after \code{m} times of visits of the node.
#' An ensemble matrix is generated at each temperature in which values represent likelihood of two nodes being in the same community.
#'
#'
#' @return a numeric vector of length n representing temperatures sampled.
#'
#' @examples
#' Sim <- as.simMat(myData)  # as.simMat checked.
#' temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random')
#' \dontrun{
#' # Note: It takes a while to run the getEnsList example.
#' Ens_list <- getEnsList(Sim, temperatures, MaxIt = 1000, m = 5)
#' }
#' \dontshow{
#' # for CRAN check only
#' Ens_list <- getEnsList(Sim, temperatures, MaxIt = 5, m = 5)
#' }
#' @export




getEnsList <- function(simMat, temperatures, MaxIt = 1000, m = 5) {
  ens_list <- lapply(temperatures, function(x) getEns(simMat, x, MaxIt = MaxIt, m = m))
  return(ens_list)
}

#' generate ensemble matrix
#' \code{getEns} get ensemble matrix from given similarity matrix and temperature
#'
#' @param simMat a similarity matrix
#' @param temperature a numeric vector of length 1, indicating the temperature used to transform the similarity matrix to ensemble matrix
#' @param MaxIt number of iterations for regulated random walks
#' @param m maxiumnum number of time a node can be visited during random walks
#' @return a matrix.
#' @details This function involves two steps.
#' It first generate similarity matrices of different variances
#' by taking the raw similarity matrix to the power of each
#' temperature. Then it called the function \code{EstClust} to perform random walks in the network to identify clusters.


getEns <- function(simMat, temperature, MaxIt = 1000, m = 5) {
  data_t <- simMat^temperature
  Ens <- EstClust(data_t, MaxIt, m)
}

