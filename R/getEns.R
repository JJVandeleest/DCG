
#' generate temperatures
#' \code{getEnsList} get ensemble matrices from given similarity matrix at all temperatures
#'
#' @param simMat a similarity matrix
#' @param temperatures temperatures selected
#' @param MaxIt parameter used in EstClust function [Read EstClust function to further understand MaxIt].
#' @param m parameter used in EstClust function [Read EstClust function to further understand MaxIt].
#' @return a numeric vector of length n representing temperatures sampled.
#'
#' @examples
#' Sim <- as.simMat(myData)  # as.simMat checked.
#' temperatures <- temperature_sample(start = 0.01, end = 20, n = 20, method = 'random')
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
  ens_list <- lapply(temperatures, function(x) getEns(SimMat, x, MaxIt = MaxIt, m = m))
  return(ens_list)
}

#' generate ensemble matrix
#' \code{getEns} get ensemble matrix from given similarity matrix and temperature
#'
#' @param simMat a similarity matrix
#' @param temperature a numeric vector of length 1, indicating the temperature used to transform the similarity matrix to ensemble matrix
#' @param MaxIt parameter used in EstClust function [Read EstClust function to further understand MaxIt].
#' @param m parameter used in EstClust function [Read EstClust function to further understand MaxIt].
#'
#' @return a matrix.
#' @details This function involves two steps.
#' It first generate similarity matrices of different variances
#' by taking the raw similarity matrix to the power of each
#' temperature. Then it called the function \code{EstClust} to perform random walks in the network to identify clusters.


getEns <- function(simMat, temperature, MaxIt = 1000, m = 5) {
  data_t <- simMat^temperature
  Ens <- EstClust(data_t, MaxIt, m)
}

