# to do: add temperature method: normal, exponential, etc...
#' generate temperatures
#' \code{temperatureSample} find the rank order for the win-loss relationship
#'
#' @param start a numeric vector of length 1, indicating the lowest temperature
#' @param end a numeric vector of length 1, indicating the highest temperature
#' @param n an integer between 10 to 30, indicating the number of temperatures (more explanations on what temperatures are).
#' @param method a character vector indicating the method used in selecting temperatures.
#' It should take either 'random' or 'fixedInterval', case-sensitive.
#' @return a numeric vector of length n representing temperatures sampled.
#'
#' @examples
#' Sim <- as.simMat(data)  # as.simMat checked.
#' temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random')
#' @export



temperatureSample <- function(start = 0.01, end = 20, n = 20, method = "random")
{
  if (n > 30 | n < 1){
    stop("n should be integers between 1 and 30.")
  }
  if (method == "random") {
    tem_seq <- seq(start, end, (end - start)/1000 + start)
    temperatures <- sample(tem_seq, n)
  } else if (method == "fixedInterval") {
    temperatures <- seq(start, end, (end - start)/n + start)
  } else {
    stop("method should be either 'random' or 'fixedInterval'.")
  }
  temperatures_sorted <- sort(temperatures)
  return(temperatures_sorted)
}

