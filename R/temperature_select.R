temperature_select <- function(start = 0.01, end = 20, n = 20, method = "random")
{
  if (n > 30 | n < 1){
    stop("n should be integers between 1 and 30.")
  }
  if (method == "random") {
    tem_seq <- seq(start, end, (end - start)/1000 + start)
    temperatures <- sample(tem_seq, n)
  } else if (method == "equalLength") {
    temperatures <- seq(start, end, (end - start)/n + start)
  } else {
    stop("method should be either 'random' or 'equalLength'.")
  }
  return(temperatures)
}
