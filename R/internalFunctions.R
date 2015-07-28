# helper function
vector2List <- function(vector){
  newList <- list()
  for (i in seq_along(vector)){
    newList[[i]] <- vector[i]
  }
  return(newList)
}
