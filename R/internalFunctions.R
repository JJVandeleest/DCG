# helper function
# I don't think this function is used.
# Try deleting and run R CMD CHECK.
vector2List <- function(vector){
  newList <- list()
  for (i in seq_along(vector)){
    newList[[i]] <- vector[i]
  }
  return(newList)
}
