simAll <- function(sim, temperatures) {
  tempList <- vector2List(temperatures)
  Sim_list<- lapply(temperatures, function(x) Sim^x)
  return(Sim_list)
}
