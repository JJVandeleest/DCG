#####
source("./R/GetNCluster.R")
source("./R/MakeSeries.R")
source("./R/GetSim.R")
source("./R/EstClust.R")
source("./R/as.simMat.R")

data = practiceData[-875, ]
Sim <- as.simMat(data)  # as.simMat checked.
temperatures <- temperature_select(0.01, 20, 20, 'random')
plot(temperatures)

sim_List <- simAll(Sim, temperatures)

est_list <- lapply(sim_List, function(x) EstClust(x, MaxIt = 1000, m = 5))

eigenAll <- function(est_list) {
  eigen_list <- lapply(est_list, GetEigenvalues)
  eigen_all <- do.call(cbind, eigen_list)
  return(eigen_all)
}

eigen_all <- eigenAll(est_list)


ncol(eigen_all)

color_matrix <- eigen_all
color_matrix[] <- NA
color_matrix
plot.eigenValues <- function(eigen_all, alpha, )

?rainbow
color_samples <- rainbow(ncol(eigen_all), alpha = 0.3)
color_matrix <- matrix(NA, nrow = nrow(eigen_all), ncol = ncol(eigen_all))

for (i in 1:length(color_samples)){
  color_matrix[,i] <- rep(color_samples[i], nrow(eigen_all))
}

color_matrix[which(eigen_all < 0)] <- "black"

color_matrix

plot(eigen_all[,1],
     type = "n",
     xlab = "",
     ylab = "Normalized eigenvalues",
     main = "Eigen-plot")
for (i in 1:ncol(eigen_all)) {
  lines(eigen_all[,i], type = "p", pch = 20, cex = 0.5, col = color_matrix[,i])
}

legend("topright", # places a legend at the appropriate place

  c(paste0("temperature", 1:ncol(eigen_all))), # puts text in the legend


  lty = rep(1, ncol(eigen_all)), # gives the legend appropriate symbols (lines)


  col = color_samples,
  cex = 0.5) # gives the legend lines the correct color

?legend




Treee1 <- hclust(as.dist(1-est1))
Treee2 <- plot(hclust(as.dist(1-est2)))

GetNCluster(est1)
summary(Treee1)
str(Treee1)
plot(Treee1)

Treee1[[1]]
Treee1[[2]]
print(Treee1)
?hclust
Ens.list <- lapply(Sim.temp.list, function(x) EstClust(x, MaxIt = 1000, m = 5))



## Function 'doStuff' added by Matt Pagel to combine Ens and Tree code, as well as add node names to tree plot
doStuff <- function(dta, iters, mm, pl=TRUE, NClust=FALSE) {
  Ens <-EstClust(dta, MaxIt = iters, m = mm)
  rownames(Ens) <- subjects
  colnames(Ens) <- subjects
  Treee <- hclust(as.dist(1-Ens))
  if (pl) plot(Treee)
  if (NClust) GetNCluster(Ens)
  return(Ens)
}

Ens2 <- doStuff(Sim^2, 1000, 5)

Ens1 <- EstClust(Sim^0.5, MaxIt = 1000, m = 5)
Ens1

Ens2 <- EstClust(Sim^0.9, MaxIt = 1000, m = 5)
Ens2

?as.dist

Tree1 <- hclust(as.dist(1-Ens1))
plot(Tree1)

GetNCluster(Ens1)
?lapply
jpeg("mypic.jpg", width = 1000, height = 1000)
par(mfrow = c(1, 2))
lapply(list(Ens1, Ens2), GetNCluster)
dev.off()
?par
#Temp <- 0.5
#Sim <- GetSim(distance, Temp)
Ens2 <- EstClust(Sim^0.8, MaxIt = 1000, m = 4)
Ens2

d = rowSums(Ens2)
n = nrow(Ens2)
Tmp = diag(d^(-1/2))
NormalizeEns = Tmp %*% Ens2 %*% Tmp
Eigenvalues = eigen(NormalizeEns)$values

plot(Eigenvalues)

?nScree
Tree2 <- hclust(as.dist(1-Ens2))
hclust
plot(Tree2)
GetNCluster(Ens2)

#Temp <- 2
#Sim <- GetSim(distance, Temp)
Ens3 <- EstClust(Sim^2, MaxIt = 1000, m = 5)
Tree3 <- hclust(as.dist(1-Ens3))
plot(Tree3)
GetNCluster(Ens3)

Ens4 <- EstClust(Sim^5, MaxIt = 1000, m = 5)
Tree4 <- hclust(as.dist(1-Ens4))
plot(Tree4)
GetNCluster(Ens4)

Ens5 <- EstClust(Sim^7, MaxIt = 1000, m = 5)
Tree5 <- hclust(as.dist(1-Ens5))
plot(Tree5)
GetNCluster(Ens5)

write.csv(Ens3, file = "NC6Cmatfrag_postEnsembleDCG_Temp10.csv")
write.csv(subjects, file="ScanSubgp_PPAsubjects.csv")


save(Ens1, Ens2, Ens3, file = "Ensembles.RData")

### Can we add indirect grooming paths to the direct paths, using percolation code?

plot.conf.mat = function(conf.mat, ordering = NA){
  if(length(ordering) == 1){
    ordering = 1:ncol(conf.mat)
  }
  conf.mat.ord = conf.mat[ordering, ordering]
  ramp = colorRamp(c("white","blue", "orange", "red"))
  colors = rgb(ramp(seq(0, 1, length = 1000)), max = 255)
  levelplot(t(conf.mat.ord)[,ncol(conf.mat.ord):1], col.regions = colors,
            xlab = "Losing Team", ylab = "Winning Team",
            #list the proper tick mark values here
            #note: they are usually not perfect multiples
            #of 20 like listed here.
            #e.g. the first cage is something like
            #13, 33, 53, 73 if I am remembering properly.
            #Let me know if you know of a more automatic
            #way to do this.
            scales=list(y=list(labels=c('120','100','80','60','40','20',''))))
}

plot.conf.mat(Ens1 + 0.5 * diag(102), ordering = subjects)

