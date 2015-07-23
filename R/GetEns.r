
#### [from Kevin's percolation code]
#### head = TRUE indicates that there is a header row in the .csv file.
setwd("C:/Users/Bri/Documents/BrianneDocuments/UCDavis/CNPRC/PostDocSocial Network Study/R code special/DCG R code")

file = "ScanSubgroup_ADedgelistNames_DCG.csv"

data = read.csv(file, head = TRUE)
data = practiceData
#### If you want to remove all lines with a "0" code...
data = data[data$Initiator != 0 & data$Recipient != 0,]

#### Change "Initiator" and "Recipient" to the names of the column headers
#### This will give you the sorted list of unique subject ID numbers
subjects = sort(unique(c(as.matrix(data$Initiator), as.matrix(data$Recipient))))	## Done by Matt Pagel to handle string variables as nodes

N = length(subjects)

#### Converting the raw data into a conflict matrix called "conf".
rawsim = matrix(0, N, N) #no data, sized N*N
for (j in 1:nrow(data)){
  subject1 = which(subjects == data$Initiator[j])
  subject2 = which(subjects == data$Recipient[j])
  rawsim[subject1, subject2] = rawsim[subject1, subject2] + 1
}

#### [end Kevin's code for creating a matrix from edgelist##


## Start with a data set X in R^d of N nodes, given in the
## form of an N by d matrix




Grooming^1
Grooming^2
....
Grooming^20

#### Which method to use for transforming rawsim into different temperatures
#### of the similarity matrix

maxraw<- max(rawsim)
Sim<- rawsim/maxraw





#####
source("./R/GetEigenPlot.R")
source("./R/MakeSeries.R")
source("./R/GetSim.R")
source("./R/EstClust.R")

load("Distance-DCG.RData")

Temp <- 0.2
Sim <- GetSim(costDist, Temp)

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
rownames(Ens1) <- names
colnames(Ens1) <- names
Tree1 <- hclust(as.dist(1-Ens1))
plot(Tree1)
GetNCluster(Ens1)

#Temp <- 0.5
#Sim <- GetSim(distance, Temp)
Ens2 <- EstClust(Sim^0.8, MaxIt = 1000, m = 4)
Tree2 <- hclust(as.dist(1-Ens2))
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

