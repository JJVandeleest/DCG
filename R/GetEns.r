
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
for(j in 1:nrow(data)){
  subject1 = which(subjects == data$Initiator[j])
  subject2 = which(subjects == data$Recipient[j])
  rawsim[subject1, subject2] = rawsim[subject1, subject2] + 1
}

#### [end Kevin's code for creating a matrix from edgelist##


## Start with a data set X in R^d of N nodes, given in the
## form of an N by d matrix

############################################################################
## For simplicity, suppose a distance matrix D is given.
## The similarity matrix W is calculated at each temperature T.
## The diagonal of W are all 0.
############################################################################


GetSim <- function(D, T) {
	W <- exp(-D/T)
	diag(W) <- 0
	return(W)
}


Grooming^1
Grooming^2
....
Grooming^20

#### Which method to use for transforming rawsim into different temperatures
#### of the similarity matrix

maxraw<- max(rawsim)
Sim<- rawsim/maxraw


#############################################################################
## We create another function MakeSeries which carries out a regulated random
## walk on the nodes of the data set based on the similarity matrix W and a
## parameter M (whose default is 5), which is the maximum number of times
## any node can be visited during the random walk.
## After the random walk, the function returns a vector of recurrence
## times called Recur, which in its rth position gives the number of time
## steps between the (r-1)st node removal and the rth node removal (the
## 0th node removal is just the beginning of the random walk).
## The function also returns a vector I which is a record of the nodes
## visited during the random walk in sequential order.
#############################################################################

MakeSeries <- function(W, M=5) {

  N <- nrow(W)  ## N is the number of data nodes

      ## Compute the degree of each node as the sum of its row in W, and
      ## store the results in the vector Degree.
      ## Create the diagonal matrix D^{-1} whose diagonal elements are the
      ## reciprocals of the degrees of the nodes, and store it as Dinv.
      ## Create the transition matrix Q = Dinv * W.  All entries in Q are
      ## between 0 and 1, with Q_{ij} being the probability of moving
      ## from node i to node j during the random walk.

  Degree <- rowSums(W)
  Dinv <- diag(1/Degree)
  Q <- Dinv %*% W

      ## Vstd is a vector whose vth element is the number of times node v
      ## has been visited during the random walk.  Initially all elements
      ## equal zero.
      ## Vec is a vector which shows the nodes that are available for
      ## visitation at any point in the walk.  Initially nodes 1:N are
      ## available.

  Vstd <- rep(0,N)
  Vec <- 1:N

      ## We select the starting node from among those nodes which have the
      ## greatest degree, as follows:
      ## First, sort the degrees in descending order and store the sorted
      ## vector as Deg.
      ## Next find the largest i such that the sum of the first i elements
      ## of Deg is less than half of the sum of all the degrees.
      ## Then choose only those nodes whose degrees are no less than the
      ## ith element of Deg, and store their indices in the vector Top.
      ## Next, create the corresponding vector Probs as the ratio of the degree
      ## of each node in Top to the sum of all the degrees of the nodes in Top.
      ## Then, from among those nodes in Top randomly select one as the
      ## starting node, with the probability of choosing the jth node of Top
      ## given by Probs_j, and store this as the first element of I.
      ## Finally, the element of Vstd corresponding to the starting node is
      ## updated to equal 1, since that node has now been visited once.

  Deg <- sort(Degree, decreasing=TRUE)
  Sum <- 0
  i <- 1
  while(Sum < 0.5*sum(Degree)) {
    Sum <- sum(Deg[1:i])
    i <- i + 1
  }
  Top <- which(Degree >= Deg[i])
  Probs <- Degree[Top]/sum(Degree[Top])
  I <- sample(x=Vec[Top], size=1)
  Vstd[I[1]] <- 1

      ## Initialize L (the length of I), Recur (the vector of recurrence times),
      ## and Count (the number of steps since the last node removal).

  L <- 1
  Recur <- vector("numeric")
  Count <- 1

      ## Begin regulated random walk.  Continue walk until every node has been
      ## visited at least once, except one.

  while(length(which(Vstd == 0)) > 1) {

      ## Select those nodes which have not yet been eliminated from the walk.  Store
      ## their indices in Wh.  These are the nodes eligible for visitation.

    Wh <- which(Vec > 0)

      ## If the row of the transition matrix Q corresponding to the current node in
      ## the walk has at least one nonzero entry corresponding to the nodes eligible
      ## for visitation, select that row of Q, and multiply it entry-wise by the
      ## factor exp(Vstd[Wh]/M), so that the probability of moving to a node which
      ## has been previously visited is enhanced based on the number of previous
      ## visits.  Then normalize the resulting vector Row by dividing its elements
      ## by their sum, creating a probability vector Prob.  Then randomly select an
      ## eligible node as the next node, with the probability of choosing the jth node
      ## given by Prob_j, and store its index as the next element of I.
      ## Otherwise, select the next node uniformly from those eligible, and store its
      ## index as the next element of I.

    if(sum(Q[I[L],Wh], na.rm=TRUE) > 0 ) {
      Row <- Q[I[L],Wh]
      Prob <- Row / sum(Row)
      I <- c(I, sample(x=Vec[Wh], size=1, prob=Prob))
    } else {
      I <- c(I, sample(x=Vec[Wh], size=1))
    }

      ## Update L to the new length of vector I.  Update the number of visits made to
      ## the node selected as the next node, increasing the number by one.
      ## If that node has now been visited M times, its index is replaced with 0,
      ## thus removing it from the random walk.  Then the vector Recur is updated by
      ## concatenating it with the number of steps since the previous node removal,
      ## which is stored in Count.  Count is reset to 0.
      ## No matter what, Count is increased by one.

    L <- length(I)
    Vstd[I[L]] <- Vstd[I[L]] + 1
    if(Vstd[I[L]] == M) {
	  Vec[I[L]] <- 0
      Recur <- c(Recur, Count)
      Count <- 0
    }
    Count <- Count + 1

  }  ## end of while loop

      ## Concatenate I with the index of the last node which has not been visited.
      ## Return the vectors Recur and I.

  I <- c(I, which(Vstd == 0))
  return(list(I=I, Recur=Recur))

} ## end of function MakeSeries

#######################################################################################
## We create a third function EstClust which takes a similarity matrix Sim and runs
## MaxIt consecutive regulated random walks (1000 by default) with the node removal
## parameter m (5 by default).  EstClust reads the profile of recurrence times stored
## in Recur for each walk and selects the top 5% of recurrence times as Spikes which
## signify probably transitions into previously unexplored clusters of nodes.  The
## record of the random walk stored in I is then segmented based on the occurrence of
## the spikes, with the locations of the segment boundaries stored in the vector Clust.
## Each node is assigned to a segment if the walk visited that node at least m/2
## times while the walk was within that segment.  The assignments are stored in the
## vector Assign.  The symmetric matrix EmpSim stores in element (i,j) the proportion
## of times nodes i and j were assigned to the same segment over MaxIt random walks.
## The function returns EmpSim.
#######################################################################################

EstClust <- function(Sim, MaxIt=1000, m=5) {
  N <- nrow(Sim)  ## N is the number of nodes in the data set

      ## Initialize the N x N matrix Ensemble with 0s for every element.  Initialize
      ## Iter at 1.

  Ensemble <- matrix(0, nrow=N, ncol = N)

  Iter <- 1

      ## Continue until MaxIt iterations are completed.

  while(Iter <= MaxIt) {

      ## Conduct a regulated random walk based on Sim with M=m.  Store the resulting
      ## recurrence time profile as Rec.

    Series <- MakeSeries(Sim, M=m)
    Rec <- Series$Recur

      ## Select the locations of the outliers of recurrence times and store them as the
      ## vector Spikes.  Make sure the first spike is placed at the beginning, and make
      ## sure it is not redundant.  Then pass through the elements of Spikes and remove
      ## any spike which occurs immediately after the preceding spike, so that there is
      ## always some space between spikes in the recurrence times.
      ## Concatenate Spikes with the location of the end of the recurrence time
      ## profile, i.e., put a final spike at the end of the profile.


    Spikes <- unique(c(1, which(Rec >= quantile(Rec, 0.95))))
    i <- 2
    while(i <= length(Spikes)) {
      if((Spikes[i] - Spikes[i-1]) == 1)  Spikes <- Spikes[-i]
      i <- i + 1
    }
    Spikes <- c(Spikes, (length(Series$Recur)+1))

      ## Create the vector Clust the same length as Spikes, and set its first element = 1.
      ## For each successive element of Clust, compute the number of steps in the random
      ## walk that occurred prior to the corresponding spike.  The number of steps is the
      ## sum of the recurrence times from one spike to the next, plus the number of steps
      ## prior to that (stored in Clust[i-1]).  Clust now stores the number of steps in the
      ## random walk between each successive spike in the recurrence times.

    Clust <- rep(0, length(Spikes))
    Clust[1] <- 1
    for(i in 2:length(Spikes))
    	Clust[i] <- sum(Series$Recur[Spikes[(i-1)]:(Spikes[i]-1)]) + Clust[i-1]

      ## Create the vector Assign of length N.  For each data node i, check each segment j of
      ## the random walk, as delineated by Clust, and compute the number of times node i
      ## was visited during the random walk within segment j.  Store that number in Temp.
      ## If this number is at least m/2, assign node i to segment j

    SpikesRmv <- cumsum(Rec)[Spikes]
    SpikesRmv[length(Spikes)] <- length(Series$I)

    Assign <- rep(0, N)

    for(k in 1:N){
    	FirstVst <- which(Series$I == k)[1]
    	Assign[k] <- which(SpikesRmv >= FirstVst)[1]
    }

      ## For each pair of nodes i and j, if both nodes are assigned to the same segment,
      ## increment elements (i,j) and (j,i) of the Ensemble matrix by 1.  Then increment
      ## Iter by 1.

    for(i in 2:N) {
      for(j in 1:(i-1)) {
        if(Assign[i] == Assign[j]) {
          Ensemble[i,j] <- Ensemble[i,j] + 1
          Ensemble[j,i] <- Ensemble[j,i] + 1
        }
      }
    }

    Iter <- Iter + 1

  } ## end of while loop

      ## Divide all elements of Ensemble by MaxIt, so that element (i,j) is the proportion
      ## of times nodes i and j were assigned to the same segment of the random walk over
      ## a total of MaxIt random walks.  Return this matrix.

  Ensemble <- Ensemble/MaxIt
  return(Ensemble)

} ## end of function EstClust

#####
source("GetEigenPlot.R")

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

