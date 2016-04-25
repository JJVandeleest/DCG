library(DCG)
# check MakeSeries.R
M = 5
Sim <- as.simMat(myData)
DCG:::MakeSeries(Sim, M = 5)


N <- nrow(Sim)  ## N is the number of nodes in the data set

## Initialize the N x N matrix Ensemble with 0s for every element.  Initialize
## Iter at 1.

Ensemble <- matrix(0, nrow=N, ncol = N)

Iter <- 1

## Continue until MaxIt iterations are completed.

while(Iter <= MaxIt) {

  ## Conduct a regulated random walk based on Sim with M=m.  Store the resulting
  ## recurrence time profile as Rec.

  Series <- DCG:::MakeSeries(Sim, M=5)
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

  Series$Recur[Spikes[(2-1)]:(Spikes[2]-1)]

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
rownames(Ensemble) <- rownames(Sim)
colnames(Ensemble) <- colnames(Sim)
return(Ensemble)









W = Sim
Degree <- rowSums(Sim)
Dinv <- diag(1/Degree)

Q <- Dinv %*% W

Sim[90,]

N <- nrow(W)

Vstd <- rep(0,N)
Vec <- 1:N

Deg <- sort(Degree, decreasing=TRUE)
Sum <- 0
i <- 1


while(Sum < 0.5*sum(Degree)) {
  Sum <- sum(Deg[1:i])
  i <- i + 1
}

i

Top <- which(Degree >= Deg[i])

Probs <- Degree[Top]/sum(Degree[Top])

I <- sample(x=Vec[Top], size=1)
Vstd[I[1]] <- 1


L <- 1
Recur <- vector("numeric")
Count <- 1


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
    Prob <- Row / sum(Row)  # the probability of being in the same community with someone.
    I <- c(I, sample(x=Vec[Wh], size=1, prob=Prob))
  } else {
    I <- c(I, sample(x=Vec[Wh], size=1)) # if degree == 0, the select one randomly.
  }

  L <- length(I)
  Vstd[I[L]] <- Vstd[I[L]] + 1
  if(Vstd[I[L]] == M) {
    Vec[I[L]] <- 0
    Recur <- c(Recur, Count)
    Count <- 0
  }
  Count <- Count + 1

}


Series <- MakeSeries(Sim, M=5)
str(Series)
table(Series$I)

Spikes <- unique(c(1, which(Rec >= quantile(Rec, 0.95))))


Rec <- Series$Recur








