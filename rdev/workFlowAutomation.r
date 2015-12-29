# vigenette
# tests

library(DCG)
head(myData)

Sim <- as.simMat(myData)  # as.simMat checked. # documented
?as.simMat
# Transform your rawsim matrix into a matrix whose values range from 0 to 1 by dividing this by the maximum value of the matrix.


temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random') # documented


Ens_list <- getEnsList(Sim, temperatures, MaxIt = 5, m = 5)


plotEnsList(Ens_list, name = "eigenvalue_plots")


plotTrees(Ens_list, "myfirtforest")

plotTheTree(Ens_list, 2)
