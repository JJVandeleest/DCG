# fix plotting
# http://stats.stackexchange.com/questions/14524/draw-multiple-plots-on-one-graph-in-r
# layout(matrix(1:5, ncol = 1), widths = 1,
#       heights = c(1,5,5,5,7), respect = FALSE)
# par(mar=c(0, 4, 0, 0))


library(DCG)

head(myData)

Sim <- as.simMat(myData)  # as.simMat checked. # documented. # tested.


# Transform your rawsim matrix into a matrix whose values range from 0 to 1 by dividing this by the maximum value of the matrix.


temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random') # documented


Sim <- as.simMat(myData)  # as.simMat checked.
temperatures <- temperatureSample(start = 0.01, end = 20, n = 20, method = 'random')

Ens_list <- getEnsList(Sim, temperatures, MaxIt = 1000, m = 5)


# test:
#     - getEnsList is  alist
#    -
#

# not necessary: multi_engenvalues <- getEigenvalueList(Ens_list)

plotMultiEigenvalues(Ens_list,
                     mfrow = c(10, 2), mar = c(1,1,1,1),
                     line = -1.5, cex = 0.8)



pdf(file = "./rdev/trythisplot.pdf", width = 20, height = 60)

plotCLUSTERS(EnsList = Ens_list, c(10, 2), mar = c(1,1,1,1),
             line = -1.5, cex = 0.8)

dev.off()


plotTheCluster(Ens_list, 2)

