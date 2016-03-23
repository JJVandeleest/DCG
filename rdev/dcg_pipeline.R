library(DCG)
library(stringr)
packageVersion("DCG")
sessionInfo()
# ==== set up temperatures ====
temperatures2 <- temperatureSample(start = 0.01, end = 10, n = 30, method = 'fixedInterval')
getwd()
#path = "/Users/JianJin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle/"
path = "C:/Users/jian jin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle/"
#outputPath = "/Users/JianJin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle Output/"
outputPath = "C:/Users/jian jin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle Output/"
filenames <- list.files(path)
dataNames <- stringr::str_sub(filenames, end = -5L)
length(dataNames)
dataNames <- dataNames[15:32]
for (i in seq_len(length(dataNames))) {
  dir.create(paste0(outputPath, dataNames[i]))
  data <- read.csv(paste0(path, dataNames[i], ".csv"))
  simMat2 <- as.simMat(data)
  Ens_list <- getEnsList(simMat = simMat2,
                         temperatures = temperatures2,
                         MaxIt = 1000,
                         m = 5)
  saveRDS(Ens_list, file = paste0(outputPath, dataNames[i], "/EnsembleMatrix.rds"))
  pdf(file = paste0(outputPath, dataNames[i], "/eigenPlot.pdf"),
      onefile = TRUE,
      width = 30,
      height = 60)
  # start a pdf graphic device
  plotMultiEigenvalues(Ens_list, mfrow = c(10, 2),
                       mar = c(2, 2, 2, 2),
                       line = -1.5, cex = 0.8)
  dev.off()
}






dcgpipeline <- function(name, outputPath = myoutputPath, path = mypath){
  dir.create(paste0(outputPath, name))
  data <- read.csv(paste0(path, name, ".csv"))
  simMat2 <- as.simMat(data)
  Ens_list <- getEnsList(simMat = simMat2,
                         temperatures = temperatures2,
                         MaxIt = 1000,
                         m = 5)
  saveRDS(Ens_list, file = paste0(outputPath, name, "/EnsembleMatrix.rds"))
  pdf(file = paste0(outputPath, name, "/eigenPlot.pdf"),
      onefile = TRUE,
      width = 30,
      height = 60)
  # start a pdf graphic device
  plotMultiEigenvalues(Ens_list, mfrow = c(10, 2),
                       mar = c(2, 2, 2, 2),
                       line = -1.5, cex = 0.8)
  dev.off()
}

dcgpipeline(names[1])

?dir.create

simdata <- read.csv(paste0(mypath, names[1], ".csv"))
simMat2 <- as.simMat(simdata)
Ens_list <- getEnsList(simMat = simMat2,
                       temperatures = temperatures2,
                       MaxIt = 5,
                       m = 5)


saveRDS(Ens_list, file = paste0(myoutputPath, names[1], "/EnsembleMatrix.rds"))

pdf(file = paste0(myoutputPath, names[1], "/eigenPlot.pdf"),
    onefile = TRUE,
    width = 30,
    height = 60)
# start a pdf graphic device
plotMultiEigenvalues(Ens_list, mfrow = c(10, 2),
                     mar = c(2, 2, 2, 2),
                     line = -1.5, cex = 0.8)
dev.off()

# ==== plot trees ====
library(DCG)

path_part1 = "/Users/JianJin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle Output/"
path_part2 = list.files("/Users/JianJin/Dropbox/Rfile/DCG/rdev/DCG_groom and huddle Output/")

getwd()
path = paste0(path_part1, path_part2, "/")
for (i in 1:length(path_part2)){
  file = paste0(path_part1, path_part2[i], "/EnsembleMatrix.rds")
  Ens_List = readRDS(file)
  DCG:::plotCLUSTERS(Ens_List, path_part2[i])
}

file = paste0(path_part1, path_part2[1], "/EnsembleMatrix.rds")
Ens_List = readRDS(file)
DCG:::plotCLUSTERS(Ens_List, path_part2[1])

