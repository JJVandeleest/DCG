GetNCluster = function(Ens){

	d = rowSums(Ens)
	n = nrow(Ens)
	Tmp = diag(d^(-1/2))
	NormalizeEns = Tmp %*% Ens %*% Tmp
	Eigenvalues = eigen(NormalizeEns)$values
	COLOR = rep("black",n)
	COLOR[which(Eigenvalues>0)] = "red"
	plot(Eigenvalues, type = "b", pch = 20, col = COLOR,
	cex = 0.5, ylab = "Normalized eigenvalues", main = "Eigen-plot")
}
