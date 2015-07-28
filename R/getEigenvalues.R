GetEigenvalues = function(Ens){
  d = rowSums(Ens)
  n = nrow(Ens)
  Tmp = diag(d^(-1/2))
  NormalizeEns = Tmp %*% Ens %*% Tmp
  Eigenvalues = eigen(NormalizeEns)$values
  return(Eigenvalues)
}
