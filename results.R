
path <- file.path('/home/juan/Dropbox/saves')
arch <- dir(path, pattern='corrida')
arch <- file.path(path, arch)
na   <- length(arch)
pops <- vector('list', )
M    <- numeric(na)
k    <- numeric(na)
for (i in 1:na) {
  load(arch[i])
  p <- run$pop
  l <- length(p)
  pops[[i]] <- p
  k[i] <- mean(p[round(l / 2):l])
  M[i] <- run$parms$M
}

names(pops) <- paste('M', round(M, 2), sep='')

save(pops, M, k, file=file.path(path, 'pops-n-M.RData'))
