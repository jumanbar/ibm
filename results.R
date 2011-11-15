path <- file.path('/home/juan/projects/maestria/saves')
arch <- dir(path, pattern='corrida')
arch <- file.path(path, arch)
pops <- vector('list', length(arch))
M    <- numeric(length(arch))
for (i in 1:length(arch)) {
  load(arch[i])
  pops[[i]] <- run$pop
  M[i]      <- run$parms$M
}

save(pops, M, file='pops-n-M.RData')
