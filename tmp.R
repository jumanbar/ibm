directorio <- file.path('..', 'muestras-2011-10')
unlink(directorio, TRUE)
dir.create(directorio)
dir.create(file.path(directorio, 'corridas'))

isla <- mklands(lmax_=1, n_=1)

nM <- 8
nY <- 8
vectorM <- seq(-1, 1, len=nM)
vectorY <- seq(5000 ^ (1 / 2), 30000 ^ (1 / 2), len=nY)
tf <- round(800 + (10 ^ vectorM) * 565.6566)
mintf <- round(tf / 8)
listaM  <- vector("list", nM)

for (i in 1:nM) {
  muestra <- data.frame(M=numeric(nY), yield=0, K=0)
  cat('No. de M =', i, '\n')
  for (j in 1:nY) {
    cat('\tNo. de yield =', j, '\n')
    count <- 0
    while (count < 20) {
      count <- count + 1
      y <- vectorY[j] ^ 2
      run <- ibm(lands=isla, yield=y, tfinal=tf[i],
                 M=10 ^ vectorM[i], verboso=FALSE)
      if (!run$extinction)
        break
    }
    last <- length(run$pop)
    if (run$extinction) {
      muestra[j, ] <- c(run$parms$M, run$parms$yield, 0)
    } else {
      muestra[j, ] <- c(run$parms$M, run$parms$yield, mean(run$pop[mintf[i]:last]))
    }
    nombrerun <- paste('run-M-', i, '-yield-', j, 'RData', sep='')
    save(run, file=file.path(directorio, 'corridas', nombrerun))
  }
  listaM[[i]] <- muestra[,]
  nombre <- paste('muestra-M-', i, '.RData', sep='')
  save(muestra, file=file.path(directorio, nombre))
}

