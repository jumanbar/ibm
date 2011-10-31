

minYield <- function(M=0.1, yinit=10, yincr=5, Pmin=0.01, tfinal=200,
                     dirOut='../ibm-output/exp-minYield') {
  count <- 0
  yield <- yinit - yincr
  popul <- matrix(0, nreps, 100)
  while (P > Pmin) {
    count <- count + 1
    yield <- yield + yincr 
    cat('tries:', count, '\n')
    for (i in 1:nreps) {
      cat('rep', i, 'of', nreps, '\n')
      run <- ibm(M=M, tfinal=tfinal, yield=yield)
      ext <- ext + run$extinction
      run.name <- paste('run', i, 'yield', count, 'minYield.RData', sep='-')
      save(run, file=file.path(dirOut, run.name))
      meanPop <- mean(run$pop[1:(round(tfinal / 2))])
      popul[i, count] <- meanPop
    }
    P <- ext / nreps
    if (count == ncol(popul))
      popul <- cbind(popul, matrix(0, nreps, 100))
  }
  popul <- popul[, 1:count]
  out <- list(M=M, Pmin=Pmin, tfinal=tfinal, pop=popul, yield=yield)
  return(out)
}


## Variante: tasas ajustadas
# Se ajusta respecto al 20% del tiempo que lleva a que se agoten reservas a un
# individuo con todas las reservas en ayunos.
# Todo lo que es diario se multiplica por el número de días que abarca este
# tiempo
# El yield se multiplica por el número de días tb (?)
# Se mantiene la distancia de percepción.
# 
