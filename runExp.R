## SCRIPT GENÉRICO PARA CORRER EXPERIMENTOS

ptm <- proc.time()[3]

sizes  <- unique(seq(logMinSize, logMaxSize, length.out=nSizes))
values <- unique(seq(eval(minValue), eval(maxValue), length.out=nValues))

outAll        <- vector('list', nValues)
names(outAll) <- paste('value', round(values, 2), sep='')

sizesValues <- as.data.frame(matrix(nrow=nSizes, ncol=nValues))
colnames(sizesValues) <- paste('value_', 1:nValues, sep='')
rownames(sizesValues) <- paste('size_', 1:nSizes, sep='')

condition <- lastValue + lastSize + lastRep
first     <- ifelse(condition, lastValue, 1)
condition <- FALSE

for(v in first:nValues) {
  cat(paste('\nvalue:', v, 'of', nValues, '\n'))
  out <- matrix(nrow=nSizes * nReps, ncol=5)
  colnames(out) <- c('logSize', 'size', 'popK', 'births', 'deaths')
  out <- as.data.frame(out)
  s <- 1
  i <- 1
  acum <- 1
  if(v == lastValue) {
    load(paste(dirOut, '/tabla_', v, '.RData', sep=''))
    out[1:nrow(tabla),] <- tabla
    s <- lastSize
    i <- nrow(tabla) + 1
  }
  while(s <= nSizes) {
    cat(paste('\tsize:', s, 'of', nSizes, '\n'))
    r   <- 1; if(v == lastValue && s == lastSize) r <- lastRep + 1
    div <- 1
    sv1 <- 0
    while(r <= nReps) {
      cat(paste('\t\trep:', r, 'of', nReps, '\n'))
      cat(paste('\t\tinicio:', Sys.time(), '\n'))
      formals(ibm)[variable] <- values[v]
      run <- ibm(M=10 ^ sizes[s], verboso=verboso, tfinal=tfinal)
      if(!run$extinction) {
        meanPop    <- mean(run$pop[(tfinal - ultimos):tfinal])
        meanBirths <- mean(run$births[(tfinal - ultimos):tfinal])
        meanDeaths <- mean(run$deaths[(tfinal - ultimos):tfinal])
        sv1 <- sv1 + meanPop
        out[i,] <- c(sizes[s], 10 ^ sizes[s], meanPop, meanBirths, meanDeaths)
        tabla <- out[1:i,]
        save(tabla, file=paste(dirOut, '/tabla_', v, '.RData', sep=''))
        save(run, file=paste(dirOut, '/value_', v, '_size_', s, '_rep_', r, '.RData', sep=''))
        save(v, s, r, count, file=paste(dirOut, '/last.RData', sep=''))
        i <- i + 1
        r <- r + 1
        acum <- 1
        div  <- div + 1 
      } else {
        acum <- acum + 1
        if(acum > maxTry) {
          out[i,] <- c(sizes[s], 10 ^ sizes[s], 0, 0, 0)
          tabla <- out[1:i,]
          save(tabla,
            file=file.path(dirOut,
              paste('tabla_', v, '.RData', sep='')))
          save(run,
            file=file.path(dirOut,
              paste('value_', v, '_size_', s, '_rep_', r, '.RData', sep='')))
          save(v, s, r, count, file=file.path(dirOut, 'last.RData'))
          i <- i + 1
          r <- r + 1
          acum <- 1
          cat('\t\t\tAll dead!\n')
        }
      }
    }
    s <- s + 1
    sizesValues[s, v] <- ifelse(is.nan(sv1 / div), 0, sv1 / div)
  }
  outAll[[v]] <- out
}

v <- s <- r <- 0
save(v, s, r, count, file=file.path(dirOut, 'last.RData'))

tablas <- outAll

save(tablas, file=paste(dirOut, '/tablas.RData', sep=''))
save(sizesValues, file=paste(dirOut, '/sizesValues.RData', sep=''))

png(filename=file.path(dirOut, 'logSize_vs_popK.png'),
    bg = "white",  res = 100)
plot(log10(popK) ~ logSize, data=tabla)
dev.off() 

tiempo <- proc.time()[3] - ptm

cat(paste('TIEMPO TOTAL:', round(tiempo, 2), 'seg.\n'))

cat(paste('\n#TIEMPO TOTAL:', round(tiempo / 3600, 2), 'hrs\n'),
  file=file.path(dirScr, 'log.nfo'),
  append=TRUE)

