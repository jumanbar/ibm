## v1.4

## FUNCIONES AUXILIARES PARA ibm

bholt <- function(N, Ro=2, K=yield) {
## Con Ro = 2 en 20 iteraciones llega al máximo (No=1), al menos
## en el rango de valores de yield entre 3000 y 50000
  ## K = (Ro - 1) * M
  M <- K / (Ro - 1)
  return(Ro * N / (1 + N / M))
}

###############################################
###############################################

## Para ver la dinámica:
bholtDyn <- function(No=1, Ro=2, K=1e4, tfinal=20, ...) {
  ## K = (Ro - 1) * M 
  M <- K / (Ro - 1)
  out <- vector('numeric', length=tfinal)
  out[1] <- No
  for(t in 2:tfinal) {
    out[t] <- Ro * out[t-1] / (1 + out[t-1] / M)
  }
  plot(out, ...)
  return(out)
}

###############################################
###############################################

## Para ver la dinámica:
bholtDyn2 <- function(No=1, Ro=2, M=1, tfinal=20, ...) {
  ## K = (Ro - 1) * M 
#~   M <- K / (Ro - 1)
  out <- vector('numeric', length=tfinal)
  out[1] <- No
  for(t in 2:tfinal) {
    out[t] <- Ro * out[t-1] / (1 + out[t-1] / M)
  }
  plot(out, ...)
  return(out)
}
###############################################
###############################################
distancias <- function(xyInd, xyParches) 
{
  if(!is.matrix(xyParches)) xyParches <- matrix(xyParches, ncol=2)
  out <- sqrt((xyParches[,1] - xyInd[1]) ^ 2 + (xyParches[,2] - xyInd[2]) ^ 2)
    return(out)
}

###############################################
###############################################

enerObt <- function(x, sdaMax=sdaMax, mei=mei[i]) x * (1 - x * sdaMax / mei)

###############################################
###############################################

feed <- function(foodAcum, mei, pasto) {
  return(ifelse(foodAcum + pasto <= mei, pasto, mei - foodAcum))
}

###############################################
###############################################

gompertz <- function(x, p_max, gompB, gompC) {
## ej: curve(gompertz(x, 1, -5.5, -1.5), from=-1, to=8, ylim=c(0,1)); ejes()
# En modo Auto:
#   gompB <- log(p_0)
#   gompC <- log(log(p1) / log(p0)) / psi[i]
  return(p_max * exp(gompB * exp(gompC * x)))
}

###############################################
###############################################

logistica <- function(x, logitA0, logitA1) {
# ~> curve(logit(x, -2.5, .06), from=-10, 10)
# En modo Auto:
#   logitA0 <- log(p_0 / (1 - p_1))
#   logitA1 <- log((p_1 * (1 - p_0)) / (p_0 * (1 - p_1))) / psi[i]
  Y <- (logitA0 + x * logitA1)
  return(p_max * exp(Y) / (1 + exp(Y)))
}

###############################################
###############################################

makePointsFun <- function(name) {
# name: character, nombre de la función
  fun <- NULL
  foo <- eval(parse(text=name))
  if(is.function(foo))
    return(foo)
  else {
    switch(name,
      logitAuto={
        logitA0 <- log(p_0 / (1 - p_1))
        fun <- function(x) {
          logitA1 <- log((p_1 * (1 - p_0)) / (p_0 * (1 - p_1))) / psi[i]
          Y <- (logitA0 + x * logitA1)
          return(p_max * exp(Y) / (1 + exp(Y)))
        }
      },
      logitManual={
        fun <- function(x) {
          Y <- (logitA0 + x * logitA1)
          return(p_max * exp(Y) / (1 + exp(Y)))
        }
      },
        fun <- logistica,
      gompAuto1={
        gompB <- log(p_0)
        fun <- function(x) {
          gompC <- log(log(p_1) / log(p_0)) / psi[i]
          return(p_max * exp(gompB * exp(gompC * x)))
        }
      },
      gompAuto2={
        gompB <- - 1
        fun <- function(x) {
          gompC <- log(- log(p_1)) / psi[i]
          return(p_max * exp(gompB * exp(gompC * x)))
        }
      },
      gompManual={
        fun <- function(x) {
          return(p_max * exp(gompB * exp(gompC * x)))
        }
      },
      potencia={
        fun <-function(x) {
          return((x - min(x) + 1) ^ ptExp)
        }
      })
  }
  if(!is.function(fun))
    stop('la variable "name" debe ser una función o uno de los
         siguientes strings: "logitAuto", "logitManual", "gompAuto",
         "gompManual", "potencia"')
  environment(fun) <- parent.frame()
  return(fun)
}

###############################################
###############################################

mklands <- function(dim_=2, dist_=1, lmax_=2, n_=3, rdist_=3, type='fractal') {
## ejemplo:
##   plot(mklands())
  require(splancs, quietly=TRUE)
  require(ellipse, quietly=TRUE)
  n_ <- round(n_)
  npatch <- (n_ ^ dim_) ^ lmax_

  areas  <- vector('list', lmax_)
  belong <- as.data.frame(matrix(1:npatch, nrow=npatch, ncol=lmax_))
  names(belong) <- names(areas) <- paste('l', 0:(lmax_ - 1), sep='')

  if(lmax_ <= 1) {
    areas <- vector('list', 1)
    if(npatch == 1) {
      coordsAll <- matrix(0, 1, 2)
      pos       <- 0
    }
  }

  if(type == 'fractal' && npatch > 1) {
    dists_ <- NULL
    seg <- dists_
    for(l_ in 1:lmax_) {
      for(i in 1:(n_ - 1)) {
        dists_ <- c(dists_, c(dist_ * rdist_ ^ (l_ - 1), seg))
      }
      seg <- dists_
    }
    pos <- c(0, cumsum(dists_))
  }

  if(type == 'regular' && npatch > 1) {
    n_ <- (n_ ^ dim_) ^ lmax_
    pos <- 0:n_ * dist_
  }

  if(type == 'randUnif' && npatch > 1) {
    x  <- sort(runif(npatch, 0, (n_ - 1) * dist_))
    y  <- runif(npatch, 0, (n_ - 1) * dist_)
    coordsAll <- cbind(x=x, y=y)
    pos <- NULL
  }

  if(type != 'randUnif' && npatch > 1) {
    npos      <- length(pos)
    coordsAll <- matrix(nrow=npatch, ncol=2)

    p <- 0:npos * npos + 1
    for(i in 1:npos) {
      coordsAll[p[i]:(p[i + 1] - 1),] <- cbind(pos[i], pos)
    }

    if(type == 'fractal' && lmax_ > 1) {

      for(l_ in 1:(lmax_ - 1)) {
        p   <- 1
        nl  <- n_ ^ l_
        b   <- (dist_ * rdist_ ^ l_) * .15
        ini <- 0:(npos / nl - 1) * nl + 1
        level <- vector('list', (n_ ^ dim_) ^ (lmax_ - l_))

        for(j in ini) {
          x <- c(pos[j], pos[j + nl - 1])
          for(i in ini) {
            y <- c(pos[i], pos[i + nl - 1])
            linea <- rbind(c(x[1] - b, y[1] - b),
                      c(x[1] - b, y[2] + b),
                      c(x[2] + b, y[2] + b),
                      c(x[2] + b, y[1] - b),
                      c(x[1] - b, y[1] - b))
            level[[p]] <- linea
            id <- inpip(coordsAll, linea, bound=TRUE)
            belong[id, l_ + 1] <- p
            p <- p + 1
          }
        }
        areas[[l_ + 1]] <- level
      }
    }
  }

  patchAreas <- vector('list', nrow(coordsAll))
  for(i in 1:nrow(coordsAll)) {
    patchAreas[[i]] <- ellipse(0, centre=c(coordsAll[i,1], coordsAll[i,2]), t=dist_ * .15, npoints=15)
  }
  areas[[1]] <- patchAreas
  
  coordsAll <- as.data.frame(coordsAll)
  names(coordsAll) <- c('x', 'y')

  parms <- list(dim_=dim_, dist_=dist_, lmax_=lmax_, n_=n_, rdist_=rdist_, type=type)  
  out <- list(areas=areas, belong=belong, coordsAll=coordsAll, parms=parms, pos=pos)
  class(out) <- 'lands'
  return(out)
}

###############################################
###############################################

mod <- function(x, y) { out <- x %% y; out[out == 0] <- y; return(out)}

###############################################
###############################################

msd <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(sd(total))
}

###############################################
###############################################

mvar <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(var(total))
}

###############################################
###############################################

plot.ibm <- function(x, kind='animation', outdir='default', nmax=500,
            type='l', col1=1, col2=8, uplim=1, areaFactor=1.3, t_=1,
            resFactor=2, noiseFactor=1/7, from, to, ...) {

  M         <- x$parms$M
  parches   <- x$lands
  pos       <- x$lands$pos
  coordsAll <- x$lands$coordsAll
  record    <- x$record
  tfinal    <- length(x$pop)
  if(tfinal < x$parms$tfinal) {
    tfinal <- tfinal - 1
  }
  dmin      <- ifelse(length(pos) > 1, min(diff(pos)), 1)
  maxname   <- record[[tfinal]]$name[nrow(record[[tfinal]])]
  pasto     <- x$pastoAll
  yield     <- x$parms$yield

  ini   <- ifelse(missing(from), 1, from)
  fin   <- ifelse(missing(to), tfinal, to)
  times <- ini:fin
  
  x.lims <- y.lims  <- range(pos)
  ancho <- diff(x.lims)
  y.lims[2] <- y.lims[1] + ancho * (1 + areaFactor) / 2
  y.lims[1] <- y.lims[1] + ancho * (1 - areaFactor) / 2
  x.lims[2] <- x.lims[1] + ancho * (1 + areaFactor) / 2
  x.lims[1] <- x.lims[1] + ancho * (1 - areaFactor) / 2


  seriePasto <- sapply(pasto, sum)
  plotGrass  <- max(x$pop) * seriePasto / (x$parms$yield * nrow(x$lands$coordsAll))

   if(outdir == 'default') out <- 'animation'
  else {
    out <- outdir
    system(paste('mkdir', out))
  }
  
  if(kind == 'animation' || kind == 1) {

    require(animation)
    height <- 1.3 * 480
    width  <- 2 * (1.3) * 480
    oopt <- ani.options(interval=0.4, nmax=100, outdir=out, ani.height=height, ani.width=width)

# ~    if(tfinal > 100) {
# ~      div <- round(tfinal / 100)
# ~      times <- (1:tfinal)[(1:tfinal %% div) == 0]
# ~    } else {

# ~    }

    ani.start()
      for(i in times) {
        plot(1,1,type='n')
      }
    ani.stop()
#~     ani.start()
    k <- 1
    for(i in times) {
      png(filename=paste('animation/images/', k, '.png', sep=''),
        width=width, height=height, bg = "white",  res = 100)
      par(mfcol=c(1,2), mar=c(4,5.5,1.5,1.5))        
      plot(parches, yield=yield, pasto=pasto[[i]], ylim=y.lims, xlim=x.lims, cex.lab=1.7, cex.axis=1.7)
      rec <- record[[i]]
      noise1 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      noise2 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      points(rec$x + noise1, rec$y + noise2,
          pch='x', cex=resFactor * (rec$m / M),
          col=rainbow(maxname)[rec$name])
      legend('topleft', legend=x$pop[i], cex=1.5, bty='n')
      
      plot(ini:i, plotGrass[ini:i], type='o', pch=19, lwd=1.25, col='#409951', xlim=c(ini, fin),
        ylim=c(0, max(x$pop) * uplim), ylab='Población (N)', xlab='Iteración', , cex.lab=1.7, cex.axis=1.7)
      points(ini:i, x$pop[ini:i], lwd=3, col=col1, type='o', pch=20)
      dev.off()
#~         dev2bitmap(paste('animation/images/', k, '.png', sep=''), width=width, height=height, units='px')
      k <- k + 1
    }
#~     ani.stop()
  }

  if(kind == 'pop' || kind == 2) {
    if(x$parms$grassMode != 'fixed') {
      plot(plotGrass[times], type='o', pch=19, lwd=1.25, col='#409951',
      ylim=c(0, max(x$pop[times]) * uplim), ylab='Población (N)', xlab='Iteración', ...)
      points(x$totalMigra[times], lwd=1.25, col='#B32323', type='o', pch=20)
    } else {
      plot(x$totalMigra[times], type='o', pch=20, lwd=1.25, col='#B32323',
      ylim=c(0, max(x$pop[times]) * uplim), ylab='Población (N)', xlab='Iteración', ...)
    }
    points(x$pop[times], lwd=2, col=col1, type='o', pch=20)
  }

  if(kind == 'foto' || kind == 3) {
      par(mfcol=c(1,2), mar=c(4,5.5,1.5,1.5))        
      plot(parches, yield=yield, pasto=pasto[[t_]], ylim=y.lims, xlim=x.lims, cex.lab=1.7, cex.axis=1.7)
      rec <- record[[t_]]
      noise1 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      noise2 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      points(rec$x + noise1, rec$y + noise2,
          pch='x', cex=resFactor * (rec$m / M),
          col=rainbow(maxname)[rec$name])
      legend('topleft', legend=x$pop[t_], cex=1.5, bty='n')
      
      plot(1:t_, plotGrass[1:t_], type='o', pch=19, lwd=1.25, col='#409951', xlim=c(1, t_),
        ylim=c(0, max(x$pop) * uplim), ylab='Población (N)', xlab='Iteración', , cex.lab=1.7, cex.axis=1.7)
      points(1:t_, x$pop[1:t_], lwd=3, col=col1, type='o', pch=20)
      par(mfcol=c(1,1))
  }
}

###############################################
###############################################

plot.lands <- function(x, yield=1, pasto=rep(yield, nrow(x$coordsAll)), cex.grass=3, pch.grass=19, ...) {
  with(x, {
    cex.grass <- cex.grass * (pasto / yield)
    plot(coordsAll, xlab='x', ylab='y', cex=cex.grass, pch=pch.grass, col='#409951', ...)
    rm(cex.grass)
    })
}

###############################################
###############################################

powerPts <- function(x, ptExp) {
  return((x - min(x) + 1) ^ ptExp)
}

###############################################
###############################################

print.ibm <- function(x, stats=TRUE) {

  if(stats) {
    print(x$indStats)
    print(x$lands)
  }

  if(stats) cat(paste('\nYIELD = ', round(x$parms$yield, 2), '\n', sep=''))

  tiempo   <- length(x$pop)
  halfTime <- round(tiempo / 2)
  pop1     <- mean(x$pop[1:(halfTime - 1)])
  mig1     <- mean(x$totalMigra[1:(halfTime - 1)])
# ~  popAll1  <- mean(x$popAll[1:(halfTime - 1)])
  pop2     <- mean(x$pop[halfTime:tiempo])
  mig2     <- mean(x$totalMigra[halfTime:tiempo])
# ~  popAll2  <- mean(x$popAll[halfTime:tiempo])

  nPatches  <- length(x$vecinos[[tiempo]])
  nVecinos  <- sapply(x$vecinos[[tiempo]], length)
  nOccupied <- sum(sum(nVecinos > 0))
  cat(paste(
    '\nTiempo total de simulación:\t', tiempo, ' iteraciones\n',
    '\nPoblación promedio, 1er. mitad:\t', round(pop1,2),
# ~    '\nPoblación promedio, 1er. mitad:\t', round(pop1,2), '\t/  ', round(popAll1,2),
    '\nPoblación promedio, 2da. mitad:\t', round(pop2,2), '\n',
# ~    '\nPoblación promedio, 2da. mitad:\t', round(pop2,2), '\t/  ', round(popAll2,2),'\n',

    '\nMigrantes promedio, 1er. mitad:\t', round(mig1,3),
    '\nMigrantes promedio, 2da. mitad:\t', round(mig2,3), '\n',

    '\nCantidad de parches ocupados (%):\t', paste(nOccupied, nPatches, sep='/'),
      ' (', round(100 * nOccupied / nPatches, 2), '%)',
#~     '\nAbundancias en parches:\n\t', nVecinos, '\n',
    sep=''))


  
# ~  nOccupied <- length(unique(x$record[[tiempo]][,c('x','y')]))
# ~  percOccup <- 100 * nOccupied / nPatches

  grass    <- x$pastoAll[[tiempo]]
  yield    <- x$parms$yield
  perYield <- 100 * (1 - mean(grass) / yield )
  
  cat(paste(
# ~    '\nPorcentaje de parches ocupados:\t', round(percOccup, 1), '%',
    '\nRecursos promedio por parche (nivel 0):\t', round(mean(grass), 1),
    '\nProm. de recursos consumidos:\t\t', round(perYield, 1), '%\n',
    '\nTIEMPO TOTAL DE SIMULACIÓN:\t', round(x$tiempo, 1),
    's.\n\n',
    sep=''))
}

###############################################
###############################################

print.ibmStats <- function(x) {
# x es un objeto de la clase ibmStats
    with(x, {
    cat(paste('\nAtributos individuales:\n\tMMD = ', round(MMD, 3),
      ' Km/day\tICL = ', round(ICL, 4), ' KJ/Km\tMMC = ', round(MMC, 4),
      ' KJ/day\n\tBMR = ', round(BMR,3), ' KJ/day\tMEI = ', round(MEI, 3),
      ' KJ/day\tPSI = ', round(PSI, 4), ' Kg/day\n\tM0 = ', round(M0, 2),
      ' Kg\t\tMPD = ', round(MPD, 3), ' Km', '\t\tTRS = ', round(TRS, 4),
      ' Kg\n\tTMC = ', TMC, ' Kg/day',
      ' Kg\n\tREE = ', REE, ' KJ',
      sep=''))
    cat(paste('\n\tTAMAÑO: ', round(M, 3), ' Kg\n'))
    })
}

###############################################
###############################################

print.lands <- function(x) {

  with(x$parms, {
    cat(paste('\nPropiedades del paisaje (', type, '):\n',
    '\tDIM = ', dim_, '\t\tDIST = ', round(dist_, 2), ' Km\n',
    '\tRDIST = ', round(rdist_, 2), '\tNIVELES = ', lmax_, '\n',
    '\tNo. PARCHES = ', n_, '\n',
    sep=''))
  })
}

###############################################
###############################################

rectas <- function(tabla) {

  masTramos <- TRUE 
  while(masTramos) {
    cat('\nVER GRÁFICA:\n')
    plot(popAllK ~ logSize, data=tabla, type='o', log='y')
    mastramos <- readline('¿Agregar un tramo?')
  }
  
  r <- lm(log10(popAllK) ~ logSize, data=tabla)
  plot(popAllK ~ logSize, data=tabla, type='o', log='y')
  abline(r, col=2, lwd=3)
  
  cat('\nRECTA AJUSTADA:')
  print(r)
  pendiente <- round(r$coefficients[2], 3)

  legend('bottomleft', title='Pendiente:', legend=pendiente,
      lwd=4, col=2, cex=2, bty='n')
}

###############################################
###############################################

results <- function(experimento='unParcheSolo') {

  filesDir <- paste('exp', experimento, sep='_')
  source(paste(filesDir, 'log.nfo', sep='/'))
  load(paste(filesDir, 'tablas.RData', sep='/'))
  load(paste(filesDir, 'sizesValues.RData', sep='/'))
  
  if(nValues == 1) {
  
    tabla <- tablas[1]
  
    cat('
    K ESTIMADOS PARA DISTINTOS TAMAÑOS CORPORALES.
    ')
    
    plot(popAllK ~ logSize, data=tabla, type='o')
    
    cat(paste('\n
    Se muestra el tamaño poblacional promedio alcanzado\n
    (entre t=', tfinal - ultimos,' y t=', tfinal,') para cada tamaño corporal (NEGRO).\n
    *  Se consideran TODOS los individuos (incluidos los
       que tienen pocas reservas).\n\n'))
    
    nada <- readline('Agregar los tamaños poblacionales para individuos con\n\treservas > (1.5 * minres)?(s/N): ')
    
    if(nada != "" && (nada == "s" || nada == "S")) {
    
      points(popK ~ logSize, data=tabla, type='o', col=4)
    
      cat('
      Se muestra el tiempo de simulación en función del
      tamaño poblacional promedio (AZUL).
      \n')
    
    }
    
    nada <- readline('\nVer las dinámicas poblacionales?(s/N):\n\t(todas las simulaciones) ')
    
    if(nada != "" && (nada == "s" || nada == "S")) {
      for(s in 1:nSizes) {
        cat(paste('\nSize nro.', s, 'de', nSizes, '\n'))
        for(r in 1:nReps) {
    
          nada <- readline(paste('Repetición nro.:', r, 'de', nReps))
          load(paste(filesDir, '/value_1_size_', s,
                    '_rep_', r, '.RData', sep=''))
          plot(x$popAll, type='o')
          points(x$pop, type='o', col=4)
          print(paste('Promedio de Nro. de individuos:',
              round(mean(x$pop[(tfinal - ultimos):tfinal], 2))))
        }
      }
    }
    
    nada <- readline('\nVer escalamientos (regresiones) de las densidades poblacionales?\n(incluye individuos con pocas reservas)\n\t(S/n):\t\t')
    
    if(nada == "" || nada == "s" || nada == "S") {
    
      r <- lm(log10(popAllK) ~ logSize, data=tabla)
      plot(popAllK ~ logSize, data=tabla, type='o', log='y')
      abline(r, col=2, lwd=3)
      
      cat('\nRECTA AJUSTADA:')
      print(r)
      pendiente <- round(r$coefficients[2], 3)
    
      legend('bottomleft', title='Pendiente:', legend=pendiente,
          lwd=4, col=2, cex=2, bty='n')
    }
  
  
  } else {
  
    sizes <- seq(logMinSize, logMaxSize, length.out=nSizes)
    values <- seq(minValue, maxValue, length.out=nValues)
    values <- unique(values)
  
    nada <- menu(title='\nQué tipo de población querés visualizar?',
          choices=c(
            'Todos los individuos',
            'Sólo los que tienen reservas > 1.5 * minres'))
  
    if(nada == 1) tabla <- sizesValues else tabla <- sizesValuesAll
  
    colores <- heat.colors(nValues + 7)
    
    matplot(sizes, tabla, type='o', xlab='log Size', ylab='K estimado', lwd=3, lty=1, col=colores[1:nValues])
    
    legend('topright', lwd=3, col=colores, bty='n', cex=1.5, legend=paste(variable, '=', values))
    
    cat(paste('\n
       Densidades poblacionales para cada tamaño corporal (en log)\n\n
       Cada línea es el resultado con un valor distinto de', variable, '\n\n'))
  
       
    nada <- readline('\n>>> Siguiente: relaciones yield ~ K (estimado) para para cada M...\n')
    
    colores <- heat.colors(nSizes + 7)
    
    matplot(values, t(tabla), type='o', lwd=3, lty=1, ylab='K estimado', xlab=variable, col=colores[1:nSizes])
    
    legend('topleft', pch=19, pt.cex=4, legend=c('Min Size', 'Max Size'),
        col=c(colores[1], colores[nValues]), cex=2, bty='n')
    
    cat(paste('\n
       Densidades poblacionales para cada valor de', variable, '\n\n
       Cada línea corresponde a un tamaño corporal distinto\n\n
       (los tamaños corporales aumentan haca el extremo amarillo)\n\n'))
  }
}

###############################################
###############################################

sdaFun <- function(x, sdaMax=sdaMax, mei=mei[i]) {
  return(x * sdaMax / mei)
}

###############################################
###############################################

seeStats <- function(x) print(x$indStats)

###############################################
###############################################

stats <- function() {

  require(ellipse, quietly=TRUE)

  with(parent.frame(), {
    MMD <- mmd0 * M ^ mmdExp
    ICL <- icl0 * M ^ iclExp
    MMC <- MMD * ICL
    M0  <- pm * M
    MPD <- mpd0 * M ^ mpdExp
    BMR <- bmr0 * M ^ bmrExp
    MEI <- mei0 * BMR
    TMC <- B_c * M / E_cr
    TRS <- trs0 * M ^ trsExp
    REE <- TRS * E_cr
    PSI <- REE + npsi * MEI * m_c / E_cr - TMC
    if(ptsMode == 'PEB') PSI <- PSI - REE
  
    out <- list(MMD=MMD, ICL=ICL, MMC=MMC, M=M,  M0=M0,
          MEI=MEI, MPD=MPD, PSI=PSI, BMR=BMR, REE=REE, TMC=TMC, TRS=TRS)
  
    class(out) <- 'ibmStats'
# ~  
# ~    if(printOut) print(out)
  
    return(out)
  })
}

###############################################
###############################################

## TASA DE CRECIMIENTO PER CÁPITA
tasa <- function(x) {
# x es un objeto 'ibm'
  tfinal <- length(x$pop)
  Rt <- log((x$pop[2:tfinal]) / (x$pop[1:(tfinal - 1)]))
  tiempo <- 1:(tfinal - 1)
  halfTime <- round(tfinal / 2)
  Nbefore <- x$pop[1:(tfinal - 1)]
  plot(Rt ~ tiempo, xlab='t-1', main='R[dt] vs. tiempo'); ejes()
  nada <- readline('Enter para seguir...')
  plot(Rt ~ Nbefore, xlab='N[t-1]', main='R[dt] vs. N[t-1]'); ejes()
  nada <- readline('Enter para seguir...')
# ~  plot(Rt ~ x$pop[1:(tfinal - 1)], type='l')
  t_ <- halfTime:(tfinal - 1)
  recta <- lm(Rt[t_] ~ Nbefore[t_])
  abline(recta, col=3, lwd=2)
  print(recta)
  a <- recta$coefficients[2]
  b <- recta$coefficients[1]
  K <- - b / a
  print(paste('El K aproximado es:', round(K, 2)))
}

ibmplot <- function(x, landsTitle='A Simple Plot', popTitle='Dinámica Poblacional',
            xlab='Iteración (t)', ylab='Abundancia (N[t])', ini=1, fin=2) {
  ## Ejemplo:
  ## x <- ibm(tfinal=100)
  ## for(i in 1:100) { grid.newpage(); ibmplot(x, fin=i) }
  coordsAll <- x$lands$coordsAll
  x_ <- coordsAll$x
  y_ <- coordsAll$y
  landPlot  <- viewport(name="landscape", x=unit(5, "lines"), y=unit(4, "lines"),
    width=unit(0.5, "npc") - unit(7, "lines"), height=unit(1, "npc") - unit(7, "lines"),
    just=c("left", "bottom"), xscale=range(x_) + c(-0.05,  0.05) * diff(range(x_)),
    yscale=range(y_) + c(-0.05, 0.05) * diff(range(y_)))
  pushViewport(landPlot)
  grid.points(x_, y_, pch=19)#, col='#409951')
  rec <- x$record[[fin]]
  grid.points(rec$x, rec$y, pch='X')#, col='#409951')
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.text('<-- x -->', y=unit(-2.5, "lines"),
    gp=gpar(fontsize=13))
  grid.text('<-- y -->', x=unit(-3.5, "lines"),
    gp=gpar(fontsize=13), rot=90)
  grid.text(landsTitle, y=unit(1, "npc") + unit(1,
    "lines"), gp=gpar(fontsize=15))
  upViewport()
  
  tf <- x$parms$tfinal
  times <- ini:fin
  pop <- x$pop
  popPlot <- viewport(name="population", x=unit(5, "lines") + unit(0.5, "npc"), y=unit(4, "lines"),
    width=unit(0.5, "npc") - unit(7, "lines"), height=unit(1, "npc") - unit(7, "lines"),
#~     layout=grid.layout(ncol=2), height=unit(1, "npc") - unit(7, "lines"),
    just=c("left", "bottom"), xscale=c(1, tf) + c(-0.05,  0.05) * (tf - 1),
    yscale=range(pop) + c(-0.05, 0.05) * diff(range(pop)))
  pushViewport(popPlot)
  grid.lines(times, pop[times])#, col='#409951')
  grid.points(times, pop[times], pch=20)
  grid.rect()
  grid.xaxis()
  grid.yaxis()
  grid.text(xlab, y=unit(-2.5, "lines"),
    gp=gpar(fontsize=13))
  grid.text(ylab, x=unit(-3.5, "lines"),
    gp=gpar(fontsize=13), rot=90)
  grid.text(popTitle, y=unit(1, "npc") + unit(1,
    "lines"), gp=gpar(fontsize=15))
  upViewport()
}
code <- function(){
  plot(plotGrass[times], type='o', pch=19, lwd=1.25, col='#409951',
    ylim=c(0, max(x$pop[times]) * uplim), ylab='Población (N)', xlab='Iteración', ...)
  points(x$pop[times], lwd=3, col=col1, type='o', pch=20)
}
