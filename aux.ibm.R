# FUNCIONES AUXILIARES PARA ibm

bholt <- function(N, Ro=2, K=yield) {
# Con Ro = 2 en 20 iteraciones llega al máximo (No=1), al menos
# en el rango de valores de yield entre 3000 y 50000
  # K = (Ro - 1) * M
  M <- K / (Ro - 1)
  return(Ro * N / (1 + N / M))
}
############
############
# Para ver la dinámica:
bholtDyn <- function(No=1, Ro=2, K=1e4, tfinal=20, ...) {
  # K = (Ro - 1) * M 
  M <- K / (Ro - 1)
  out <- vector('numeric', length=tfinal)
  out[1] <- No
  for (t in 2:tfinal) {
    out[t] <- Ro * out[t-1] / (1 + out[t-1] / M)
  }
  plot(out, ...)
  return(out)
}
############
############
# Para ver la dinámica:
bholtDyn2 <- function(No=1, Ro=2, M=1, tfinal=20, ...) {
  # K = (Ro - 1) * M 
#    M <- K / (Ro - 1)
  out <- vector('numeric', length=tfinal)
  out[1] <- No
  for (t in 2:tfinal) {
    out[t] <- Ro * out[t-1] / (1 + out[t-1] / M)
  }
  plot(out, ...)
  return(out)
}
############
############
# chFun
chooseFromAll <- function(puntaje, critQuant=0.9) {
  sample(length(puntaje), 1, prob=puntaje)
}
chooseMax <- function(puntaje, critQuant=0.9) {
  out <- which.max(puntaje)
  if (length(out) > 1) {
    out <- sample(out, 1)
  }
  out
}
chooseQuant <- function(puntaje, critQuant=0.9) {
  valor <- quantile(puntaje, critQuant)
  cond  <- puntaje >= valor
  bag <- which(cond)
  if (sum(cond) > 1) {
    out <- sample(bag, 1)
  } else {
    out <- bag
  }
  out
}
############
############
distancias <- function(xyInd, xyParches) 
{
  if (!is.matrix(xyParches)) xyParches <- matrix(xyParches, ncol=2)
  out <- sqrt((xyParches[,1] - xyInd[1]) ^ 2 + (xyParches[,2] - xyInd[2]) ^ 2)
    return(out)
}
############
############
enerObt <- function(x, sdaMax=sdaMax, mei=mei[i]) x * (1 - x * sdaMax / mei)
############
############
feed <- function(foodAcum, mei, pasto) {
  return(ifelse(foodAcum + pasto <= mei, pasto, mei - foodAcum))
}
############
############
# 0.1 Objetos fijos
fixedObjects <- function() {
  with(parent.frame(), {
    m0     <- pm * M
    trsMax <- trs0 * M ^ trsExp
    trsMin <- trs0 * m0 ^ trsExp
    minBio <- m0 + (trsMin * E_cr / E_c)
    B_c    <- 2 * m_c * bmr0 / M ^ (1 / 4)
    #->Asume que k promedio es 2 (i.e.: average energy intake = 2 * BMR)
    #->El tejido de reserva tiene un valor diferente, del punto de vista
    #  energético, al tejido de normal. Para simplificar los cálculos,
    #  el valor 'minBio' es la suma del tejido normal y de reserva de un
    #  recién nacido, pero todo en el equivalente a tejido normal.
    #  Esta medida sirve para calcular en número de descendientes de cada
    #  adulto en el momento de reproducción.
    cfaRand <- ellipse(0, centre=c(0, 0), t=1, npoints=60)
  })
}
############
############
gompertz <- function(x, p.max, gompB, gompC) {
# ej: curve(gompertz(x, 1, -5.5, -1.5), from=-1, to=8, ylim=c(0,1)); ejes()
# En modo Auto:
#   gompB <- log(p.0)
#   gompC <- log(log(p1) / log(p0)) / psi[i]
  return(p.max * exp(gompB * exp(gompC * x)))
}
############
############
# 2. REGENERACIÓN DE PASTO
grassGrow <- function() {
  with(parent.frame(), {
    switch(grassMode,
      fixed={
        # Los parches con poco pasto (pero > 0), crecen con dinámica bholt.
        pasto[pasto < yield] <- yield
      },
      semiFixed={
        # Los parches con poco pasto (pero > 0), crecen con dinámica bholt.
        raleados <- which(pasto > tol & pasto < yield)
        if (length(raleados) > 0) {
          pasto[raleados] <- bholt(pasto[raleados], Ro=pastoRo, K=yield)
        }
        # Los parches sin pasto vuelven a su valor de yield automáticamente.
        pasto[pasto < tol] <- yield
      },
      bholt={
        # Los pastos crecen con una función logística.
        pasto[pasto < tol] <- 1
        pasto <- bholt(pasto, Ro=pastoRo, K=yield)
      },
      randSprout={
        # Los parches con poco pasto (pero > 0), crecen con dinámica bholt.
        raleados <- which(pasto > tol & pasto < yield)
        if (length(raleados) > 0) {
          pasto[raleados] <- bholt(pasto[raleados], Ro=pastoRo, K=yield)
        }
        # Plantaría semillas aleatoriamente sobre los parches
        # sin pasto, de forma que vuelven al valor de yield...
        tomuer  <- pasto < tol
        luckies <- as.logical(sample(0:1, length(tomuer), replace=TRUE, prob=rep(grassProb, 2)))
        pasto[tomuer & luckies] <- yield
      }
    )
  })

}
############
############
importer <- function(im, tfinal) {
# im= ibm importado
  out <- NULL
  out$tfinal <- tfinal
  out$extra_t <- im$tiempo
  pr  <- im$parms[names(im$parms) != 'import']
  out <- c(pr, im[!(names(im) %in% c('call', 'extinction'))]) 
  out <- within(pr, {
    t_ <- length(im$pop)
    tfinal  <- tfinal + t_
    lands   <- im$lands
    N       <- im$pop[t_]
    m0      <- pm * M
    trsMax  <- trs0 * M ^ trsExp
    trsMin  <- trs0 * m0 ^ trsExp
    minBio  <- m0 + (trsMin * E_cr / E_c)
    B_c     <- 2 * m_c * bmr0 / M ^ (1 / 4)
    cfaRand <- ellipse(0, centre=c(0, 0), t=1, npoints=60)
  
    babyBiom  <- im$babyBiom
    optPatch  <- im$optPatch
    foodAcum  <- im$record[[t_]]$foodAcum
    nombres   <- im$record[[t_]]$name
    reser     <- im$record[[t_]]$reser
    lastname  <- nombres[N]
    m         <- im$record[[t_]]$m; pop[1] <- N
    xypos     <- as.matrix(im$record[[t_]][, c('x','y')])
    indStats  <- im$indStats
    pointsFun <- im$pointsFun
  
    pastoAll <- vector('list', tfinal)
    pastoAll[1:t_] <- im$pastoAll
    pasto <- pastoAll[[t_]]
    pop <- numeric(tfinal)
    pop[1:t_] <- im$pop
    births <- numeric(tfinal)
    births[1:t_] <- im$births
    deaths <- numeric(tfinal)
    deaths[1:t_] <- im$deaths
    ijMigra <- numeric(tfinal)
    ijMigra[1:t_] <- im$ijMigra
    popMigra <- numeric(tfinal)
    popMigra[1:t_] <- im$popMigra
    totalMigra <- numeric(tfinal)
    totalMigra[1:t_] <- im$totalMigra
    totalEmigra <- numeric(tfinal)
    totalEmigra[1:t_] <- im$totalEmigra
    totalInmigra <- numeric(tfinal)
    totalInmigra[1:t_] <- im$totalInmigra
    record <- vector('list', tfinal)
    record[1:t_] <- im$record
    migra <- vector('list', tfinal)
    migra[1:t_] <- im$migra
    migra_t <- migra[[t_]]
    vecinos <- vector('list', tfinal)
    vecinos[1:t_] <- im$vecinos
    vecinos_t <- vecinos[[t_]]
    t_ <- t_ + 1
  })
  return(out)
}
############
############
# 0.2 Individuos iniciales
indivSeed <- function() {
  with(parent.frame(), {
    if (levelSeeds < 0 || tolower(levelSeeds) == 'random') {
    # Repartija aleatoria de individuos en los parches nivel 0
      N       <- N_0
      nombres <- 1:N
      pos     <- sample(nrow(xypasto), N, replace=TRUE)
      xypos   <- xypasto[pos,]
      if (length(pos) == 1) {
      # Para que siempre sea una matriz:
        xypos <- matrix(xypos, ncol=2)
      }
    } else {
    # Repartijas con criterios más selectivos
      npatch  <- length(lands$areas[[levelSeeds + 1]])
      N       <- npatch
      nombres <- 1:N
      if (levelSeeds %in% c(0, 'all')) {
      # Un individuo por parche de nivel 0
        pos <- 1:N
      } else {
      # Un individuo por parche de nivel 'levelSeeds'
        pos <- numeric(npatch)
        for (i in 1:npatch) {
          cuales <- which(lands$belong[,levelSeeds + 1] == i)
          pos[i] <- lands$belong[sample(cuales, 1),1]
        }
      }
      xypos <- xypasto[pos,]
      if (length(pos) == 1) {
      # Para que siempre sea una matriz:
        xypos <- matrix(xypos, ncol=2)
      }
    }
  
    if (addGuys && N < N_0) {
    # Si luego de sembrar individuos me siguen sobrando (número de parches es
    # menor que N_0), agrego nuevos individuos en lugares al azar:
      more    <- N_0 - N
      N       <- N_0
      nombres <- 1:N
      posMore <- sample(nrow(xypasto), more, replace=TRUE)
      xypos   <- rbind(xypos, xypasto[posMore,])
    }
  
    lastname <- N
    foodAcum <- numeric(N)
    m        <- rep.int(M, N)
  #    m        <- rep.int(m0, N)
  #    m        <- runif (N, m0, M)
    reser    <- trs0 * m ^ trsExp
    babyBiom <- numeric(N)
    optPatch <- numeric(N) * NA
  })
}

############
############
logistica <- function(x, logitA0, logitA1) {
# ~> curve(logit(x, -2.5, .06), from=-10, 10)
# En modo Auto:
#   logitA0 <- log(p.0 / (1 - p.1))
#   logitA1 <- log((p.1 * (1 - p.0)) / (p.0 * (1 - p.1))) / psi[i]
  Y <- (logitA0 + x * logitA1)
  return(p.max * exp(Y) / (1 + exp(Y)))
}
############
############
# ptsFun
# Parámetros:
logitA0=-2.5 # numeric
logitA1=.06 # numeric
gompB=-5.5
gompC=-1.5
p.max=10 # numeric
p.0=0.01 # probability
p.1=0.9 # probability
# --+
pfLogitAuto <- function() {
  out <- with(parent.frame(), {
    logitA0 <- log(p.0 / (1 - p.1))
    logitA1 <- log((p.1 * (1 - p.0)) / (p.0 * (1 - p.1))) / psi[i]
    Y <- (logitA0 + input * logitA1)
    p.max * exp(Y) / (1 + exp(Y))
  })
  return(out)
}
pfLogitManual <- function() {
  out <- with(parent.frame(), {
    Y <- logitA0 + input * logitA1
    p.max * exp(Y) / (1 + exp(Y))
  })
  return(out)
}
pfGompAuto1 <- function() {
  out <- with(parent.frame(), {
    gompB <- log(p.0)
    gompC <- log(log(p.1) / log(p.0)) / psi[i]
    p.max * exp(gompB * exp(gompC * input))
  })
  return(out)
}
pfGompAuto2 <- function() {
  out <- with(parent.frame(), {
    gompB <- - 1
    gompC <- log(- log(p.1)) / psi[i]
    p.max * exp(gompB * exp(gompC * input))
  })
  return(out)
}
pfGompManual <- function() {
  out <- with(parent.frame(), {
    p.max * exp(gompB * exp(gompC * input))
  })
  return(out)
}
############
############

mklands <- function(dim_=2, dist_=1, lmax_=2, n_=3, rdist_=3, type='fractal') {
# ejemplo:
#   plot(mklands())
  require(splancs, quietly=TRUE)
  require(ellipse, quietly=TRUE)
  n_ <- round(n_)
  npatch <- (n_ ^ dim_) ^ lmax_

  areas  <- vector('list', lmax_)
  belong <- as.data.frame(matrix(1:npatch, nrow=npatch, ncol=lmax_))
  names(belong) <- names(areas) <- paste('l', 0:(lmax_ - 1), sep='')

  if (lmax_ <= 1) {
    areas <- vector('list', 1)
    if (npatch == 1) {
      coordsAll <- matrix(0, 1, 2)
      pos       <- 0
    }
  }

  if (type == 'fractal' && npatch > 1) {
    dists_ <- NULL
    seg <- dists_
    for (l_ in 1:lmax_) {
      for (i in 1:(n_ - 1)) {
        dists_ <- c(dists_, c(dist_ * rdist_ ^ (l_ - 1), seg))
      }
      seg <- dists_
    }
    pos <- c(0, cumsum(dists_))
  }

  if (type == 'regular' && npatch > 1) {
    n_ <- (n_ ^ dim_) ^ lmax_
    pos <- 0:n_ * dist_
  }

  if (type == 'randUnif' && npatch > 1) {
    x  <- sort(runif (npatch, 0, (n_ - 1) * dist_))
    y  <- runif (npatch, 0, (n_ - 1) * dist_)
    coordsAll <- cbind(x=x, y=y)
    pos <- NULL
  }

  if (type != 'randUnif' && npatch > 1) {
    npos      <- length(pos)
    coordsAll <- matrix(nrow=npatch, ncol=2)

    p <- 0:npos * npos + 1
    for (i in 1:npos) {
      coordsAll[p[i]:(p[i + 1] - 1),] <- cbind(pos[i], pos)
    }

    if (type == 'fractal' && lmax_ > 1) {

      for (l_ in 1:(lmax_ - 1)) {
        p   <- 1
        nl  <- n_ ^ l_
        b   <- (dist_ * rdist_ ^ l_) * .15
        ini <- 0:(npos / nl - 1) * nl + 1
        level <- vector('list', (n_ ^ dim_) ^ (lmax_ - l_))

        for (j in ini) {
          x <- c(pos[j], pos[j + nl - 1])
          for (i in ini) {
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
  for (i in 1:nrow(coordsAll)) {
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
# Ejemplo x <- mklands() (paisaje fractal):
# x
#   areas
#     l0: areas al nivel 0, circulares
#     l1: areas al nivel 1, cuadradas
#     ...
#     ln: areas al nivel n, cuadradas
#   belong
#     l0: pertenencia de parches respecto al nivel 0
#     l1: pertenencia de parches respecto al nivel 1
#     ...
#     ln: pertenencia de parches respecto al nivel n
#   coordsAll: coordenadas x y de todos los parches nivel 0
#   parms
#     dim_:  dimensión del paisaje (2)
#     dist_: distancia entre parches nivel 0
#     lmax_: número de niveles del paisaje
#     n_: número de parches 0 en un parche nivel 1
#     rdist_: razón de distancias entre niveles consecutivos
#     type: tipo de paisaje ('fractal' por defecto).
#   pos: posiciones de los parches en la proyección sobre cualquiera de los ejes
#        de coordenadas.

############
############
mod <- function(x, y) { out <- x %% y; out[out == 0] <- y; return(out)}
############
############
msd <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(sd(total))
}
############
############
mvar <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(var(total))
}
############
############
plot.ibm <- function(x, kind='animation', outdir='default', nmax=500,
            type='l', col1=1, col2=8, uplim=1, areaFactor=1.3, t_=1,
            resFactor=2, noiseFactor=1/7, from, to, ...) {

  M         <- x$parms$M
  parches   <- x$lands
  pos       <- x$lands$pos
  coordsAll <- x$lands$coordsAll
  record    <- x$record
  tfinal    <- length(x$pop)
  if (tfinal < x$parms$tfinal) {
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

   if (outdir == 'default') out <- 'animation'
  else {
    out <- outdir
    system(paste('mkdir', out))
  }
  
  if (kind == 'animation' || kind == 1) {

    require(animation)
    height <- 1.3 * 480
    width  <- 2 * (1.3) * 480
    oopt <- ani.options(interval=0.4, nmax=100, outdir=out, ani.height=height, ani.width=width)

# ~    if (tfinal > 100) {
# ~      div <- round(tfinal / 100)
# ~      times <- (1:tfinal)[(1:tfinal %% div) == 0]
# ~    } else {

# ~    }

    ani.start()
      for (i in times) {
        plot(1,1,type='n')
      }
    ani.stop()
#      ani.start()
    k <- 1
    for (i in times) {
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
#          dev2bitmap(paste('animation/images/', k, '.png', sep=''), width=width, height=height, units='px')
      k <- k + 1
    }
#      ani.stop()
  }

  if (kind == 'pop' || kind == 2) {
    if (x$parms$grassMode != 'fixed') {
      plot(plotGrass[times], type='o', pch=19, lwd=1.25, col='#409951',
      ylim=c(0, max(x$pop[times]) * uplim), ylab='Población (N)', xlab='Iteración', ...)
      points(x$totalMigra[times], lwd=1.25, col='#B32323', type='o', pch=20)
    } else {
      plot(x$totalMigra[times], type='o', pch=20, lwd=1.25, col='#B32323',
      ylim=c(0, max(x$pop[times]) * uplim), ylab='Población (N)', xlab='Iteración', ...)
    }
    points(x$pop[times], lwd=2, col=col1, type='o', pch=20)
  }

  if (kind == 'foto' || kind == 3) {
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
############
############
plot.lands <- function(x, yield=1, pasto=rep(yield, nrow(x$coordsAll)), cex.grass=3, pch.grass=19, ...) {
  with(x, {
    cex.grass <- cex.grass * (pasto / yield)
    plot(coordsAll, xlab='x', ylab='y', cex=cex.grass, pch=pch.grass, col='#409951', ...)
    rm(cex.grass)
    })
}
############
############
powerPts <- function(x, ptExp) {
  return((x - min(x) + 1) ^ ptExp)
}
############
############
print.ibm <- function(x, stats=TRUE) {
  if (stats) {
    print(x$indStats)
    print(x$lands)
  }

  if (stats) cat(paste('\nYIELD = ', round(x$parms$yield, 2), '\n', sep=''))

  tiempo   <- length(x$pop)
  halfTime <- round(tiempo / 2)
  pop1     <- mean(x$pop[1:(halfTime - 1)])
  mig1     <- mean(x$totalMigra[1:(halfTime - 1)])
  pop2     <- mean(x$pop[halfTime:tiempo])
  mig2     <- mean(x$totalMigra[halfTime:tiempo])

  lands <- x$lands
  totalPatches0 <- with(lands, length(pos) ^ parms$dim_)
  xypos <- x$record[[tiempo]][, c('x', 'y')]
  count <- 0
  for (i in 1:totalPatches0) {
    vec <- inpip(xypos, lands$areas$l0[[i]], bound=TRUE)
    count <- count + (length(vec) > 0)
  }
  patch0    <- count / totalPatches0
  nPatches  <- length(x$vecinos[[tiempo]])
  nVecinos  <- sapply(x$vecinos[[tiempo]], length)
  nOccupied <- sum(sum(nVecinos > 0))
  cat(paste(
    '\nTiempo total de simulación:\t', tiempo, ' iteraciones\n',
    '\nPoblación promedio, 1er. mitad:\t', round(pop1,2),
    '\nPoblación promedio, 2da. mitad:\t', round(pop2,2), '\n\n',
    '\nMigrantes promedio, 1er. mitad:\t', round(mig1,3),
    '\nMigrantes promedio, 2da. mitad:\t', round(mig2,3), '\n\n',
    '\nCantidad de parches 0 ocupados (%):\t', paste(count, totalPatches0, sep='/'),
      ' (', round(100 * patch0, 2), '%)',
    '\nCantidad de parches ocupados (%):\t', paste(nOccupied, nPatches, sep='/'),
      ' (', round(100 * nOccupied / nPatches, 2), '%)',
    sep=''))

  grass    <- x$pastoAll[[tiempo]]
  yield    <- x$parms$yield
  perYield <- 100 * (1 - mean(grass) / yield )
  
  cat(paste(
    '\nRecursos promedio por parche (nivel 0):\t', round(mean(grass), 1),
    '\nProm. de recursos consumidos:\t\t', round(perYield, 1), '%\n',
    '\nTIEMPO TOTAL DE SIMULACIÓN:\t', round(x$tiempo, 1),
    's.\n\n',
    sep=''))
}
############
############
print.ibmStats <- function(x) {
# x es un objeto de la clase ibmStats
    with(x, {
    cat(paste('\nAtributos individuales:\n\tMMD = ', round(MMD, 3),
      ' Km/day\tICL = ', round(ICL, 4), ' KJ/Km\tMMC = ', round(MMC, 4),
      ' KJ/day\n\tBMR = ', round(BMR,3), ' KJ/day\tMEI = ', round(MEI, 3),
      ' KJ/day\tPSI = ', round(PSI, 4), ' Kg/day\n\tM0 = ', round(M0, 2),
      ' Kg\t\tMPD = ', round(MPD, 3), ' Km', '\t\tTRS = ', round(TRS, 4),
      ' Kg\n\tTMC = ', round(TMC, 3), ' Kg/day',
      ' Kg\n\tREE = ', round(REE, 3), ' KJ',
      sep=''))
    cat(paste('\n\tTAMAÑO: ', round(M, 3), ' Kg\n'))
    })
}
############
############
print.lands <- function(x) {

  with(x$parms, {
    cat(paste('\nPropiedades del paisaje (', type, '):\n',
    '\tDIM = ', dim_, '\t\tDIST = ', round(dist_, 2), ' Km\n',
    '\tRDIST = ', round(rdist_, 2), '\tNIVELES = ', lmax_, '\n',
    '\tNo. PARCHES = ', n_, '\n',
    sep=''))
  })
}
############
############
rectas <- function(tabla) {

  masTramos <- TRUE 
  while (masTramos) {
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
############
############
# 0.3 Registros:
register <- function() {
  with(parent.frame(), {
    #  0.3.a Vectores
    pasto         <- rep.int(yield, nrow(xypasto))
    pastoAll      <- vector('list', length=tfinal)
    pastoAll[[1]] <- pasto
    pop           <- numeric(tfinal)
    births        <- numeric(tfinal)
    deaths        <- numeric(tfinal)
    extra_t       <- 0
    t_ <- 2; pop[1] <- N
  
    #  0.3.b Tabla
    record      <- vector('list', tfinal)
    record[[1]] <- data.frame(foodAcum=foodAcum,
                name=nombres, m=m, #pos=pos,
                reser=reser,
                x=xypos[,1],
                y=xypos[,2])
  
    # 0.3.c Migración
    npatch  <- length(lands$areas[[levelFocus + 1]])
    migra   <- vector('list', tfinal)
    migra_t <- matrix(0, npatch, npatch) -> migra[[1]]
    #-->migra_t[i,j] = migración de j --> i
    emigra    <- vector('list', tfinal)
    emigra_t  <- vector('list', npatch)
    inmigra   <- vector('list', tfinal)
    inmigra_t <- vector('list', npatch)
    vecinos   <- vector('list', tfinal)
    vecinos_t <- vector('list', npatch)
    for (v in 1:npatch) {
      emigra_t[[v]]  <- 0
      inmigra_t[[v]] <- 0
      vecinos_t[[v]] <- nombres[inpip(xypos,
                                      lands$areas[[levelFocus + 1]][[v]],
                                      bound=TRUE)]
    }
    vecinos[[1]] <- vecinos_t
  })
}
############
############
results <- function(experimento='unParcheSolo') {
  filesDir <- paste('exp', experimento, sep='_')
  source(paste(filesDir, 'log.nfo', sep='/'))
  load(paste(filesDir, 'tablas.RData', sep='/'))
  load(paste(filesDir, 'sizesValues.RData', sep='/'))
  
  if (nValues == 1) {
  
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
    
    if (nada != "" && (nada == "s" || nada == "S")) {
    
      points(popK ~ logSize, data=tabla, type='o', col=4)
    
      cat('
      Se muestra el tiempo de simulación en función del
      tamaño poblacional promedio (AZUL).
      \n')
    
    }
    
    nada <- readline('\nVer las dinámicas poblacionales?(s/N):\n\t(todas las simulaciones) ')
    
    if (nada != "" && (nada == "s" || nada == "S")) {
      for (s in 1:nSizes) {
        cat(paste('\nSize nro.', s, 'de', nSizes, '\n'))
        for (r in 1:nReps) {
    
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
    
    if (nada == "" || nada == "s" || nada == "S") {
    
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
  
    if (nada == 1) tabla <- sizesValues else tabla <- sizesValuesAll
  
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
############
############
sdaFun <- function(x, sdaMax=sdaMax, mei=mei[i]) {
  return(x * sdaMax / mei)
}
############
############
seeStats <- function(x)
  print(x$indStats)
############
############
seeTime <- function(t_, pop)
    cat(paste('| ', t_, ' N=', pop[t_], ' ', sep=''))
############
############
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
    if (ptsMode == 'PBB')
      PSI <- PSI - REE
  
    out <- list(MMD=MMD, ICL=ICL, MMC=MMC, M=M,  M0=M0, MEI=MEI,
                MPD=MPD, PSI=PSI, BMR=BMR, REE=REE, TMC=TMC, TRS=TRS)
  
    class(out) <- 'ibmStats'
    return(out)
  })
}
############
############
# TASA DE CRECIMIENTO PER CÁPITA
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
############
############
# Experimental...
ibmplot <- function(x, landsTitle='A Simple Plot', popTitle='Dinámica Poblacional',
            xlab='Iteración (t)', ylab='Abundancia (N[t])', ini=1, fin=2) {
  # Ejemplo:
  # x <- ibm(tfinal=100)
  # for (i in 1:100) { grid.newpage(); ibmplot(x, fin=i) }
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
#      layout=grid.layout(ncol=2), height=unit(1, "npc") - unit(7, "lines"),
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
