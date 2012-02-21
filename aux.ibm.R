# FUNCIONES AUXILIARES PARA ibm

bholt <- function(N, Ro=2, K=yield) {
# Con Ro = 2 en 20 iteraciones llega al máximo (No=1), al menos
# en el rango de valores de yield entre 3000 y 50000
  # K = (Ro - 1) * M
  M <- K / (Ro - 1)
  return(Ro * N / (1 + N / M))
}
####################################
####################################
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
####################################
####################################
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
####################################
####################################
calcLims <- function(pos, areaFactor=1.3) {
  x.lims <- y.lims  <- range(pos)
  ancho <- diff(x.lims)
  y.lims[2] <- y.lims[1] + ancho * (1 + areaFactor) / 2
  y.lims[1] <- y.lims[1] + ancho * (1 - areaFactor) / 2
  x.lims[2] <- x.lims[1] + ancho * (1 + areaFactor) / 2
  x.lims[1] <- x.lims[1] + ancho * (1 - areaFactor) / 2
  return(list(x.lims=x.lims, y.lims=y.lims))
}
####################################
####################################
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
chooseRandom <- function(puntaje, critQuant=0.9) {
  sample(length(puntaje), 1)
}
####################################
####################################
distancias <- function(xyInd, xyParches) {
  if (length(xyParches) == 2)
    dim(xyParches) <- c(1, 2)
  out <- sqrt((xyParches[,1] - xyInd[1]) ^ 2 +
              (xyParches[,2] - xyInd[2]) ^ 2)
  return(out)
}
####################################
####################################
enerObt <- function(x, sdaMax=sdaMax, mei=mei[i]) x * (1 - x * sdaMax / mei)
####################################
####################################
feed <- function(foodAcum, mei, pasto) {
  return(ifelse(foodAcum + pasto <= mei, pasto, mei - foodAcum))
}
####################################
####################################
# 0.1 Fixed object creation:
fixedObjects <- function() {
  with(parent.frame(), {
    # Error tolerance:
    tol <- 1e-6
    # Fixed individual stats:
    m0     <- pm * M
    trsMax <- trs0 * M ^ trsExp
    trsMin <- trs0 * m0 ^ trsExp
    minBio <- m0 + (trsMin * E_cr / E_c)
    B_c    <- bmr0 * m_c * M ^ (bmrExp - 1)
    cfaRand <- ellipse(0, centre=c(0, 0), t=1, npoints=60)
    xypasto <- as.matrix(lands$coordsAll)
    npatchFocus <- length(lands$areas[[levelFocus + 1]])
  })
}
####################################
####################################
getNormSd <- function(x, init=1) {
# La idea es que x normalmente sea la población.
# Calcularía el desvío estándar normalizado del tamaño poblacional para
# cada iteración. Esto se calcula:
# 2 * ds / vp
# ds: desvío standard de toda la serie (empezando desde init)
# vp: varianza promedio de toda la serie (empezando desde init)
  x <- x[init:length(x)]
  v <- numeric(length(x) - 1)
  for (i in 2:length(x))
    v[i - 1] <- var(x[1:i])
  s <- sqrt(v)
  m <- cumsum(v) / (1:length(v))
  o <- 2 * s / m
  return(o)
}
####################################
####################################
getStats <- function(x, show=TRUE) {
# x:
#  1. Objeto ibm
#  2. Objeto stats
#  3. Vector con nombres, indicando los valores de los distintos parámetros,
#     por ejemplo 'x=c(M=12)'
# salida:
#  Debuelve las estadísticas de los individuos para los parámetros incluidos
#  en x; los parámetros no especificados son tomados de los valores por
#  defectos de los argumentos de la función ibm.
  if ('ibm' %in% class(x))
    iStats <- x$indStats
  if ('stats' %in% class(x))
    iStats <- x
  if (is.vector(x)) {
    parms <- formals(ibm)
    parms <- lapply(parms, eval)
    if (!(names(x) %in% names(parms)))
      stop('Los nombres del vector x deben coincidir con los argumentos de ibm')
    parms[names(x)] <- x[names(x)]
    parms <- within(parms,
      B_c <- 2 * m_c * bmr0 / M ^ (1 / 4))
    iStats <- with(parms, stats())
  }
  if (show)
    print(iStats)
  invisible(iStats)
}
####################################
####################################
gompertz <- function(x, p.max, gompB, gompC) {
# ej: curve(gompertz(x, 1, -5.5, -1.5), from=-1, to=8, ylim=c(0,1)); ejes()
# En modo Auto:
#   gompB <- log(p.0)
#   gompC <- log(log(p1) / log(p0)) / psi[i]
  return(p.max * exp(gompB * exp(gompC * x)))
}
####################################
####################################
# 2. REGENERACIÓN DE PASTO
grassGrow <- function() {
  with(parent.frame(), {
    switch(grassMode,
      fixed={
        # Los parches se recuperan por completo al finalizar el turno.
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
        pasto[pasto < tol] <- yield * 0.02
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
####################################
####################################
importer <- function(im, tfinal) {
# im= ibm importado
  if (im$extinction)
    stop("The population is already extinguished!!")
  out <- NULL
  pr  <- im$parms[!(names(im$parms) %in% c('import', 'lands', 'tfinal'))]
  out <- c(pr, im[names(im) != 'call'])
  out$tfinal  <- tfinal
  out$extra_t <- im$tiempo
  out <- within(out, {
    # Objetos fijos:
    fixedObjects()
    t_      <- length(pop)
    tplus   <- tfinal
    tfinal  <- tplus + t_
    lands   <- lands
    N       <- pop[t_]

    if (saveRecord) {
      rec        <- record[[t_]]
      foodAcum   <- rec$foodAcum
      lifeSpan   <- rec$lifeSpan
      nombres    <- rec$name
      reser      <- rec$reser
    }
    lastname   <- nombres[N]

    pop <- c(pop, numeric(tplus))
    births <- c(births, numeric(tplus))
    deaths <- c(deaths, numeric(tplus))
    if (saveRecord) {
      pastoAll <- c(pastoAll, vector('list', tplus))
      pasto <- pastoAll[[t_]]
      ijMigra <- c(ijMigra, numeric(tplus))
      popMigra <- c(popMigra, numeric(tplus))
      totalMigra <- c(totalMigra, numeric(tplus))
      totalEmigra <- c(totalEmigra, numeric(tplus))
      totalInmigra <- c(totalInmigra, numeric(tplus))
      record <- c(record, vector('list', tplus))
      migra <- c(migra, vector('list', tplus))
      migra_t <- migra[[t_]]
      emigra <- c(emigra, vector('list', tplus))
      emigra_t <- emigra[[t_]]
      inmigra <- c(inmigra, vector('list', tplus))
      inmigra_t <- inmigra[[t_]]
      vecinos <- c(vecinos, vector('list', tplus))
      vec.t <- vecinos[[t_]]
    }
    t_ <- t_ + 1
  })
  return(out)
}
####################################
####################################
# 0.2 Individuos iniciales
indivSeed <- function() {
  with(parent.frame(), {
    if (levelSeeds < 0 || tolower(levelSeeds) == 'random') {
    # Repartija aleatoria de individuos en los parches nivel 0
      N       <- N_0
      nombres <- 1:N
      hasrep  <- numeric(N) > 100 # has reproduced previously?
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
      hasrep  <- numeric(N) > 100
      if (levelSeeds %in% c(0, 'all')) {
      # Un individuo por parche de nivel 0
        pos <- 1:N
      } else {
      # Un individuo por parche de nivel 'levelSeeds'
        pos <- numeric(npatch)
        for (i in 1:npatch) {
          cuales <- which(lands$belong[,levelSeeds + 1] == i)
          pos[i] <- lands$belong[sample(cuales, 1), 1]
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
      hasrep  <- numeric(0) > 100
      posMore <- sample(nrow(xypasto), more, replace=TRUE)
      xypos   <- rbind(xypos, xypasto[posMore,])
    }
  
    lastname <- N
    foodAcum <- numeric(N)
    m <- switch(sizeMode,
                  adults = rep.int(M, N),
                  infants = rep.int(m0, N),
                  random = runif(N, m0, M),
                  ratio = sample(c(m0, M), N, replace=TRUE,
                                 prob=c(1, sizeRatio)))
    reser    <- trs0 * m ^ trsExp
    babyBiom <- numeric(N)
    optPatch <- numeric(N) * NA
  })
}
####################################
####################################
logistica <- function(x, logitA0, logitA1) {
# ~> curve(logit(x, -2.5, .06), from=-10, 10)
# En modo Auto:
#   logitA0 <- log(p.0 / (1 - p.1))
#   logitA1 <- log((p.1 * (1 - p.0)) / (p.0 * (1 - p.1))) / psi[i]
  Y <- (logitA0 + x * logitA1)
  return(p.max * exp(Y) / (1 + exp(Y)))
}
####################################
####################################
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
####################################
####################################
migrator <- function() {
  with(parent.frame(), {
    for (i in 1:npatchFocus) {
      vec <- inpip(xypos, lands$areas[[levelFocus + 1]][[i]], bound=TRUE)
      vec.t[[i]] <- nombres[vec]
      emigra_t[[i]]  <- c(emigra_t[[i]], setdiff(vecinos[[t_ - 1]][[i]],
                                                 vec.t[[i]]))
      inmigra_t[[i]] <- setdiff(vec.t[[i]],
                                vecinos[[t_ - 1]][[i]])
    }

    for (i in 1:npatchFocus) {
        for (j in (1:npatchFocus)[-i]) {
        common <- intersect(emigra_t[[j]],  # parche de origen
                            vec.t[[i]]) # parche de destino
        migra_t[i,j]  <- length(common)
        #-->migra[i,j] = migración de j --> i
        emigra_t[[j]] <- setdiff(emigra_t[[j]], common)
      }
    }
    vecinos[[t_]] <- vec.t
    migra[[t_]]   <- migra_t
    emigra[[t_]]  <- emigra_t
    inmigra[[t_]] <- inmigra_t
  })
}
####################################
####################################
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
    patchAreas[[i]] <- ellipse(0, centre=c(coordsAll[i,1], coordsAll[i,2]), t=dist_ * .5, npoints=15)
  }
  areas[[1]] <- patchAreas
  
  coordsAll <- as.data.frame(coordsAll)
  names(coordsAll) <- c('x', 'y')

  frm <- formals()
  parms <- lapply(names(frm), get, envir=sys.parent(0))
  names(parms) <- names(frm)
  out <- list(areas=areas, belong=belong, coordsAll=coordsAll,
              parms=parms, pos=pos)
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

####################################
####################################
mod <- function(x, y) { out <- x %% y; out[out == 0] <- y; return(out)}
####################################
####################################
msd <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(sd(total))
}
####################################
####################################
mvar <- function(m_) {
  low   <- m_[upper.tri(m_)]
  up    <- m_[lower.tri(m_)]
  total <- c(low, up)
  return(var(total))
}
####################################
####################################
# plotFoto <- function(noiseFactor=1/7, areaFactor=1.3, ) {
## No está lista aún...
#   with(parent.frame(), {
#     parop  <- par(mfrow=mfrow, mar=c(4,5.5,1.5,1.5))
#     lims   <- calcLims(pos, areaFactor)
#     x.lims <- lims[[1]]
#     y.lims <- lims[[2]]
#     dmin   <- ifelse(length(pos) > 1, min(diff(pos)), 1)
#     plot(lands, yield=yield, pasto=pasto[[t_]], ylim=y.lims, xlim=x.lims,
# 	 scex.lab=1.7, cex.axis=1.7)
#     rec <- record[[t_]]
#     noise1 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
#     noise2 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
#     tamano <- (rec$m / M + 1) * resFactor
#     points(rec$x + noise1, rec$y + noise2,
#            pch=19, cex=tamano, col=rainbow(maxname)[rec$name])
#     points(rec$x + noise1, rec$y + noise2,
#            pch=21, cex=tamano, col=1)
#       text(rec$x + noise1, rec$y + noise2, labels=rec$name,
# 	   cex=tamano * .2, col='white')
#     legend('topleft', legend=x$pop[t_], cex=1.5, bty='n')
#     
#     plot(1:t_, plotGrass[1:t_], type='o', pch=19, lwd=1.25, col='#409951',
#          xlim=c(1, t_), ylim=c(0, max(x$pop) * uplim), ylab='Población (N)',
#          xlab='Iteración', cex.lab=1.7, cex.axis=1.7)
#     points(1:t_, x$pop[1:t_], lwd=3, col=col1, type='o', pch=20)
#     par(parop)
#   })
# }
####################################
####################################
plot.ibm <- function(x, kind='pop', outdir='default', nmax=500,
            type='l', col1=1, col2=8, uplim=1, areaFactor=1.3, t_=1,
            resFactor=2, noiseFactor=1/7, from, to, mfrow=c(1, 2),
            ..., lang='en', follow=NULL) {
  ## NOTA!!: DEFINITIVAMENTE ABANDONADO EL USO DEL PAQUETE "animation"
  if (!x$parms$saveRecord && kind != 'pop')
    stop('No se puede hacer el "plot" sin la opción saveRecord. Use kind="pop"')

  M         <- x$parms$M
  lands     <- x$lands
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
  if (fin > tfinal)
    stop('Argument "to" is too great!')
  
  lims <- calcLims(pos, areaFactor)
  x.lims <- lims[[1]]
  y.lims <- lims[[2]]

  if (x$parms$saveRecord) {
    seriePasto <- sapply(pasto, sum)
    plotGrass  <- max(x$pop) * seriePasto / (x$parms$yield * nrow(x$lands$coordsAll))
  }

  if (outdir == 'default') {
    out <- 'animation'
  } else {
    out <- outdir
    system(paste('mkdir', out))
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
      parop <- par(mfrow=c(1, 2), mar=c(4,5.5,1.5,1.5))
      plot(lands, yield=yield, pasto=pasto[[t_]], ylim=y.lims, xlim=x.lims,
           cex.lab=1.7, cex.axis=1.7)
      rec <- record[[t_]]
      noise1 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      noise2 <- rnorm(nrow(rec), 0, dmin * noiseFactor)
      tamano <- (rec$m / M + 1) * resFactor
      points(rec$x + noise1, rec$y + noise2,
             pch=19, cex=tamano, col=rainbow(maxname)[rec$name])
      points(rec$x + noise1, rec$y + noise2,
             pch=21, cex=tamano, col=1)
      if (!is.null(follow)) {
        frec <- subset(rec, name %in% follow)
        text(frec$x, frec$y, labels=frec$name)
      }
#       text(rec$x + noise1, rec$y + noise2, labels=rec$name,
# 	   cex=tamano * .2, col='white')
#     text(x, y = NULL, labels = seq_along(x), adj = NULL,
#          pos = NULL, offset = 0.5, vfont = NULL,
#          cex = 1, col = NULL, font = NULL, ...)
      legend('topleft', legend=x$pop[t_], cex=1.5, bty='n')
      
      plot(1:t_, plotGrass[1:t_], type='o', pch=19, lwd=1.25, col='#409951',
           xlim=c(1, t_), ylim=c(0, max(x$pop) * uplim), ylab='Población (N)',
	   xlab='Iteración', cex.lab=1.7, cex.axis=1.7)
      points(1:t_, x$pop[1:t_], lwd=3, col=col1, type='o', pch=20)
      par(parop)
  }
  if (kind %in% c('gvis', 4)) {
    Motion <- plotGvisMotionChart(x, from=ini, to=fin, show=FALSE)
    Line   <- plotGvisLineChart(x, from=ini, to=fin,
                                dates=TRUE, show=FALSE)
    Merged <- gvisMerge(Motion$gvis, Line$gvis, horizontal=TRUE)
    out    <- list(gvis=Merged,
                   tablaMotion=Motion$tabla,
                   tablaLine=Line$tabla)
    class(out) <- 'ibmGvis'
    plot(out)
    invisible(out)
  }
}
####################################
####################################
plot.lands <- function(x, yield=1, pasto=rep(yield, nrow(x$coordsAll)),
                       cex.grass=3, pch.grass=19, ...) {
  with(x, {
    cex.grass <- cex.grass * (pasto / yield)
    plot(coordsAll, xlab='x', ylab='y',
         cex=cex.grass, pch=pch.grass,
         col='#BAFFC7', ...)
    rm(cex.grass)
  })
}
####################################
####################################
plotGvisLineChart <- function(x, from=1, to=length(x$pop), dates=FALSE,
                              op=list(width=800), show=TRUE) {
  tabla <- with(x,
                data.frame(Population=pop,
                           Migration=totalMigra,
                           Deaths=deaths,
                           Births=births,
                           Iteracion=1:length(pop)))
  if (dates)
    tabla$Iteracion <- as.Date(tabla$Iteracion, origin='0-1-1')
  gvisObj <- gvisLineChart(tabla[from:to, ],
                           xvar="Iteracion",
                           yvar=names(tabla)[-5],
                           options=op)
  if (show)
    plot(gvisObj)
  out <- list(gvis=gvisObj, tabla=tabla)
  class(out) <- 'ibmGvis'
  invisible(out)
}
####################################
####################################
plotGvisMotionChart <- function(x, from=1, to=length(x$pop),
                                op=list(showChartButtons=TRUE),
                                show=TRUE) {
# x: objeto ibm
  require(googleVis)
  rec    <- x$record[from:to]
  total  <- sapply(rec, nrow)
  tiempo <- rep.int(1:length(total), total)
  fecha  <- as.Date(tiempo, origin='0/1/1')
  tabla  <- data.frame(time=fecha, foodAcum=0, name=0, m=0, reser=0, x=0, y=0)
  for(i in 1:length(total)) {
    cols <- names(rec[[i]]) != 'lifeSpan'
    tabla[tiempo == i, -1] <- rec[[i]][cols]
  }
#   stateSettings <- '{"nonSelectedAlpha":0.4,"playDuration":15000,"duration":{"multiplier":1,"timeUnit":"D"},"xZoomedIn":false,"xAxisOption":"5","uniColorForNonSelected":false,"xZoomedDataMin":0,"iconKeySettings":[],"colorOption":"_UNIQUE_COLOR","yZoomedIn":false,"time":"1900-01-02","showTrails":true,"orderedByX":false,"xZoomedDataMax":12,"yZoomedDataMin":0,"iconType":"BUBBLE","xLambda":1,"dimensions":{"iconDimensions":["dim0"]},"orderedByY":false,"yLambda":1,"yZoomedDataMax":11.8780815,"sizeOption":"3","yAxisOption":"6"}'
  # Note: this are the settings I would choose for a default view, but it
  # doesn't seem to work properly... hope it gets right in next versions. For this reason the column order is changed:

  tabla <- tabla[,c('name', 'time', 'x', 'y', 'reser', 'm', 'foodAcum')]

  gvisObj <- gvisMotionChart(tabla, timevar="time", idvar="name",
                             options=op)
  if (show)
    plot(gvisObj)
  out <- list(gvis=gvisObj, tabla=tabla)

  class(out) <- 'ibmGvis'
  invisible(out)
}
####################################
####################################
plot.ibmGvis <- function(x, ...)
  plot(x$gvis, ...)
####################################
####################################
powerPts <- function(x, ptExp) {
  return((x - min(x) + 1) ^ ptExp)
}
####################################
####################################
print.ibm <- function(x, stats=TRUE) {
  require(splancs)
  require(ellipse)
  if (stats) {
    print(x$indStats)
    print(x$lands)
  }

  if (stats)
    cat('\nYIELD =', round(x$parms$yield, 2), '\n')

  tiempo   <- length(x$pop)
  halfTime <- round(tiempo / 2)
  pop1     <- mean(x$pop[1:(halfTime - 1)])
  pop2     <- mean(x$pop[halfTime:tiempo])
  if (x$parms$saveRecord) {
    mig1     <- mean(x$totalMigra[1:(halfTime - 1)])
    mig2     <- mean(x$totalMigra[halfTime:tiempo])
  }
  LF <- x$parms$levelFocus

  lands <- x$lands
  totalPatches0 <- with(lands, length(pos) ^ parms$dim_)
  xypos <- x$xypos
  count <- 0
  if (length(x$xypos) > 0) {
    for (i in 1:totalPatches0) {
      vec   <- inpip(xypos, lands$areas$l0[[i]], bound=TRUE)
      count <- count + (length(vec) > 0)
    }
  }
  patch0    <- count / totalPatches0
  nPatches  <- length(x$vecinos[[tiempo]])
  nVecinos  <- sapply(x$vecinos[[tiempo]], countPositives)
  nOccupied <- sum(nVecinos > 0)
  level <- x
  cat(
    '\nTiempo total de simulación:\t', tiempo, ' iteraciones\n',
    '\nPoblación promedio, 1er. mitad:\t', round(pop1,2),
    '\nPoblación promedio, 2da. mitad:\t', round(pop2,2), '\n')
  if (x$parms$saveRecord) {
    cat(
      'Migrantes promedio, 1er. mitad:\t ', round(mig1,3), '\n',
      'Migrantes promedio, 2da. mitad:\t ', round(mig2,3), '\n',
      '\nCantidad de parches 0 ocupados (%):\t',
      paste(count, totalPatches0, sep='/'),
        ' (', round(100 * patch0, 2), '%)',
      '\nCantidad de parches ', LF,' ocupados (%):\t',
      paste(nOccupied, nPatches, sep='/'),
        ' (', round(100 * nOccupied / nPatches, 2), '%)',
      sep='')
  }

  grass    <- x$pasto[tiempo]
  yield    <- x$parms$yield
  perYield <- 100 * (1 - mean(grass) / yield)
  
  cat(
    '\nRecursos promedio por parche (nivel 0):\t', round(mean(grass), 1),
    '\nProm. de recursos consumidos:\t\t', round(perYield, 1), '%\n',
    '\nTIEMPO TOTAL DE SIMULACIÓN:\t', round(x$tiempo, 1),
    's.\n\n',
    sep='')

  if (x$extinction)
    cat('>>> The population collapsed!\n')
}
####################################
####################################
print.stats <- function(x) {
# x es un objeto de la clase stats
    with(x, {
    cat('Atributos individuales:\n\tMMD = ', round(MMD, 3),
      ' Km/day\tICL = ', round(ICL, 4), ' KJ/Km\tMMC = ', round(MMC, 4),
      ' KJ/day\n\tBMR = ', round(BMR,3), ' KJ/day\tMEI = ', round(MEI, 3),
      ' KJ/day\tPSI = ', round(PSI, 4), ' Kg/day\n\tM0  = ', round(M0, 2),
      ' Kg\t\tMPD = ', round(MPD, 3), ' Km', '\t\tTRS = ', round(TRS, 4),
      ' Kg\n\tTMC = ', round(TMC, 3), ' Kg/day', '\tREE = ', round(REE, 3),
      ' KJ\tALS = ', round(ALS), ' days\n',
      sep='')
    cat('\n\tTAMAÑO: ', round(M, 3), 'Kg\n')
    })
}
####################################
####################################
print.lands <- function(x) {

  with(x$parms, {
    npatch <- (n_ ^ dim_) ^ lmax_
    cat('\nPropiedades del paisaje (', type, '):\n',
    '\tDIM   = ', dim_, '\tDIST    = ', round(dist_, 2), ' Km\n',
    '\tRDIST = ', round(rdist_, 2), '\tNIVELES = ', lmax_, '\n',
    '\tNo. PARCHES (U. BÁSICA) = ', n_, '\n',
    '\tNo. PARCHES (TOTAL)     = ', npatch, '\n',
    sep='')
  })
}
####################################
####################################
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
####################################
####################################
# 0.3 Registros:
register <- function() {
  with(parent.frame(), {
    #  0.3.a Variables
    # tol: variable interna, para corregir errores de presición
    tol <- 1e-6
    # extinciton: variable indicadora de extinción:
    extinction <- FALSE

    #  0.3.b Vectores
    pasto         <- rep.int(yield, nrow(xypasto))
    if (saveRecord) {
      pastoAll      <- vector('list', length=tfinal)
      pastoAll[[1]] <- pasto
    }
    pop           <- numeric(tfinal)
    births        <- numeric(tfinal)
    deaths        <- numeric(tfinal)
    ALS           <- als0 * M ^ alsExp
#     lifeSpan      <- rpois(N, ALS)
    lifeSpan      <- round(runif(N, ALS * 0.05, ALS * .95))
    extra_t       <- 0
    t_ <- 2; pop[1] <- N
  
    if (saveRecord) {
      #  0.3.c Tabla
      record      <- vector('list', tfinal)
      record[[1]] <- data.frame(foodAcum=foodAcum,
                  name=nombres, m=m, #pos=pos,
                  reser=reser, lifeSpan=lifeSpan,
                  x=xypos[,1],
                  y=xypos[,2])
    
      # 0.3.d Migración
      migra   <- vector('list', tfinal)
      migra_t <- matrix(0, npatchFocus, npatchFocus) -> migra[[1]]
      #-->migra_t[i,j] = migración de j --> i
      emigra    <- vector('list', tfinal)
      emigra_t  <- vector('list', npatchFocus)
      inmigra   <- vector('list', tfinal)
      inmigra_t <- vector('list', npatchFocus)
      vecinos   <- vector('list', tfinal)
      vec.t <- vector('list', npatchFocus)
      for (v in 1:npatchFocus) {
        emigra_t[[v]]  <- 0
        inmigra_t[[v]] <- 0
        vec.t[[v]] <- nombres[inpip(xypos,
                                        lands$areas[[levelFocus + 1]][[v]],
                                        bound=TRUE)]
      }
      vecinos[[1]] <- vec.t
    }
  })
}
####################################
####################################
sdaFun <- function(x, sdaMax=sdaMax, mei=mei[i]) {
  return(x * sdaMax / mei)
}
####################################
####################################
seeStats <- function(x)
  print(x$indStats)
####################################
####################################
seeTime <- function(t_, pop)
    cat('| ', t_, ' N=', pop[t_], ' ', sep='')
####################################
####################################
stats <- function() {
  require(ellipse, quietly=TRUE)
  out <- with(parent.frame(), {
    ALS <- als0 * M ^ alsExp
    MMD <- mmd0 * M ^ mmdExp
    ICL <- icl0 * M ^ iclExp
    MMC <- MMD * ICL
    M0  <- pm * M
    MPD <- mpd0 * M ^ mpdExp
    BMR <- bmr0 * M ^ bmrExp
    MEI <- mei0 * BMR
    TMC <- B_c * M / m_c
    TRS <- trs0 * M ^ trsExp
    REE <- TRS * E_cr / m_c
    PSI <- TRS + (npsi * MEI - TMC) * m_c / E_cr
    if (ptsMode == 'PBB')
      PSI <- PSI - TRS
  
    list(ALS=ALS, MMD=MMD, ICL=ICL, MMC=MMC, M=M,  M0=M0, MEI=MEI,
         MPD=MPD, PSI=PSI, BMR=BMR, REE=REE, TMC=TMC, TRS=TRS)
  })
  class(out) <- 'stats'
  return(out)
}
####################################
####################################
countPositives <- function(x)
  sum(x > 0)
####################################
####################################
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
####################################
####################################
