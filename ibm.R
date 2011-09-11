# v1.4

ibm <- function(addGuys=FALSE,
                # agrega pibes además de los que pone por parche
                chMode='sampleAll', # max | sampleAll | quant
                critQuant=.85,
                mmd0=3 * 1.04,
                # DMD = Daily Movement Distance (Garland 1983)
                # = 1.038 * M ^ 0.25 Km/day <- supongo que es un promedio
                mmdExp=1/4,
                E_c=2.1e-8, # KJ
                E_cr=21e-8, # KJ
                m_c=3e-12, # Kg
                gompB=-5.5,
                gompC=-1.5,
                grassMode='fixed',
                # fixed | semiFixed | bholt | randSprout (?)
                grassProb=0.5,
                icl0=10.68, # KJ/km
                iclExp=0.75, # 0.70
                import=NULL,
                lands=NULL,
                # Argumentos de landscape: sólo tienen efecto si
                # lands = NULL:
                # (inicio)
                landsDim_=2,
                landsDist_=1,
                landsLmax_=2,
                landsN_=3,
                landsRdist_=3,
                landsType='fractal', # fractal | regular | randUnif
                # (fin)
                levelSeeds=1,
                # integer -1 (random) | 0 | 1 | 2 ... (landsLmax_ - 1)
                levelFocus=1, # integer 0 | 1 | 2 ... (landsLmax_ - 1)
                logitA0=-2.5, # numeric
                logitA1=.06, # numeric
                M=0.1, # numeric, ..
                mei0=8,  # numeric, ..
                mpd0=18, # numeric, ..
                mpdExp=1/2, # numeric,
                N_0=9, # integer, No. de individuos iniciales | 'K'
                npsi=2, # numeric
                p_max=10, # numeric
                p_0=0.01, # probability
                p_1=0.9, # probability
                pastoRo=2, # numeric, growth rate
                pm=0.05, # numeric, body mass fraction
                plotPop=TRUE, # logical, plot population dynamics?
                ptExp=3,
                ptsFun='gompAuto1',
                # logitAuto | logitManual | gompAuto1 | gompAuto2 |
                # gompManual | potencia
                ptsMode='PBB', # PBB | cualquier cosa
                random=TRUE,
                bmr0=293, # KJ/day ver detalles en doc.ibm.R
                bmrExp=(3/4),
                ruc=0.1, # Costo asociado a usar las reservas
                showTime=TRUE,
                showStats=TRUE,
                showSummary=TRUE,
                tfinal=100,
                trs0=0.3,
                trsExp=1,
                verboso=TRUE,
                yield=100) {
  # 0. INICIALIZACIONES
  # Comienza el conteo de tiempo:
  ptm <- proc.time()[3]
  # tol: variable interna, para corregir errores de presición
  tol <- 1e-6
  # extinciton: variable indicadora de extinción:
  extinction <- FALSE
  # paquetes necesarios:
  require(splancs, quietly=TRUE)
  require(ellipse, quietly=TRUE)

  # 0.0 Salida de info:
  if (!verboso) {
    showSummary <- showTime <- showStats <- plotPop <- FALSE
  } 
  if (verboso) cat('\n-- COMIENZA LA SIMULACIÓN --\n\n')
  # Estadísticas (de adultos)(sólo si showStats==TRUE):

  # Coordenadas de los parches
  if (is.null(lands) || missing(lands)) {
    lands <- mklands(dim_=landsDim_,
                     dist_=landsDist_,
                     lmax_=landsLmax_,
                     n_=landsN_,
                     rdist_=landsRdist_,
                     type=landsType)
  }
  xypasto <- as.matrix(lands$coordsAll)

  # 0.1 Variables fijas
{
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

  indStats <- stats()
  if (showStats && is.null(import)) {
    print(indStats)
    print(lands)
    cat(paste('\nYIELD = ', round(yield, 2), '\n', sep=''))
  }
}
  # 0.2 Individuos iniciales
{
  if (levelFocus >= landsLmax_) levelFocus <- landsLmax_ - 1
  #-->Si no se hace esta corrección va a dar error
  if (levelSeeds < 0) {
    N       <- N_0
    nombres <- 1:N
    pos     <- sample(nrow(xypasto), N, replace=TRUE)
    xypos   <- xypasto[pos,]
    if (length(pos) == 1) {
      xypos <- matrix(xypos, ncol=2)
    }
  } else {
    npatch  <- length(lands$areas[[levelSeeds + 1]])
    N       <- npatch
    nombres <- 1:N
    if (levelSeeds == 0) {
      pos <- 1:N
    } else {
      pos <- numeric(npatch)
      for (i in 1:npatch) {
        cuales <- which(lands$belong[,levelSeeds + 1] == i)
        pos[i] <- lands$belong[sample(cuales, 1),1]
      }      
    }
    xypos <- xypasto[pos,]
    if (length(pos) == 1) {
      xypos <- matrix(xypos, ncol=2)
    }
  }

  if (addGuys) {
    more    <- N_0 - N
    N       <- N_0
    nombres <- 1:N
    pos2    <- sample(nrow(xypasto), more, replace=TRUE)
    xypos   <- rbind(xypos, xypasto[pos2,])
  }

  lastname <- N
  foodAcum <- numeric(N)
  m        <- rep.int(M, N)
#~   m        <- rep.int(m0, N)
#~   m        <- runif (N, m0, M)
  reser    <- trs0 * m ^ trsExp
  babyBiom <- numeric(N)
  optPatch <- numeric(N) * NA
}
  # 0.3 Registros:
{
  #  0.3.a Vectores
  pasto         <- rep.int(yield, nrow(xypasto))
  pastoAll      <- vector('list', length=tfinal)
  pastoAll[[1]] <- pasto
  pop           <- numeric(tfinal)
# ~  popAll        <- numeric(tfinal)
  births      <- numeric(tfinal)
  deaths      <- numeric(tfinal)
  extra_t       <- 0
# ~  t_ <- 2; popAll[1] <- pop[1] <- 1;
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
}
  # 0.4 Crear función de asignación de puntajes:
  pointsFun <- makePointsFun(ptsFun)

  # Protocolo de importación de simulaciones anteriores
  im <- NULL
  if (!is.null(import)) {
      im <- import
      extra_t <- im$tiempo
      tf <- tfinal
      pr <- im$parms[names(im$parms) != 'import']
      for (i in 1:length(pr)) {
        assign(names(pr)[[i]], pr[[i]])
      }
      t_ <- length(im$pop)
      tfinal  <- tf + t_
      lands   <- im$lands
      N       <- im$pop[t_]
      m0      <- pm * M
      trsMax  <- trs0 * M ^ trsExp
      trsMin  <- trs0 * m0 ^ trsExp
      minBio  <- m0 + (trsMin * E_cr / E_c)
      B_c     <- 2 * m_c * bmr0 / M ^ (1 / 4)
      cfaRand <- ellipse(0, centre=c(0, 0), t=1, npoints=60)
      if (showStats) {
        print(im$indStats)
        print(im$lands)
        cat(paste('\nYIELD = ', round(yield, 2), '\n', sep=''))
      }

      babyBiom  <- im$babyBiom
      optPatch  <- im$optPatch
      foodAcum  <- im$record[[t_]]$foodAcum
      nombres   <- im$record[[t_]]$name
      reser     <- im$record[[t_]]$reser
      lastname  <- nombres[N]
      m         <- im$record[[t_]]$m; pop[1] <- N
      xypos     <- as.matrix(im$record[[t_]][,c('x','y')])
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
  }

  # COMIENZAN LAS ITERACIONES
  # Seguimiento de las iteraciones
  if (showTime) {
    cat(paste('\n Tiempo (de ', tfinal, '):\n>>',
           t_ - 1, ' N=', pop[t_ - 1], ' ', sep=''))
  }
  
  while (pop[t_ - 1] > 0 && t_ <= tfinal) {
    # Alometrías et al.:
    icl <- icl0 * m ^ iclExp
    mmd <- mmd0 * m ^ mmdExp -> restoMov
    mmc <- mmd * icl
    mpd <- mpd0 * m ^ mpdExp
    bmr <- bmr0 * m ^ bmrExp
    mei <- mei0 * bmr
    tmc <- B_c * m / E_cr
    #-->tmc = "total maitenance cost"
    trs <- trs0 * m ^ trsExp
    if (ptsMode == 'PBB') {
      psi <- npsi * mei * m_c / E_cr - tmc
    } else {
      psi <- trs + npsi * mei * m_c / E_cr - tmc
    }
#~     RCB <- - (tmc + mmc * m_c / E_cr) # mmc = maximum movement cost
#~     #-->RCB = "Random Choice Balance"
#~     NMB <- - tmc
#~     #-->"No Movement Balance"

    obtEner   <- numeric(N)
    
    if (random) {
      # Para que el orden en q actúan sea aleatorio:
      index <- sample(N)
    } else {
      # Con esta opción se mantiene el orden 4ever
      index <- 1:N
    }

    for (i in index) {

      # 1. MOVIMIENTO Y ALIMENTACIÓN
      goRand <- FALSE
      stay   <- FALSE
      condA  <- sum(pasto) > tol
      condB  <- TRUE
      cfa    <- ellipse(0, centre=c(0,0), t=mpd[i] + 1e-2)

      while (condA && condB) {
        xy <- xypos[i,]
        if (is.na(optPatch[i])) {
          # 1.1 Elección del próximo parche
          cfa_i    <- cbind(cfa[,1] + xy[1], cfa[,2] + xy[2])
          visibles <- inpip(xypasto, cfa_i, bound=TRUE)

          if (length(visibles) >= 1) {
            #### Elección de parche (inicio)
            # Distancia y cantidad de recursos:
            dist2visib <- distancias(xy, xypasto[visibles,])
            ratioDist  <- dist2visib / mmd[i]
            ratioRsrc  <- pasto[visibles] / mei[i]
            #-->(Rsrc = resource)
            flrsrc <- floor(ratioRsrc)
    
            # Costos:
            costPerDist <- ratioDist * mmc[i] * m_c / E_cr
            costPerMant <- pmax(ratioDist, ratioRsrc) * tmc[i]
  # ~          if (any(costPerMant == - Inf)) browser()
  
            # Ganancias:
            plus <- ratioRsrc * mei[i] * m_c / E_cr
    
            # Balance de Biomasa:
            PBB <- c(plus - (costPerDist + costPerMant),
              - restoMov[i] * icl[i], # Balance corresp. a mov. aleatorio.
              - tmc[i] * max(restoMov[i] / mmd[i],
              1 - foodAcum[i] / mei[i]))
              # Balance corresp. a quedarse en el lugar.
            #-->(PBB = Partial Biomass Balance)
    
            # Incluír el costo extra por usar reservas (si PBB < 0):
            PBB[PBB < 0] <- PBB[PBB < 0] / (1 - ruc)
  # ~          deltaBiom <- PBB / E_cr
  # ~          deltaBiom[PBB < 0] <- deltaBiom[PBB < 0] / (1 - ruc)
  
  # ~  browser()
    
            # Asignación de puntajes & elección de parche:
            input <- PBB
            if (ptsMode != 'PBB') {
              input <- input + trs[i]
            }
            puntaje <- pointsFun(input)
            puntaje[puntaje <= 0] <- tol
            switch(chMode,
              sampleAll={
                choiceNum <- sample(length(puntaje), 1, prob=puntaje)
              },
              max={
                choiceNum <- which(puntaje == max(puntaje))
                if (length(choiceNum) > 1) {
                  choiceNum <- sample(choiceNum, 1)
                }
              },
              quant={
                valor <- quantile(puntaje, critQuant)
                if (sum(puntaje >= valor) > 1) {
                  choiceNum <- sample((1:length(puntaje))[puntaje >= valor], 1)
                } else {
                  choiceNum <- which(puntaje >= valor)
                }
              })
            if (PBB[choiceNum] == 0) break
            #### Elección de parche (fin)
          } else {
            PBB <- c(
              - restoMov[i] * icl[i],
              - tmc[i])
            input <- PBB
            if (ptsMode != 'PBB') {
              input <- input + trs[i]
            }
            puntaje <- pointsFun(input)
            puntaje[puntaje <= 0] <- tol
            choiceNum <- sample(1:2, 1, prob=puntaje)
          }

          if (choiceNum == length(visibles) + 1) {
            goRand <- TRUE
          }
          if (choiceNum == length(visibles) + 2) {
            stay <- TRUE
          }
          if (!any(goRand, stay)) {
            optPatch[i] <- visibles[choiceNum]
            dist2patch  <- dist2visib[choiceNum]
            # Si veía un sólo parche y estaba sobre él, no más turno:
            if (length(visibles) == 1 && dist2patch < tol) {
              optPatch[i] <- NA
              break
            }
          }
        } else {
          dist2patch <- distancias(xy, xypasto[optPatch[i],])
        }

        # 1.2.a Movimiento al azar
        if (goRand) {
          xyRand      <- cfaRand[sample(60, 1), ]
          xypos[i,]   <- xy + restoMov[i] * xyRand
          restoMov[i] <- 0
          condB       <- FALSE

        }
        if (stay) {
          condB <- FALSE
        }
        if (!any(goRand, stay)) {

          # 1.2.b Movimiento hacia el parche
          # Si llega al parche
          if (restoMov[i] - dist2patch > tol) {
            # 1.3 Comer del parche
            food <- feed(foodAcum[i], mei[i], pasto[optPatch[i]])
        ## obtEner[i] <- obtEner[i] + food * (1 - (food * sdaMax / mei[i]))
            # Esta línea se comentó por un efecto inesperado:
            # Los individuos obtienen más energía si hacen varias comidas
            # de valor intermedio que si hacen una sóla comida con el
            # máximo posible de energía. Considerando que esto no parece
            # muy realista ni útil, se optó por que los individuos hacen el
            # procesamiento de la energía 1 vez por turno.
  
            foodAcum[i]        <- foodAcum[i] + food
            pasto[optPatch[i]] <- pasto[optPatch[i]] - food
            xypos[i,]          <- xypasto[optPatch[i],]
            restoMov[i]        <- restoMov[i] - dist2patch
            optPatch[i]        <- NA
    
          # Si no le da para llegar
          } else {
            dirVector     <- xypasto[optPatch[i],] - xy
            dirVectorNorm <- sqrt(sum(dirVector ^ 2))
            movVector     <- dirVector * restoMov[i] / dirVectorNorm
            xypos[i,]     <- xy + movVector
            restoMov[i]   <- 0
          }
        }
        condA <-
        restoMov[i] > tol &&
        (mei[i] - foodAcum[i]) > tol &&
        sum(pasto) > tol
      }
      obtEner[i] <- foodAcum[i]
    }

    deltaBiom <- (obtEner  - icl * (mmd - restoMov)) * m_c / E_cr - tmc
    deltaBiom[deltaBiom < 0] <- deltaBiom[deltaBiom < 0] / (1 - ruc)

    reser <- reser + deltaBiom
    extraBiom <- reser - trs
    reser[extraBiom > 0] <- trs[extraBiom > 0]
    extraBiom[extraBiom < 0] <- 0
    for (i in 1:npatch) {
      vec <- inpip(xypos, lands$areas[[levelFocus + 1]][[i]], bound=TRUE)
      vecinos_t[[i]] <- nombres[vec]
      emigra_t[[i]]  <- c(emigra_t[[i]], setdiff(vecinos[[t_ - 1]][[i]], vecinos_t[[i]]))
      inmigra_t[[i]] <- setdiff(vecinos_t[[i]], vecinos[[t_ - 1]][[i]])
    }

    for (i in 1:npatch) {
        for (j in (1:npatch)[-i]) {
        common <- intersect(emigra_t[[j]],  # parche de origen
                         vecinos_t[[i]]) # parche de destino
        migra_t[i,j] <- length(common)
        #-->migra[i,j] = migración de j --> i
        emigra_t[[j]] <- setdiff(emigra_t[[j]], common)
      }
    }
    vecinos[[t_]] <- vecinos_t
    migra[[t_]]   <- migra_t
    emigra[[t_]]  <- emigra_t
    inmigra[[t_]] <- inmigra_t

    # 2. REGENERACIÓN DE PASTO
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
      })

    # 3. SUPERVIVENCIA
    alive      <- (1:N)[!(reser < 0)]
    N          <- length(alive)
    deaths[t_] <- sum(reser < 0)

    if (N > 0) {
      oldFoodAcum <- foodAcum[alive]
      #-->Valores para poner en el objeto 'record'
      extraBiom <- extraBiom[alive]
      nombres   <- nombres[alive]
      reser     <- reser[alive]
      m         <- m[alive]
#~       pos       <- pos[alive]
      xypos     <- matrix(xypos[alive,], ncol=2)
      # 6. CRECIMIENTO Y REPRODUCCIÓN
      nuevos <- 0
      m <- m + (extraBiom) * E_cr / E_c
      # Se suma la biomasa extra en valores equivalentes a tejido normal
      tooBig <- (1:N)[m > M]
      if (any(tooBig)) {
        babyBiom[tooBig] <- babyBiom[tooBig] + (m[tooBig] - M)
#         extraBiom[tooBig] <- (m[tooBig] - M) * E_c / E_cr
#         Desechado: se convierte de vuelta a tejido de reserva
        m[tooBig] <- M
        crios     <- floor(babyBiom / minBio)
# ~        reprod <- (1:N)[tooBig][babyBiom[tooBig] >= minBio]
# ~        if (any(is.na(reprod))) browser()
# ~        if (any(reprod)) {
        if (any(crios > 0)) {
# ~          crios <- floor(babyBiom[reprod] / minBio)
          reprod <- (1:N)[crios > 0]
          crios  <- crios[reprod]
          babyBiom[reprod] <- babyBiom[reprod] - crios * minBio
          
          # Los nuevos individuos:
          nuevos       <- sum(crios)
          nombres      <- c(nombres, (lastname + 1):(lastname + nuevos))
          lastname     <- lastname + nuevos
          m            <- c(m, rep(m0, nuevos))
          reser        <- c(reser, rep(trsMin, nuevos))
          babyBiom     <- c(babyBiom, numeric(nuevos))
# ~          print(crios)
#~           newpos       <- c(pos, rep(pos, crios))
          newXypos     <- matrix(ncol=2, nrow=nuevos)
          newXypos[,1] <- rep(xypos[reprod, 1], crios)
          newXypos[,2] <- rep(xypos[reprod, 2], crios)
          xypos        <- rbind(xypos, newXypos)
        }
      }

      foodAcum <- numeric(N + nuevos)
      
    } else {
      oldFoodAcum <- NULL
      #-->Valores para poner en el objeto 'record'
      nombres     <- NULL
      m           <- NULL
#~       pos         <- NULL
      xypos       <- NULL
      extinction  <- TRUE
      nuevos      <- 0
    }
    births[t_] <- nuevos
    N          <- N + nuevos

    # 7. ACTUALIZACIÓN DE LOS REGISTROS (inicio)
    if (N > 0)
      record[[t_]] <- data.frame(foodAcum=c(oldFoodAcum, numeric(nuevos)),
                name=nombres, m=m,
                reser=reser, #pos=pos,
                x=xypos[,1], y=xypos[,2])
    pastoAll[[t_]] <- pasto

# ~    popAll[t_] <- N
# ~    pop[t_]    <- sum(record[[t_]]$reser > (1.5 * minres))
    pop[t_] <- N
    # ACTUALIZACIÓN DE LOS REGISTROS (fin)

    if (showTime) {
      cat(paste('| ', t_, ' N=', pop[t_], ' ', sep=''))
    }
    t_ <- t_ + 1
  }

  if (showTime) cat('\n FIN\n')

  # 8. PREPARACIÓN DE LA SALIDA (inicio)
  t_       <- t_ - 1
  pop      <- pop[1:t_]
  record   <- record[1:t_]
  pastoAll <- pastoAll[1:t_]
  migra    <- migra[1:t_]
  vecinos  <- vecinos[1:t_]
  ibmpar   <- formals(ibm)
  parms    <- lapply(ibmpar, eval, envir=ibmpar)
  llama    <- match.call()
  if (!is.null(import)) {
    llama <- im$call
    llama['tfinal'] <- tfinal

###### ACAAAAAAAAA

  }
  cl <- as.list(llama)
  if (length(cl) > 1) {
    for (k in 2:length(cl)) {
      parms[names(cl)[k]] <- eval(cl[[k]])
    }
  }
  totalMigra <- sapply(migra, sum)
  ijMigra <- totalMigra / (npatch * (npatch - 1))
  #-->ijMigra: cantidad de migrantes per i ---> j (pares de parches)
  popMigra <- totalMigra / pop
  sumMig <- function(mig) sum(unlist(mig))
  totalEmigra <- sapply(emigra, sumMig)
  totalInmigra <- sapply(inmigra, sumMig)


  
  out <- list(babyBiom=babyBiom,
      births=births,
      deaths=deaths,
      call=llama,
      extinction=extinction,
      ijMigra=ijMigra,
      indStats=indStats,
      lands=lands,
      migra=migra,
      optPatch=optPatch,
      pastoAll=pastoAll,
      parms=parms,
      pointsFun=pointsFun,
      pop=pop,
      popMigra=popMigra,
      record=record,
      tiempo=proc.time()[3] - ptm + extra_t,
      totalEmigra=totalEmigra,
      totalInmigra=totalInmigra,
      totalMigra=totalMigra,
      vecinos=vecinos)
  class(out) <- c('ibm', class(out))
  # PREPARACIÓN DE LA SALIDA  (fin)

  if (showSummary)  print(out, stats=FALSE)

  if (plotPop) {
    plot(out, 'pop')
  }
  
  return(out)
}

source('aux.ibm.R')
