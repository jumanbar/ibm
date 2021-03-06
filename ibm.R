
ibm <- function(
      addGuys=TRUE,
      # agrega pibes además de los que pone por parche
      als0=1000,
      alsExp=1/4,
      chFun=chooseFromAll, # chooseMax | chooseFromAll | chooseQuant
      chFun.quant=.85,
      mmd0=3 * 1.04,
      # DMD = Daily Movement Distance (Garland 1983)
      # = 1.038 * M ^ 0.25 Km/day <- supongo que es un promedio
      mmdExp=1/4,
      E_c=2.1e-8, # KJ
      E_cr=21e-8, # KJ
      m_c=3e-12,  # Kg
      follow=NULL,
      gompB=-5.5,
      gompC=-1.5,
      grassMode='fixed',
      # fixed | semiFixed | bholt | randSprout (?)
      grassProb=0.5,
      icl0=10.68,  # KJ/km
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
      logitA1=.06,  # numeric
      M=0.1,      # numeric, ..
      mei0=8,     # numeric, ..
      mpd0=18,    # numeric, ..
      mpdExp=1/2, # numeric,
      N_0=9,  # integer, No. de individuos iniciales | 'K'
      npsi=2, # numeric
      p.max=10, # numeric
      p.0=0.01, # probability
      p.1=0.9,  # probability
      pastoRo=2, # numeric, growth rate
      pm=0.05,   # numeric, body mass fraction
      plotPop=TRUE, # logical, plot population dynamics?
#     ptExp=3,
      pointsFun=pfGompAuto1,
      # pfLogitAuto | pfLogitManual | pfGompAuto1 | pfGompAuto2 |
      # pfGompManual | pfPotencia
      ptsMode='PBB', # PBB | cualquier cosa
      randomTurn=TRUE,
      bmr0=293, # KJ/day ver detalles en doc.ibm.R
      bmrExp=(3/4),
      ruc=0.1,  # Costo asociado a usar las reservas
      saveRecord=TRUE,
      showTime=TRUE,
      showStats=TRUE,
      showSummary=TRUE,
      sizeMode='random',
      # adults | infants | random | ratio
      sizeRatio=0.4,
      tfinal=100,
      trs0=0.1,
      trsExp=1,
      verboso=TRUE,
      yield=100) {

  # 0. INICIALIZACIONES
  # Comienza el conteo de tiempo:
  ptm <- proc.time()[3]
  landsisnull <- FALSE

  # paquetes necesarios:
  require(splancs, quietly=TRUE)
  require(ellipse, quietly=TRUE)

  # 0.0 Salida de info:
  if (!verboso) {
    showSummary <- showTime <- showStats <- plotPop <- FALSE
  }
  if (verboso)
    cat('\n-- COMIENZA LA SIMULACIÓN --\n\n')

  if (is.null(import)) {  
    # Coordenadas de los parches
    if (is.null(lands) || missing(lands)) {
      landsisnull <- TRUE
      lands <- mklands(dim_=landsDim_,
                       dist_=landsDist_,
                       lmax_=landsLmax_,
                       n_=landsN_,
                       rdist_=landsRdist_,
                       type=landsType)
    }
    landsLmax_ <- lands$parms$lmax_
    if (levelFocus >= landsLmax_ && levelFocus > 0)
      levelFocus <- landsLmax_ - 1
      #-->Si no se hace esta corrección va a dar error
  
    # 0.1 Objetos fijos
    fixedObjects()
  
    # 0.2 Individuos iniciales
    indivSeed()
  
    # 0.3 Registros iniciales:
    register()

  } else {
    # Protocolo de importación de simulaciones anteriores
    bag <- importer(import, tfinal)
    for (j in 1:length(bag))
      assign(names(bag)[[j]], bag[[j]])
  }

  indStats <- stats()
  if (showStats) {
    # Estadísticas (de adultos):
    print(indStats)
    print(lands)
    cat('\nYIELD = ', round(yield, 2), '\n', sep='')
  }

  # COMIENZAN LAS ITERACIONES
  # Seguimiento de las iteraciones
  if (showTime) {
    cat('\n Tiempo (de ', tfinal, '),\n>>', sep='')
    seeTime(t_ - 1, pop)
  }

#   nuevos <- 0
#   dir.create(tempdir())
#   tmp <- tempfile()

  while (pop[t_ - 1] > 0 && t_ <= tfinal) {
      # Atributos individuales según el tamaño corporal:
    icl <- icl0 * m ^ iclExp
    mmd <- mmd0 * m ^ mmdExp
    mmd -> restoMov
    mmc <- mmd * icl
    mpd <- mpd0 * m ^ mpdExp
    bmr <- bmr0 * m ^ bmrExp
    mei <- MEI * m / M
    tmc <- B_c * m / m_c # antes: TMC * m / M
    trs <- trs0 * m ^ trsExp
    psi <- (npsi * mei - tmc) * m_c / E_cr
    if (ptsMode != 'PBB')
      psi <- psi + trs
    tmcBiom <- tmc * m_c / E_cr
    obtEner   <- numeric(N)
#     RCB <- - (tmc + mmc * m_c / E_cr) # mmc = maximum movement cost
#     #-->RCB = "Random Choice Balance"
#     #-->"No Movement Balance"

    if (randomTurn) {
      # Para que el orden en q actúan sea aleatorio:
      index <- sample(N)
    } else {
      # Con esta opción se mantiene el orden 4ever
      index <- 1:N
    }
#     conteo <- 0
    for (i in index) {
      # 1. MOVIMIENTO Y ALIMENTACIÓN
      if (!is.null(follow) && nombres(i) == follow)
        browser()
      goRand   <- FALSE
      donoth   <- FALSE
      xy       <- xypos[i,]
      cfa      <- ellipse(0, centre=c(0,0), t=mpd[i] + 1e-2)
      cfa_i    <- cbind(cfa[,1] + xy[1], cfa[,2] + xy[2])
      visibles <- inpip(xypasto, cfa_i, bound=TRUE)

      while (restoMov[i]          > tol &&
             mei[i] - foodAcum[i] > tol &&
             !donoth) {
        if (is.na(optPatch[i])) {
          # Si debe elegir un parche nuevo hacia el cual moverse...

          # 1.1 Elección del próximo parche
          # Distancia y cantidad de recursos:
	  dist2visib <- distancias(xy, xypasto[visibles,])
	  closer     <- which.min(dist2visib) 
          # Cocientes: para establecer el costo relativo por mantenimiento.
          ratioDist  <- dist2visib / mmd[i]
# 	  print(cbind(pasto[visibles], xypasto[visibles, 'x']))
          ratioRsrc  <- pasto[visibles] / mei[i]
          ratioRsrc  <- ifelse(ratioDist > 2.5, 0, ratioRsrc)
          # NOTA: 2.5 es un valor arbitrario, podría ser cualquier otro...

          # Costos:
          costPerDist <- dist2visib * icl[i] * m_c / E_cr
          costPerMant <- pmax(ratioDist, ratioRsrc) * tmcBiom[i]

          # Ganancias:
          plus <- ratioRsrc * mei[i] * m_c / E_cr

          # Balance de Biomasa:
          PBB <- c(plus - (costPerDist + costPerMant),
                   # Ballance x ir a cada parche a comer
                   - (tmc[i] + icl[i] * restoMov[i]) * m_c / E_cr,
                   # Balance x mov. aleatorio.
                   - tmcBiom[i] * max(restoMov[i] / mmd[i],
		                      1 - foodAcum[i] / mei[i]))
                   # Balance x quedarse en el lugar.

          # Incluír el costo extra por usar reservas (si PBB < 0):
          PBB[PBB < 0] <- PBB[PBB < 0] / (1 - ruc)

          # Asignación de puntajes & elección de parche:
          input <- PBB
          if (ptsMode != 'PBB')
            # La alternativa a PBB es tener en cuenta las reservas de cada
            # individuo a la hora de asignar puntajes.
            input <- input + trs[i]
	  puntaje <- pointsFun()
	  if (hasrep[i] && length(closer) > 0) {
	    puntaje    <- puntaje[-closer]
	    visibles   <- visibles[-closer]
	    dist2visib <- dist2visib[-closer]
	  }
          puntaje[puntaje <= 0] <- tol
          choiceNum <- chFun(puntaje, critQuant=chFun.quant)
          optPatch[i] <- visibles[choiceNum]
          dist2patch  <- dist2visib[choiceNum]
	  if (!is.na(dist2patch) && dist2patch < tol)
	    donoth <- TRUE

          # 1.1.2.1 Elegir quedarse o salir en una dirección al azar
          if (length(visibles) == 0) {
            goRand <- TRUE
          } else {
            if (choiceNum == length(visibles) + 1)
              goRand <- TRUE
            if (choiceNum == length(visibles) + 2)
              donoth <- TRUE
          }
	  if (donoth)
	    break
        } else {
          # Si continua moviéndose desde el turno anterior...
          dist2patch <- distancias(xy, xypasto[optPatch[i],])
        }

        if (!(goRand || donoth)) {
          # 1.2.1 Movimiento hacia el parche
          # Si llega al parche
          if (restoMov[i] - dist2patch > tol) {
            # 1.3 Comer del parche
            food <- feed(foodAcum[i], mei[i], pasto[optPatch[i]])
            # obtEner[i] <- obtEner[i] + food * (1 - (food * sdaMax / mei[i]))
            # Esta línea se comentó por un efecto inesperado:
            # Los individuos obtienen más energía si hacen varias comidas
            # de valor intermedio que si hacen una sóla comida con el
            # máximo posible de energía. Considerando que esto no parece
            # muy realista ni útil, se optó por que los individuos hacen el
            # procesamiento de la energía 1 vez por turno.
            foodAcum[i]        <- foodAcum[i] + food
            pasto[optPatch[i]] <- pasto[optPatch[i]] - food
            xy                 <- xypasto[optPatch[i],]
            restoMov[i]        <- restoMov[i] - dist2patch
            optPatch[i]        <- NA
          # Si no le da para llegar
          } else {
            dirVector     <- xypasto[optPatch[i],] - xy
            dirVectorNorm <- sqrt(sum(dirVector ^ 2))
            movVector     <- dirVector * restoMov[i] / dirVectorNorm
            xy            <- xy + movVector
            restoMov[i]   <- 0
          }
        } else {
	# 1.2.2 Movimiento al azar: ejecución
	  if (goRand) {
	    xyRand <- cfaRand[sample(60, 1),]
	    xy     <- xy + restoMov[i] * xyRand
	    restoMov[i] <- 0
	    break
#           xypos[i,]   <- xy + restoMov[i] * xyRand
	  }
        }
        cfa_i    <- cbind(cfa[,1] + xy[1], cfa[,2] + xy[2])
        visibles <- inpip(xypasto, cfa_i, bound=TRUE)
      }
      xypos[i,]  <- xy
      obtEner[i] <- foodAcum[i]
    }
#     cat('(', round(conteo / sum(m < M / 2), 2), ')', sep='')
#     cat(conteo / sum(m < M / 2), '', file=tmp, append=TRUE)

    # 1.4 BALANCE DE BIOMASA DE BIOMASA (crecimiento está en el pto. 6).
    deltaBiom <- (obtEner - icl * (mmd - restoMov)) * m_c / E_cr - tmcBiom
    deltaBiom[deltaBiom < 0] <- deltaBiom[deltaBiom < 0] / (1 - ruc)
    reser <- reser + deltaBiom
    extraBiom <- reser - trs
    reser[extraBiom > 0] <- trs[extraBiom > 0]
    extraBiom[extraBiom < 0] <- 0



    # 2. REGENERACIÓN DE PASTO
#     browser()
    if (saveRecord)
      pastoAll[[t_]] <- pasto
    grassGrow()
    pastoPop[t_] <- sum(pasto)

    # 3. SUPERVIVENCIA
    lifeSpan <- lifeSpan - 1
    survive  <- reser >= 0 & lifeSpan > 0
    alive    <- (1:N)[survive]
    hasrep   <- (1:N) > Inf
    N        <- sum(survive)
    deaths[t_] <- sum(!survive)
    
    if (N > 0) {
      oldFoodAcum <- foodAcum[alive]
      #-->Valores para poner en el objeto 'record'
      extraBiom <- extraBiom[alive]
      lifeSpan  <- lifeSpan[alive]
      nombres   <- nombres[alive]
      optPatch  <- optPatch[alive]
      reser     <- reser[alive]
      m         <- m[alive]
      last.patch <- last.patch[alive]
      next.patch <- next.patch[alive]
      xypos     <- xypos[survive,]
      dim(xypos) <- c(sum(survive), 2)
      # 1.5 REGISTRO DE MIGRACIONES
      mig.litte()
      #if (saveRecord)
      #  migrator()
      
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
	  hasrep <- crios > 0
          reprod <- (1:N)[hasrep]
          crios  <- crios[reprod]
          babyBiom[reprod] <- babyBiom[reprod] - crios * minBio

          # Los nuevos individuos:
          nuevos       <- sum(crios)
          nombres      <- c(nombres, (lastname + 1):(lastname + nuevos))
          hasrep       <- c(hasrep, numeric(nuevos) > 1)
          lastname     <- lastname + nuevos
          lifeSpan     <- c(lifeSpan, rpois(nuevos, ALS))
          m            <- c(m, rep(m0, nuevos))
          reser        <- c(reser, rep(trsMin, nuevos))
          babyBiom     <- c(babyBiom, numeric(nuevos))
          newXypos     <- matrix(ncol=2, nrow=nuevos)
          newXypos[,1] <- rep(xypos[reprod, 1], crios)
          newXypos[,2] <- rep(xypos[reprod, 2], crios)
          xypos        <- rbind(xypos, newXypos)
	  next.patch   <- c(next.patch, rep(next.patch[reprod], crios))
        }
      }

      foodAcum <- numeric(N + nuevos)

    } else {
    # SI TODOS MUEREN
      oldFoodAcum <- NULL
      nombres     <- NULL
      optPatch    <- NULL
      hasrep      <- NULL
      m           <- NULL
      xypos       <- NULL
      extinction  <- TRUE
      lifeSpan    <- NULL
      nuevos      <- 0
      if (saveRecord)
        vecinos[[t_]][1:npatchFocus] <- 0
      #-->Valores para poner en el objeto 'record'
    }
    births[t_] <- nuevos
    N          <- N + nuevos

    # 7. ACTUALIZACIÓN DE LOS REGISTROS
    if (N > 0 && saveRecord) {
      record[[t_]] <- data.frame(
                foodAcum=c(oldFoodAcum, numeric(nuevos)),
                name=nombres, m=m,
                reser=reser, lifeSpan=lifeSpan,
                x=xypos[,1], y=xypos[,2])
    }
    pop[t_] <- N

    if (showTime)
      seeTime(t_, pop)
    t_ <- t_ + 1
  }

  if (showTime)
    cat('\n FIN\n')

  # 8. PREPARACIÓN DE LA SALIDA
  t_       <- t_ - 1
  pop      <- pop[1:t_]
  births   <- births[1:t_]
  deaths   <- deaths[1:t_]
  if (saveRecord) {
    record   <- record[1:t_]
    pastoAll <- pastoAll[1:t_]
#     migra    <- migra[1:t_]
#     vecinos  <- vecinos[1:t_]
#     emigra   <- emigra[1:t_]
#     inmigra  <- inmigra[1:t_]
  }
  llama <- match.call()
  frm   <- formals(ibm)
  parms <- lapply(names(frm), get, envir=sys.parent(0))
  names(parms) <- names(frm)

  if (landsisnull)
    parms$lands <- NULL

  if (!is.null(import)) {
  # ATENCIÓN: AÚN NO SE IMPLEMENÓ EL IMPORTAR (C/ SAVERECORD)
    llama <- c(llama, import$call)
    names(llama) <- paste('call', length(llama):1, sep='.')
    parms$import <- import$call
    if (is.null(import$lands))
      parms$lands <- NULL
  }

#   if (saveRecord) {
#     totalMigra <- sapply(migra, sum)
#     ijMigra  <- totalMigra / (npatchFocus * (npatchFocus - 1))
    #-->ijMigra: cantidad de migrantes per i ---> j (pares de parches)
#     popMigra <- totalMigra / pop
    #-->popMigra: cantidad de migrantes per cápita
#     totalEmigra  <- sapply(emigra,  function(x) sum(unlist(x)))
#     totalInmigra <- sapply(inmigra, function(x) sum(unlist(x)))
#   }

  # LISTA DE SALIDA
  out <- list(
    fun=ibm,
    babyBiom=babyBiom,
    births=births,
    deaths=deaths,
    call=llama,
    extinction=extinction,
    foodAcum=foodAcum,
    hasrep=hasrep,
    indStats=indStats,
    lands=lands,
    last.patch=last.patch,
    lifeSpan=lifeSpan,
    m=m,
    next.patch=next.patch,
    nombres=nombres,
    optPatch=optPatch,
    parms=parms,
    pasto=pasto,
    pastoPop=pastoPop,
    pointsFun=pointsFun,
    pop=pop,
    reser=reser,
    tiempo=proc.time()[3] - ptm + extra_t,
    totalMigra=totalMigra,
    xypos=xypos)

  if (saveRecord) {
#     out$emigra       <- emigra
#     out$ijMigra      <- ijMigra
#     out$inmigra      <- inmigra
#     out$migra        <- migra
    out$pastoAll     <- pastoAll
#     out$popMigra     <- popMigra
    out$record       <- record
#     out$totalEmigra  <- totalEmigra
#     out$totalInmigra <- totalInmigra
#     out$totalMigra   <- totalMigra
#     out$vecinos      <- vecinos
  }

  class(out) <- c('ibm', class(out))

  if (showSummary)
    print(out, stats=FALSE)

  if (plotPop)
    plot(out, 'pop')

  return(out)
}

cdir <- getwd()
ruta <- file.path(cdir, 'aux.ibm.R')
source(ruta)
