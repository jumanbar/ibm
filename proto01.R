source('constantes.R')
N      <- 20
tfinal <- 10000
area   <- 9 * 9
.15 * 9 -> totalResources
resDen <- totalResources / area
M      <- 0.700
pm     <- 0.05
m0     <- pm * M
trsMax <- trs0 * M ^ trsExp
trsMin <- trs0 * m0 ^ trsExp
mass2e <- E_c / m_c
e2mass <- m_c / E_c
mass2r <- E_c / E_cr
r2mass <- E_cr / E_c
minBio <- m0 + trsMin * mass2r
B_c    <- 2 * m_c * bmr0 / M ^ (1 / 4)
als    <- als0 * M ^ alsExp # Average Life Span (Peters 1983)
#->Sale de las ecuaciones de West et al. 2001 (curva de crecimiento)

pop <- numeric(tfinal)
t_  <- 1
pop[t_] <- N

m <- rep(M, N)
reservas <- rep(trsMax, N)
nombres  <- 1:N
lastname <- N
lifeSpan <- rpois(N, als)
babyBiom <- numeric(N)

while(N > 0 && tfinal >= t_) {

 ## Alometrías et al.:
	icl <- icl0 * m ^ iclExp
#  	mmd <- mmd0 * m ^ mmdExp -> restoMov
#  	mmc <- mmd * icl
#  	mpd <- mpd0 * m ^ mpdExp
	bmr <- bmr0 * m ^ bmrExp
	mei <- mei0 * bmr
	tmc <- B_c * m / E_cr
	#-->tmc = "total maitenance cost"
	trs <- trs0 * m ^ trsExp

	BMR <- bmr0 * M ^ bmrExp
	MEI <- mei0 * BMR

	avgArea     <- area / N
	cumArea     <- cumsum(rep(avgArea, N))
	areaNoise   <- rnorm(N - 1, mean=0, sd=avgArea * .25)
	cumArea[-N] <- cumArea[-N] + areaNoise
	indAreas    <- diff(sort(c(0, cumArea)))

	gain  <- indAreas * resDen
	mei   -> gain[gain > mei]
	saldo <- gain - tmc

	u <- saldo < 0
	saldo[u] <- saldo[u] / (1 - ruc)
	deltabiom <- saldo * e2mass * mass2r
	reservas <- reservas + deltabiom

	vivos <- reservas >= 0

	m[vivos] -> m
	reservas <- reservas[vivos]
	nombres  <- nombres[vivos]
	lifeSpan <- lifeSpan[vivos]
	babyBiom <- babyBiom[vivos]

	excedente <- reservas - trs
	if(any(excedente > 0)) {
		crecen <- which(m < M & excedente > 0)
		if(length(crecen) > 0) {
			m[crecen] <- m[crecen] + excedente[crecen] * r2mass
			m[m > M] <- M
			excedente[crecen] <- 0
		}
		e <- which(excedente > 0)
		babyBiom[e] <- (babyBiom + excedente)[e]
		babies <- floor(babyBiom[e] * r2mass / minBio)
		padres <- which(babies >= 1)
		if(length(padres) > 0) {
			babyBiom[padres] <- babyBiom[padres] - babies[padres] * minBio
			b <- sum(babies[padres])
			m <- c(m, rep(m0, b))
			nombres  <- c(nombres, (lastname + 1):(lastname + b))
			lastname <- lastname + b
			reservas <- c(reservas, rep(trsMin, b))
			lifeSpan <- c(lifeSpan, rpois(b, als) + 1)
			# el + 1 es porque en seguida se va a restar
			babyBiom <- c(babyBiom, numeric(b))
		}
		
	}
	
	lifeSpan <- lifeSpan - 1
	vivos    <- lifeSpan > 0
	m[vivos] -> m
	nombres  <- nombres[vivos]
	reservas <- reservas[vivos]
	lifeSpan <- lifeSpan[vivos]
	babyBiom <- babyBiom[vivos]
	
	N  <- length(nombres)
	N  -> pop[t_]
	t_ <- t_ + 1
}

