mmd0 <- 3 * 1.04  # DMD  <-  Daily Movement Distance (Garland 1983)  <-  1.038 * M ^ 0.25 Km/day <- supongo que es un promedio...
mmdExp <- 1/4 
icl0 <- 10.68  # KJ/km
iclExp <- 0.75  # 0.70
mei0 <- 8   # numeric  ..
mpd0 <- 18  # numeric  ..
mpdExp <- 1/2  # numeric  
bmr0 <- 293  # KJ/day ver detalles en doc.ibm.R
bmrExp <- (3/4)
E_c <- 2.1e-8 # KJ
m_c <- 3e-12  # Kg

modelito <- function(mass, yield=1e6, npatch=9, tfinal=100, N0=1, conv=m_c / E_c, A=2*2) {

	Rtot <- yield * npatch
	r    <- Rtot / A
	mmd  <- mmd0 * mass ^ mmdExp
	icl  <- icl0 * mass ^ iclExp
	bmr  <- bmr0 * mass ^ bmrExp
	m0   <- 0.01 * mass

	gain <- mmd * r
	cost <- mmd * icl + bmr
	print(conv * (gain - cost))
	print(A > mmd)

	t_ <- 1
	pop <- numeric(tfinal)
	pop[1] <- N0
	N <- N0
	store <- numeric(N) + gain * 2
	
	while(t_ <= tfinal && N >= 0) {
		print(paste('tiempo:', t_, 'N =', N))
		ener <- numeric(N)
		index <- sample(N)
		R <- Rtot

		

		
		for(i in index) {
			if(R > 0) {
#~ 				browser()
				ener[i] <- ifelse(gain < R, gain, R)
				R <- R - ener[i]
			}
		}
		bal   <- ener - cost
		store <- store + bal
		store <- store[store >= 0]
		kids  <- floor(store * conv / mass)
		store <- store - kids * mass / conv
		N <- length(store) + sum(kids)
		store <- c(store, rep(0, sum(kids)))
		t_ <- t_ + 1
		pop[t_] <- N
	}
	return(pop[pop > 0])
}
