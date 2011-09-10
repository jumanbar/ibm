mei <- function(
	pm=0.05,
	n_=5, # No. de crías
	M=0.1, # Kg
	epm=7000, # Ec / mc
	rmr0=600,
	sdaMax=0.35,
	icl0=.010678,
	dmd0=1) {

	mei0 <- (M * pm * n_ * epm + icl0 * dmd0 * M + rmr0 * (M ^ 0.75)) / (rmr0 * (M ^ .75) * (1 - sdaMax))
	return(mei0)
}
