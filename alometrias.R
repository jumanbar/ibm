op <- par(lwd=3, cex=1.75)
consum0 <- 1.5
consExp <- 1
fmr0    <- 0.1
fmrExp  <- (3/4)
percep0 <- 0.6
percExp <- 1/2
reser0  <- 0.5
resExp  <- 1
vel0    <- 1
velExp  <- 1/4

vel     <- function(logSize) log10(vel0) + logSize * velExp
fmr     <- function(logSize) log10(fmr0) + logSize * fmrExp
maxcons <- function(logSize) log10(consum0) + logSize * consExp
maxres  <- function(logSize) log10(reser0) + logSize * resExp
percep  <- function(logSize) log10(percep0) + logSize * percExp

curve(vel(x), from=1e-3, to=3, lwd=2, col=1, ylim=c(-2,3.5))
curve(fmr(x), from=1e-3, to=3, add=TRUE, lwd=2, col=2)
curve(maxcons(x), from=1e-3, to=3, add=TRUE, lwd=2, col=3)
curve(maxres(x), from=1e-3, to=3, add=TRUE, lwd=2, col=4)
curve(percep(x), from=1e-3, to=3, add=TRUE, lwd=2, col=5)

percMaxres <- (log10(percep0) - log10(reser0)) / (resExp - percExp)
velMaxres  <- (log10(vel0) - log10(reser0)) / (resExp - velExp)
velPercep  <- (log10(vel0) - log10(percep0)) / (percExp - velExp)
fmrVel     <- (log10(fmr0) - log10(vel0)) / (velExp - fmrExp)
fmrRoot    <- - log10(fmr0) / fmrExp
percRoot   <- - log10(percep0) / percExp
reserRoot  <- - log10(reser0) / resExp
velRoot    <- - log10(vel0) / velExp

ejes(col=8, lty=1, lwd=1)

secantes <- sort(c(
	percMaxres=percMaxres,
	velMaxres=velMaxres,
	velPercep=velPercep,
	fmrVel=fmrVel,
	fmrRoot=fmrRoot,
	velRoot=velRoot,
	reserRoot=reserRoot,
	percRoot=percRoot))

abline(v=secantes, lwd=1, col='yellow')

legend('topleft', lwd=2, col=1:5, bty='n', legend=c('vel', 'fmr', 'maxcons', 'maxres', 'percep'), cex=.8)

dev2bitmap('curvas_alometricas.png', res=300, width=10, height=8)

save(secantes, file='secantes.RData')

par(op)
