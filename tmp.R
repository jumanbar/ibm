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