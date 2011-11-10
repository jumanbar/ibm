
getNormSd <- function(x, init=1) {
  x <- x[init:length(x)]
  v <- numeric(length(x) - 1)
  for (i in 2:length(x))
    v[i - 1] <- var(x[1:i])
  s <- sqrt(v)
  m <- cumsum(v) / (1:length(v))
  o <- 2 * s / m
  return(o)
}
