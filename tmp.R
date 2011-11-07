plotgvis <- function(x, from=1, to=length(x$pop)) {
# x: objeto ibm
#   require(ggplot2)
  require(googleVis)
#   require(Hmisc)
  rec    <- x$record[from:to]
  total  <- sapply(rec, nrow)
  tiempo <- rep.int(1:length(total), total)
  fecha  <- as.Date(tiempo, origin='0/1/1')
  tabla  <- data.frame(time=fecha, foodAcum=0, name=0, m=0, reser=0, x=0, y=0)
  for(i in 1:length(total)) {
    tabla[tiempo == i, -1] <- rec[[i]]
  }
  stateSettings <- '{"nonSelectedAlpha":0.4,"playDuration":15000,"duration":{"multiplier":1,"timeUnit":"D"},"xZoomedIn":false,"xAxisOption":"5","uniColorForNonSelected":false,"xZoomedDataMin":0,"iconKeySettings":[],"colorOption":"_UNIQUE_COLOR","yZoomedIn":false,"time":"1900-01-02","showTrails":true,"orderedByX":false,"xZoomedDataMax":12,"yZoomedDataMin":0,"iconType":"BUBBLE","xLambda":1,"dimensions":{"iconDimensions":["dim0"]},"orderedByY":false,"yLambda":1,"yZoomedDataMax":11.8780815,"sizeOption":"3","yAxisOption":"6"}'
  # Note: this are the settings I would choose for a default view, but it
  # doesn't seem to work properly... hope it gets right in next versions.

  gvisObj <- gvisMotionChart(tabla, timevar="time", idvar="name",
                             options=list(state=stateSettings))
  plot(gvisObj)
  out <- list(gvis=gvisObj, tabla=tabla)

  # Outgoing message:
  cat("Change settings manually in the browser. The method showed\n")
  cat("in the documentation doesn't seem to work.\n")

  class(out) <- 'ibmGvis'
  invisible(out)
}

plot.ibmGvis <- function(x, ...)
  plot(x$gvis, ...)
