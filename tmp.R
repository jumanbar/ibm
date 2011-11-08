plotGvisLineChart <- function(x, options=list(width=800)) {
  tabla <- with(x,
                data.frame(Population=pop,
                           Migration=totalMigra,
                           Deaths=deaths,
                           Births=births,
                           Iteracion=1:length(pop)))
  gvisObj <- gvisLineChart(tabla,
                           xvar="Iteracion",
                           yvar=names(tabla)[-5],
                           options=options)
  plot(gvisObj)
  out <- list(gvis=gvisObj, tabla=tabla)
  class(out) <- 'ibmGvis'
  invisible(out)
}
