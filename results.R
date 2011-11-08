####################################
# función que nunca se llegó a implementar
results <- function(experimento='unParcheSolo') {
  filesDir <- paste('exp', experimento, sep='_')
  source(paste(filesDir, 'log.nfo', sep='/'))
  load(paste(filesDir, 'tablas.RData', sep='/'))
  load(paste(filesDir, 'sizesValues.RData', sep='/'))
  
  if (nValues == 1) {
  
    tabla <- tablas[1]
  
    cat('
    K ESTIMADOS PARA DISTINTOS TAMAÑOS CORPORALES.
    ')
    
    plot(popAllK ~ logSize, data=tabla, type='o')
    
    cat(paste('\n
    Se muestra el tamaño poblacional promedio alcanzado\n
    (entre t=', tfinal - ultimos,' y t=', tfinal,') para cada tamaño corporal (NEGRO).\n
    *  Se consideran TODOS los individuos (incluidos los
       que tienen pocas reservas).\n\n'))
    
    nada <- readline('Agregar los tamaños poblacionales para individuos con\n\treservas > (1.5 * minres)?(s/N): ')
    
    if (nada != "" && (nada == "s" || nada == "S")) {
    
      points(popK ~ logSize, data=tabla, type='o', col=4)
    
      cat('
      Se muestra el tiempo de simulación en función del
      tamaño poblacional promedio (AZUL).
      \n')
    
    }
    
    nada <- readline('\nVer las dinámicas poblacionales?(s/N):\n\t(todas las simulaciones) ')
    
    if (nada != "" && (nada == "s" || nada == "S")) {
      for (s in 1:nSizes) {
        cat(paste('\nSize nro.', s, 'de', nSizes, '\n'))
        for (r in 1:nReps) {
    
          nada <- readline(paste('Repetición nro.:', r, 'de', nReps))
          load(paste(filesDir, '/value_1_size_', s,
                    '_rep_', r, '.RData', sep=''))
          plot(x$popAll, type='o')
          points(x$pop, type='o', col=4)
          print(paste('Promedio de Nro. de individuos:',
              round(mean(x$pop[(tfinal - ultimos):tfinal], 2))))
        }
      }
    }
    
    nada <- readline('\nVer escalamientos (regresiones) de las densidades poblacionales?\n(incluye individuos con pocas reservas)\n\t(S/n):\t\t')
    
    if (nada == "" || nada == "s" || nada == "S") {
    
      r <- lm(log10(popAllK) ~ logSize, data=tabla)
      plot(popAllK ~ logSize, data=tabla, type='o', log='y')
      abline(r, col=2, lwd=3)
      
      cat('\nRECTA AJUSTADA:')
      print(r)
      pendiente <- round(r$coefficients[2], 3)
    
      legend('bottomleft', title='Pendiente:', legend=pendiente,
          lwd=4, col=2, cex=2, bty='n')
    }
  
  
  } else {
  
    sizes <- seq(logMinSize, logMaxSize, length.out=nSizes)
    values <- seq(minValue, maxValue, length.out=nValues)
    values <- unique(values)
  
    nada <- menu(title='\nQué tipo de población querés visualizar?',
          choices=c(
            'Todos los individuos',
            'Sólo los que tienen reservas > 1.5 * minres'))
  
    if (nada == 1) tabla <- sizesValues else tabla <- sizesValuesAll
  
    colores <- heat.colors(nValues + 7)
    
    matplot(sizes, tabla, type='o', xlab='log Size', ylab='K estimado', lwd=3, lty=1, col=colores[1:nValues])
    
    legend('topright', lwd=3, col=colores, bty='n', cex=1.5, legend=paste(variable, '=', values))
    
    cat(paste('\n
       Densidades poblacionales para cada tamaño corporal (en log)\n\n
       Cada línea es el resultado con un valor distinto de', variable, '\n\n'))
  
       
    nada <- readline('\n>>> Siguiente: relaciones yield ~ K (estimado) para para cada M...\n')
    
    colores <- heat.colors(nSizes + 7)
    
    matplot(values, t(tabla), type='o', lwd=3, lty=1, ylab='K estimado', xlab=variable, col=colores[1:nSizes])
    
    legend('topleft', pch=19, pt.cex=4, legend=c('Min Size', 'Max Size'),
        col=c(colores[1], colores[nValues]), cex=2, bty='n')
    
    cat(paste('\n
       Densidades poblacionales para cada valor de', variable, '\n\n
       Cada línea corresponde a un tamaño corporal distinto\n\n
       (los tamaños corporales aumentan haca el extremo amarillo)\n\n'))
  }
}
####################################
