

  B_c <- m_c * bmr0 * M ^ (bmrExp - 1)

  BMR <- bmr0 * M ^ bmrExp

  B_c * M / m_c = bmr0 * M ^ bmrExp

  

#->Asume que k promedio es 2 (i.e.: average energy intake = 2 * BMR)
#->El tejido de reserva tiene un valor diferente, del punto de vista
#  energético, al tejido de normal. Para simplificar los cálculos,
#  el valor 'minBio' es la suma del tejido normal y de reserva de un
#  recién nacido, pero todo en el equivalente a tejido normal.
#  Esta medida sirve para calcular en número de descendientes de cada
#  adulto en el momento de reproducción.


 - Sistema intermedio entre el últmo cambio: me equivoqué, West et al. 2001 plantean un cálculo proporcional al tamaño corporal para el costo por mantenimiento (B_c * m / m_c). Los demás atributos tamaño dependientes van a seguir los escalamientos planteados originalmente (excepto por mei, ya que se puede considerar que dicho atributo es isométrico).

 - Se arreglaron varias cosas de la parte de elección de parche, sobre todo lo que tiene que ver con los cálculos de costos y beneficios.

 - trs0 = 0.1
