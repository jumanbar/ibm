## v1.4
epm=???, # Energy per mass (energía por unidad de masa) = E_c / m_c
gompAuto=TRUE
gompB=-5.5
gompC=-1.5
mei0=1.
## "Maximum Energy Intake"
## El consumo máximo de cada individuo por unidad de tiempo
## está determinado por la ecuación: || mei = mei0 * rmr ||
mpd0=0.6
## "Maximum Perceived Distance"
## La distancia máxima a la que un individuo puede percibir su entorno (parches)
## está determinada por la ecuación: || mpd = mpd0 * M ^ mpdExp ||
mpdExp=1/2
## Idem que mpd0
rmr0=600
## "Resting Metabolic Rate"
## La tása metabólica de reposo de cada individuo está determinada por la
## ecuación: || rmr = rmr0 * M ^ rmrExp ||
## Tomo rmr0 = aprox 2 * bmr0, y bmr0 = 293 KJ/day (Garland 1983)
## Ojo que para West etal 2001 bmr0 = 0.019 W (1.6416 KJ/day),
## pero este trabajo cita a su vez a Peters 1983, en donde dice que
## Hemmingsen (1960) calculó la smr0 como 4.1 W (354.24 KJ/day)
## Ver también Apendix IIIa del mismo libro:
## rmr0 = 6.81 W (passerines, day) -> 588.384 KJ / day
## rmr0 = 5.56 W (passerines, night) -> 480.384 KJ / day
## rmr0 = 4.41 W (non-passerines, day) -> 381.024 KJ / day
## rmr0 = 3.57 W (non-passerines, night) -> 308.448 KJ / day
rmrExp=3/4
## Idem que rmr0
lands=mkvecin(mklands(), 2)
## Dos opciones:
## 1. Objeto obtenido con la función mkvecin
## 2. NULL (habilita la construcción del paisaje desde dentro de la función ibm).
##      Nota: debería pasar lo mismo si no se pone nada en este argumento.
landsDim_=2         
## Argumento pasado a mkvecin (dim_) que determina la dimensión geométrica
## del paisaje. Por ej. si es una superficie, landsDim_=2
#* Sólo tiene efecto si lands = NULL
landsDist_=1
## Argumento pasado a mklands (dist_). Es la distancia entre dos parches
## consecutivos si se recorre el paisaje en forma horizontal (o vertical).
## Sólo cuenta para el nivel 1 del fractal (si landsType='fractal'). Para
## niveles superiores, ver landsRdist_.
#* Sólo tiene efecto si lands = NULL
#* El efecto difiere según si landsType = 'fractal'/'regular' o 'randUnif'
#*		(randUnif: distancia promedio entre parches consecutivos en misma dimensión)
landsLmax_=1
## Argumento pasado a mklands (lmax_). Es el nivel máximo en el cual se
## repite el fractal. El nivel básico considerado es 0, por lo tanto, si
## landsLmax_=1, implica que hay dos niveles: nivel 0 ("el parche") y
## nivel 1 (el conjunto de parches)
#* Sólo tiene efecto si lands = NULL
#* Sólo tiene efecto si landsType = 'fractal'
landsN_=3
## Argumento pasado a mklands (n_). Es el número de unidades contenidos en
## cada dimension, dentro de cada nivel del fractal. Se entiende por unidad
## al conjunto de parches que conforman un fractal de un nivel menos (equivalente
## al obtenido con lmax_ - 1),
#* Sólo tiene efecto si lands = NULL
#* El efecto difiere según si landsType = 'fractal', 'regular' o 'randUnif'
#*		(regular o randUnif: número total de parches por dimensión)
landsRdist_=3
## Argumento pasado a mklands (rdist_). Es la relación entre la distancia
## entre unidades de un nivel del fractal N + 1 y su nivel precedente N.
#* Sólo tiene efecto si lands = NULL
#* Sólo tiene efecto si landsType='fractal'
landsType='fractal'
## Argumento pasado a mkvecin (type). Determina el tipo de paisaje generado
## Las opciones son: 'fractal', 'regular', 'randUnif'
#* Los efectos de varios argumentos, como landsN_ difieren según el landsType
#* Sólo tiene efecto si lands = NULL
logitAuto=TRUE
## Determina si se calculan los valores de los parámetros logitA0 y logitA1,
## de tal forma que cumplan tal dos condiciones:
## 		1.  Puntaje de parche con balance demasiado negativo* = logitP0 (= 0.05 del total x defecto)
## 		2.  Puntaje de parche con máximo balance por 2 turnos** = logitP5 (= 0.95 del total x defecto)
##	*  Implica que termine con reservas negativas, es decir, muerte.
##	** = mei * 2 * (1 - sda) - 2 * rmr.
logitA0=-2.5
logitA0=-.06
## Argumentos pasados a logit (a0 y a1). Determina la forma de la curva de desición
## de los individuos. Cuanto menor a0 y mayor a1, más conservadores son los individuos
## (es decir, se arriesgan menos en ir a parche distantes con mucho alimento).
#* Sólo tienen efecto si logitAuto = FALSE
logitP0=0.01
logitP1=0.90
## Ver 'logitAuto'
pastoRo=4
## La tasa de crecimiento máxima de crecimiento de las poblaciones de 'pasto' (recurso).
## Es llamado por la función bholt
plotPop=TRUE
## Determina si se hace una gráfica de la dinámica poblacional al final de la
## simulación
p_max=10
random=TRUE
## Determina si al comienzo de cada iteración en la simulación el orden en que
## actúan los individuos debe ser o no aleatorio
reser0=0.5
## El valor maxres, el cual determina a partir de cuando los individuos invierten de sus
## reservas para producir descendencia, está determinado por la ecuación:
## || reser = reser0 * M ^ resExp ||
resExp=1
## Idem que reser0
showTime=TRUE
## Determina si se debe mostrar el número de iteración (junto con el número de individuos)
## a lo largo de la simulación.
showStats=TRUE
## Determina si se deben mostrar las estadísticas de cada individuo al principio de la
## simulación.
showSummary=TRUE
## Determina si se deben mostrar las estadísticas poblacionales al final de la
## simulación.
M=0.1
## Es el tamaño de los individuos, el cual determina varios parámetros importantes
## En Kg
tfinal=100
## Es el número de iteraciones totales que deben realizarse
vel0=1
## El valor vel, el cual determina a la velocidad de movimiento de los individuos,
## está determinado por la ecuación: || vel = vel0 * M ^ velExp ||
velExp=1/4
## Idem que vel0
verboso=TRUE
## Si es FALSE, bloquea todas las salidas de ibm
## (equivale a plotPop == showTime == showStats == showSummary == FALSE)
yield=60
## Cantidad de recursos contenidos en cada parche del paisaje (en las mismas unidades
## que la tasa metabólica)
arg
## Argumento opcional. Es una lista nombrada con todos los argumentos necesarios para correr
## la función ibm. Existe para facilitar la corrida de experimentos (11-Oct-2010).
...
## Argumentos que se pasan a la función plot, en caso que se pida graficar la dinámica
## poblacional (es decir, plotPop debe ser TRUE para que tenga algún efecto).

## SALIDA:
babyBiom=babyBiom,
births=births,
deaths=deaths,
call=match.call(),
extinction=extinction,
ijMigra=ijMigra,
indStats=indStats,
lands=lands,
migra=migra,
pastoAll=pastoAll,
parms=parms,
pointsFun=pointsFun,
pop=pop,
popMigra=popMigra,
record=record,
tiempo=proc.time()[3] - ptm,
totalEmigra=totalEmigra,
totalInmigra=totalInmigra,
totalMigra=totalMigra,
vecinos=vecinos)
