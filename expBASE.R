
##### TERMINAR: CASOS EN QUE YA ESTÁ EMPEZADO...

##### PROPÓSITO:
##### .......................

dirOut  <- 'expBASE'
ibmFile <- 'ibm.R'
maxTry  <- 20

## RANGO DE TAMAÑOS:
logMinSize <- 0.5
logMaxSize <- 3

## VARIABLE DE INTERÉS:
variable <- 'yield' # (ejemplo)
minValue <- 30
maxValue <- 150

## VARIABLES SECUNDARIAS (NULL por defecto):
# Ej:
#~ secondVar <- c(yield=10, landsDist_=expression(mmd0 * pi / 4))
#~ formals(ibm)[names(secondVar)] <- secondVar

## CANTIDAD DE SIMULACIONES:
nSizes   <- 8
nReps    <- 2
nValues  <- 3

## TIEMPOS:
tfinal  <- 200
ultimos <- 100

#############################################################################################
## ÚLTIMA SIMULACIÓN?:
if(any(dir(dirOut) == 'last.RData')) {
	load(file=paste(dirOut, '/last.RData', sep=''))
	count     <- count + 1
	lastValue <- v
	lastSize  <- s
	lastRep   <- r
# ~	dirOut <- paste(dirOut, count, sep='_')
} else {
	v <- s <- r <- 0
	lastValue <- v
	lastSize  <- s
	lastRep   <- r
	count     <- 0
}
scripts <- paste('scripts', count, sep='_')
# ~system(paste('rm -RfI', dirOut))
system(paste('mkdir', dirOut))
system(paste('mkdir ', dirOut, '/', scripts, sep=''))
system(paste('cp ', dirOut, '.R ', dirOut, '/', scripts, sep=''))
system(paste('cp runExp.R ', dirOut, '/', scripts, sep=''))
system(paste('cp ', ibmFile,' ', dirOut, '/', scripts, sep=''))
system(paste('cp aux.ibm.R ', dirOut, '/', scripts, sep=''))
source(ibmFile)

cat(
	paste(
		'#FECHA     <- ', Sys.Date(), '\n',
		'variable   <- "', variable, '"\n',
		'logMinSize <- ', logMinSize, '\n',
		'logMaxSize <- ', logMaxSize, '\n',
		'minValue   <- ', minValue, '\n',
		'maxValue   <- ', maxValue, '\n',
		'nSizes     <- ', nSizes, '\n',
		'nReps      <- ', nReps, '\n',
		'nValues    <- ', nValues, '\n',
		'tfinal     <- ', tfinal, '\n',
		'ultimos    <- ', ultimos, '\n',
		sep=''),
	file=paste(dirOut, scripts, 'log.nfo', sep='/'))
	
save(secondVar, file=paste(dirOut, 'secondVar.RData', sep='/'))

source('runExp.R')
