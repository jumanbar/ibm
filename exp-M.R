
##### TERMINAR: CASOS EN QUE YA ESTÁ EMPEZADO...

##### PROPÓSITO:
##### .......................

dirOut  <- 'exp-M'
ibmFile <- 'ibm.R'
maxTry  <- 10

## RANGO DE TAMAÑOS:
logMinSize <- -1
logMaxSize <- 3

## VARIABLE DE INTERÉS:
variable <- 'yield' # (ejemplo)
minValue <- 200
maxValue <- 200

## VARIABLES SECUNDARIAS:
# secondVar es una lista nombrada que sirve para establecer valores alternativos
# a parámetros secundarios de ibm.
secondVar <- vector('list', 0)
secondVar <- within(secondVar, {
#   Agregar las variables aquí, por ejemplo:
#   yield <- 200
#   landsDist_ <- expression(mmd0 * pi / 4)
})
formals(ibm)[names(secondVar)] <- unlist(secondVar)

## CANTIDAD DE SIMULACIONES:
nSizes   <- 4
nReps    <- 2
nValues  <- 1

## TIEMPOS:
tfinal  <- 500
ultimos <- 250

## SEGUIMIENTO x SIMULACIONE INDIVIDUALES
verboso <- FALSE

## BORRAR INTENTO ANTERIOR
borra <- TRUE

#############################################################################################
exper   <- paste(dirOut, 'R', sep='.')
dirOut  <- file.path('../ibm-output', dirOut)

## Borrado:
if (borra)
  unlink(dirOut)

## ÚLTIMA SIMULACIÓN?:
if(any(dir(dirOut) == 'last.RData')) {
  load(file=file.path(dirOut, 'last.RData'))
  count     <- count + 1
  lastValue <- v
  lastSize  <- s
  lastRep   <- r
} else {
  v <- s <- r <- 0
  lastValue <- v
  lastSize  <- s
  lastRep   <- r
  count     <- 0
}
scripts <- paste('scripts', count, sep='-')
dirScr  <- file.path(dirOut, scripts)

dir.create(dirOut, FALSE)
dir.create(dirScr, FALSE)
file.copy(exper,       dirScr, TRUE)
file.copy('runExp.R',  dirScr, TRUE)
file.copy(ibmFile,     dirScr)
file.copy('aux.ibm.R', dirScr)
file.copy('doc.ibm.R', dirScr)
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
  file=paste(dirScr, 'log.nfo', sep='/'))

save(secondVar, file=file.path(dirOut, 'secondVar.RData'))

source('runExp.R')
