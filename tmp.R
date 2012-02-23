source('ibm.R')
path <- file.path('/home/juan/Dropbox/saves')
ncor <- 24
i <- 24

ex <- seq(log10(0.001), log10(0.5), len=ncor)
M  <- 10 ^ ex
tf <- function(i) round(3000 + (2000 / 2.4) * ex[i])
tfin <- 1000

# M[i] <- 3.229378

run <- ibm(landsDist_=1, landsRdist_=5, N_0=9, levelSeeds=1,
           mpd0=6, sizeMode="random", als0=5e8,
           trs0=0.1, yield=500, tfinal=tfin, chFun=chooseMax,
           M=M[i], saveRecord=F, verboso=T)
nombre <- paste('prueba-M=', round(M[i], 2), '-', Sys.Date(),
                  '.RData', sep='')
save(run, file=file.path(path, nombre))

#  [1]  0.01000000  0.01350314  0.01823348  0.02462092  0.03324598  0.04489251
#  [7]  0.06061899  0.08185467  0.11052951  0.14924955  0.20153377  0.27213388
# [13]  0.36746619  0.49619476  0.67001875  0.90473572  1.22167735  1.64964807
# [19]  2.22754295  3.00788252  4.06158599  5.48441658  7.40568469 10.00000000

# run <- ibm(landsDist_=1, landsRdist_=5, levelSeeds=1, N_0=1, mpd0=6, sizeMode="random", als0=5e8, trs0=0.1, yield=650, tfinal=tfin, M=10, saveRecord=T, verboso=TRUE)
