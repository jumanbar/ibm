source('ibm.R')

ex <- seq(-2, 1, len=24)
Ms <- 10 ^ ex

run <- ibm(landsDist_ = 4, levelSeeds = 1,
           mpd0 = 6, sizeMode = "random",
           trs0 = 0.1, yield = 2000, tfinal=20000,
           M = Ms[23])

save(run, file='../saves/corrida-M=7.4.RData')
