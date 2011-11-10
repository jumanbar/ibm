als0 <- 2000 # integer: "average life span" (días)
alsExp <- 1/4
mmd0 <- 3 * 1.04  # DMD  <-  Daily Movement Distance (Garland 1983)  <-  1.038 * M ^ 0.25 Km/day <- supongo que es un promedio...
mmdExp <- 1/4 
icl0 <- 10.68   # KJ/km
iclExp <- 0.75  # 0.70
mei0 <- 8   # numeric: "maximum energy intake"
mpd0 <- 18  # numeric  ..
mpdExp <- 1/2  # numeric  
bmr0 <- 293  # KJ/day ver detalles en doc.ibm.R
bmrExp <- (3/4)
trs0 <- 0.3
trsExp <- 1
E_c <- 2.1e-8 # KJ
E_cr<- 21e-8 # KJ
m_c <- 3e-12  # Kg
#  hr0 <- .154 # Km^2
#  hrExp <- 1
pm <- 0.05
ruc <- 0.1 # Costo asociado a usar las reservas
