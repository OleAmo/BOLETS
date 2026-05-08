library(sf)
library(terra)
library(tmap)
library(ggplot2)
library(dplyr)
library(readr)

# llegir un shape

comarques_punts <- st_read("data/raw/COMARQUES_punts.shp")


#   ---- SUMARIZE ----
#   ------------------

#   AGRUPO per COMARQUES
#   CALCULO 
#      .-) la MITJA de LONGITUDS
#      .-) la SUMA de LONGITUDS x COMARCA
#      .-) El NÚMERO de FILES x selecció =  n()
