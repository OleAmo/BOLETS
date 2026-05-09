library(sf)
library(dplyr)
library(readr)


# ----- DADES SHAPE -----
# -----------------------


comarques_punts <- st_read("data/raw/COMARQUES_punts.shp")

# ----- FUNCIONS en ALTRE ARXIU -------
# ------------------------------------

source("scripts/funcions.R")


#   ---------- OBJECTIU DE LA APP ---------
#   ---------------------------------------

#     -) Crear una APP que ajudi a detectar zones idonees on hi hagi bolets
#     -) He de saber quines variables fisiques son idonees pels bolets
#     -) Començaré amb DADES CLIMÀTIQUES

#     DADES CLIMÀTIQUES:

#     -) Usare la API openmeteo per treure dades de
#     -) Venen de la API OPEN METEO

#     -) He creat FUNCIONS dins de ARXIU funcionR que obté dades
#     -) Les dades son per CADA COORDENADA
#     -) Les dades son TEMP, HUMITAT i VENT
#     -) De cada una tenim Max i MIN

#     DADES COMARCA

#     -) Per cada COMARCA tinc un shape de punts (comarques_punts)
#     -) Calcularé per cada COMARCA la mitja de tots el punts
#     -) Així sabré per COMARCA la MITJA de TEMP, HUMIT, VENT

#     EM FALTARIA DADES de:

#     -) Obtenir la PLUJA
#     -) La ALTITUD
#     -) El tipus de VEGETACIÓP


# ---------------------------------------
# ------------- FEIN A FER
# ----------------------------------------

#     -) Crear DF del punts 
#     -) Crear columna COORDENADES

#     -) Comprovar que funciona la FUNCIO = create_DF_NO_GEOM()
#     -) Comprovar que dona les DADES

#     -) Crear un DF amb la MITJA per CADA COMARCA
#     -) Cada comarca té X PUNTS
#     -) Cacular la MITJA de les DADES DINS SEU



# -------  CREAR DF amb COORDENADES ------
# ----------------------------------------

#     -) Vull una funció que passi de GEMETRIA a coordenades
#     -) Intdrodueixes una geometria 
#     -) Obtens LAT i LONG en porjecció 4326 = La que usa la API OPEN METEO




coords <- COORDS_create(comarques_punts[1,])

date <- "2024-05-08"


DF_BCN <- create_DF_GEOM(coords$lat,coords$long,date,date)


