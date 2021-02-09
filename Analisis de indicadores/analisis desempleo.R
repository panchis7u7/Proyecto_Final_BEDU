library(dplyr)
library(ggplot2)
library(scales)

#solicitudes del programa de desempleo
dsempleo.sol <- read.csv("/cloud/project/solicitudes-desempleo.csv")

#tasa de desocupacion importar datos
dsempleot <- read.csv("/cloud/project/tasa-de-desempleo-cdmx.csv")

View(dsempleo.sol)

dim(dsempleo.sol)
  ## Columnas de interes

#dataset desocupacion cdmx
f.dsempleo.cdmx <- filter(dsempleot, type.ID == "9" )


#dataset desocupacion cdmx
f.dsempleo.mex <- filter(dsempleot, type.ID == "mex" )


#Columnas de interes de tasa de desocupacion

head(dsempleot)
head(Conf.dsempleot <- dsempleot[,-10]) 

#Serie de tiempo


dsempleotfull <- na.omit(dsempleot)


head(dsempleot)










