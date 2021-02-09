library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(datasets)
library(taRifx)
#Paquete para series de tiempo
#library(tseries)
library(astsa)
library(forecast)
#library(tidyverse)
#library(lubridate)
#library(foreign)
#library(quantmod)

install.packages("taRifx")
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("foreign")
install.packages("quantmod")





dsdolar <- read.csv("dolar_maydic2020.csv") 
View(dsdolar)

#Transformacion de los datasets
datosdolar2 <- rename(dsdolar, max = MÃ.ximo )
datosdolar2 <- rename(datosdolar2, min = MÃ.nimo)
datosdolar2<- rename(datosdolar2, fecha = ï..Fecha, var =X..var.  )

datosdolar2 <- mutate(datosdolar2, fecha = as.Date(fecha, "%d.%m.%Y"))




csvdata <- write.csv(datosdolar2, file = "dolar_cov")

datosmut <- read.csv("dolar_cov")
datosdolar2 <- mutate(datosmut, fecha = as.Date(fecha, "%d.%m.%Y"))
head(datosmut)

(dolarmin <- select(datosdolar2,  min, max))
names(datosdolar2)


#Promedios, medias y datos de apertura 
summary(datosdolar2)


#Analisis exploratorio 
#Se observa de forma general la distribucion que tienen los datos
getwd()
#En el max hay dos 
boxplot(dolarmin, col= "blue")
abline(h= median(dolarmin$min))

dir()
hist(dolarmin$min, col = "green")
abline(v= median(dolarmin$min))

hist(datosdolar2$var, col = "green", breaks = (seq(20,25, 1)))
abline(v= median(dolarmin$min))

#RESUMEN DE precio de apertura y cierre, maximo y minimo 
summary(datosmut)

#es un Modelo
serieDolar <- ggplot(datosdolar2, aes(x=fecha, y=max)) + geom_line(color="blue") + 
  labs(x = "mes", 
       y = "máximo y mínimo valor",
       title = paste("Fluctuación del dolar")) +
  geom_line(aes(y = min), color = "red") +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))



serieDolar

#Serie de tiempo 



# 1. Ajuste de serie de tiempo

serie <- ts(dolarmin$min,frequency = 365, start = c(2020, 03,20))


#descomposicion de la serie de tiempo
serie1 <- ma(serie, 3) #suaviza los picos de la serie original con las medias moviles

tsgraph <- plot(serie1) #grafica de la serie de tiempo

#Pronostico
prueba1 <- auto.arima(serie1)
#summary(prueba1)
Box.test(residuals(prueba1), type = 'Ljung-Box') ###p-value = 0.8336
pronos <- forecast(prueba1,12,level=95)

#Grafico predictivo 
autoplot(pronos, main= "Prónostico Dolar/peso MXN")
pronos



