library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(datasets)
#Paquete para series de tiempo
library(tseries)
library(astsa)
library(forecast)
library(tidyverse)
library(lubridate)
library(foreign)
library(quantmod)

install.packages("tseries")
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("foreign")
install.packages("quantmod")





dsdolar <- read.csv("/cloud/project/dolar_maydic2020.csv")
View(dsdolar)

#Transformacion de los datasets
datosdolar2 <- rename(dsdolar, max = Máximo )
datosdolar2 <- rename(datosdolar2, min = Mínimo)
datosdolar2<- rename(datosdolar2, fecha = Fecha )

datosdolar2 <- mutate(datosdolar2, fecha = as.Date(fecha, "%d.%m.%Y"))
head(datosdolar2)

(dolarmin <- select(datosdolar2, fecha, min))
names(datosdolar2)

#Serie de tiempo


serieDolar <- ggplot(datosdolar2, aes(x=fecha, y=max)) + geom_line(color="blue") + 
  labs(x = "Fecha", 
       y = "Acumulado de casos",
       title = paste("Fluctuaci?n del dolar", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) +
  geom_line(aes(y = min), color = "red") +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))

serieDolar
#Estacionalidad y tendencia

dolarmin <- na.omit(dolarmin)
  dim(dolarmin)
serie <- ts(dolarmin$min,frequency = 365, start = c(2020, 03,20))

#####
plot(serie)
prueba <- auto.arima(serie)
summary(prueba)
Box.test(residuals(prueba), type = 'Ljung-Box') ### p-value = 0.9284
plot(residuals(prueba))
pronos <- forecast(prueba,12,level=95)
autoplot(pronos)

serie1 <- ma(serie, 3)
plot(serie1)
prueba1 <- auto.arima(serie1)
summary(prueba1)
Box.test(residuals(prueba1), type = 'Ljung-Box') ###p-value = 0.8336
pronos <- forecast(prueba1,12,level=95)
autoplot(pronos)
pronos
#####

dserie <- decompose(serie, type = "multiplicative")
head(dserie)

plot(dserie)

#Tendencia
plot(dserie$trend, col = "purple", lwd = 2, main = " tendencia", ylab = "Tendencia", xlab = "Mes")

plot(dserie, main = "Tendencia precio Dolar", ylab = "Valor en MXN", xlab = "Mes", ylim = c(20, 23.8))
lines(dserie$trend , col = "purple", lwd = 2)
lines(dserie$seasonal * dserie$trend, col = "red", lty = 2, lwd = 2 )
legend(1949, 800, 
       c('Serie de tiempo original', 'Tendencia', 'Tendencia x Estacionalidad'),
       col = c('black', 'purple', 'red'), text.col = "green4", lty = c(1, 1, 2), lwd = c(1, 2, 2),
       merge = TRUE, bg = 'gray90')


