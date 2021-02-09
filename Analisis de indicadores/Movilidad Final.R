# Elaboró: Diego Armando Morales Corona 

library(ggplot2)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(forecast)
library(ggfortify)

#movilidad <- hospitalizados_transporte_movilidad[, c("fecha", "metro", "metrobus",
#                                                      "rtp", "ecobici")]

movilidad <- hospitalizados_transporte_movilidad[, c('fecha', 'metro', 
                                                     'metrobus',
                                                     'trolebus',
                                                     'rtp', 
                                                     'ecobici')]
automoviles <- hospitalizados_transporte_movilidad[, c('fecha', 'transito')]

movilidad <- mutate(movilidad, fecha = as.Date(fecha, '%d/%m/%Y'))
automoviles <- mutate(automoviles, fecha = as.Date(fecha, '%d/%m/%Y'))
corr <- cor(movilidad[,-c(1)])

### Quitamos las variables con alta correlación
movilidad <- movilidad[ , -4] 
movilidad <- movilidad[ , -4]
corr <- cor(movilidad[,-c(1)])
ggcorrplot(corr, method = "circle", 
           hc.order = TRUE,
           lab = TRUE,
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) +
  ggtitle("Correlograma de los medios de transporte CDMX") +
  theme_minimal()

## Las variables de movilidad seran metro, metrobus, ecobici
## Sumado a la afluencia vehicular

###################################################################################################
######################### POCO DE ANALISIS#########################################################
###################################################################################################

## Histograma y densidad del metro.
with(movilidad, hist(metro, freq = FALSE,breaks="Sturges", col="lightblue", title='Histograma de 
                                                                                  dendsidad de la afluencia 
                                                                                  en el metro de CDMX'))
lines(density(movilidad$metro), col="blue")
## La mayoria de los días el descenso de afluencia que se ha presentado esta entre 40% y 50%
## Mientras que los descensos de aprox 70% se presentaron en cuento el gobierno puso en marcha 
# la cuarentena

## Histograma y densidad del metrobus.
with(movilidad, hist(metrobus, freq = FALSE, breaks="Sturges", col="orange",title='Histograma de 
                                                                                  dendsidad de la afluencia 
                                                                                  en el metrobus de CDMX'))
lines(density(movilidad$metrobus), col="red", lty=3, lwd=4)
## Los descensos mas frecuentes estan entre 40 y 50% 

## Histograma y densidad del ecobici.
with(movilidad, hist(ecobici, freq = FALSE, breaks="Sturges", col="lightgreen",title='Histograma de 
                                                                                  dendsidad de la afluencia 
                                                                                  en ecobici de CDMX'))
lines(density(movilidad$ecobici), col="green", lty=3, lwd=4)
# LOs descensos mas frecuentes estan entre 60 y 70%

## Histograma y densidad del rtp.
with(automoviles, hist(transito, freq = FALSE, breaks="Sturges", col="gray"))
lines(density(automoviles$transito), col="black", lty=3, lwd=4)
# Los descensos mas frecuentes estan entre 40% y 60%, 

###################################################################################################
######################### SEIES DE TIEMPO #########################################################
###################################################################################################

####### Serie de tiempo de metro
metro_p <-ggplot(movilidad)+geom_line(aes(x=fecha,y=metro), color="orange", size=0.5)+
  geom_point(aes(x=fecha,y=metro), size=0.5)+
  ggtitle("Serie de tiempo afluencia en el metro")+
  labs(x="Tiempo", y="metro")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
metro_p <- ggplotly(metro_p)

####### Serie de tiempo de metrobus
metrobus_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=metrobus), color="red", size=0.5)+
  geom_point(aes(x=fecha,y=metrobus), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en el metrobus")+
  labs(x="Tiempo", y="metrobus")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
metrobus_p <- ggplotly(metrobus_p)

####### Serie de tiempo de ecobici
ecobico_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=ecobici), color="green", size=0.5)+
  geom_point(aes(x=fecha,y=ecobici), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en ecobici")+
  labs(x="Tiempo", y="ecobici") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ecobico_p <- ggplotly(ecobico_p)

####### Serie de tiempo de autos
autos_p<-ggplot(automoviles)+geom_line(aes(x=fecha,y=transito), color="purple", size=0.5)+
  geom_point(aes(x=fecha,y=transito), size=0.5)+
  ggtitle("Series de tiempo de afluencia en transportes públicos y privados.", 
          subtitle = 'Valores en diferencias porcentuales')+
  labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="automovil") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
autos_p <- ggplotly(autos_p)

fig <- subplot(metro_p, metrobus_p, ecobico_p, autos_p, 
               nrows = 4, shareX = TRUE, margin = 0.05, titleY = TRUE)
fig

####### Comentarios de las series de tiempo ########

#######################################PARA SHINY ##################################
#######################################PARA SHINY ##################################
#######################################PARA SHINY ##################################
metro_p <-ggplot(movilidad)+geom_line(aes(x=fecha,y=metro), color="orange", size=0.5)+
  geom_point(aes(x=fecha,y=metro), size=0.5)+
  ggtitle("Serie de tiempo afluencia en el metro", 
          subtitle = 'Desde 24/marzo/2020 al 25/enero/2021')+
  labs(x="Tiempo", y="diferencias porcentuales")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(metro_p)

####### Serie de tiempo de metrobus
metrobus_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=metrobus), color="red", size=0.5)+
  geom_point(aes(x=fecha,y=metrobus), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en el metrobus", 
          subtitle = 'Desde 24/marzo/2020 al 25/enero/2021')+
  labs(x="Tiempo", y="diferencias porcentuales")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(metrobus_p)

####### Serie de tiempo de ecobici
ecobico_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=ecobici), color="green", size=0.5)+
  geom_point(aes(x=fecha,y=ecobici), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en ecobici", 
          subtitle = 'Desde 24/marzo/2020 al 25/enero/2021')+
  labs(x="Tiempo", y="diferencias porcentuales") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(ecobico_p)

####### Serie de tiempo de autos
autos_p<-ggplot(automoviles)+geom_line(aes(x=fecha,y=transito), color="purple", size=0.5)+
  geom_point(aes(x=fecha,y=transito), size=0.5)+
  ggtitle("Series de tiempo de afluencia en vehículos particulares.", 
          subtitle = 'Desde 24/marzo/2020 al 25/enero/2021')+
  labs(x="Tiempo", y="diferencias porcentuales") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(autos_p)

#######################################PARA SHINY ##################################
#######################################PARA SHINY ##################################
#######################################PARA SHINY ##################################


############################ MODELOS  Y SERIES DE TIEMPO ############################### 
#### metro
ts_metro <- ts(movilidad$metro, start = c(2020, 3,24), 
               end = c(2021, 1, 25), frequency = 307)
metro_fit <- auto.arima(ts_metro)
pronostico_metro<-forecast(metro_fit,12,level=95)
pronos_metro <- plot(pronostico_metro,main="Pronóstico para afluencia en el metro de CDMX.")
Box.test(residuals(metro_fit), type = 'Ljung-Box')  ## p-value = 0.9735


### metrobus
ts_metrobus <- ts(movilidad$metrobus, start = c(2020, 3,24), 
               end = c(2021, 1, 25), frequency = 307)
metrobus_fit <- auto.arima(ts_metrobus)
pronostico_metrobus <- forecast(metrobus_fit,12,level=95)
plot(pronostico_metrobus,main="Pronóstico con auto.arima")
Box.test(residuals(metrobus_fit), type = 'Ljung-Box')  ## p-value = 0.3256
ggtsdiag(metrobus_fit)

ts_metrobus_m <- ma(ts_metrobus, 3)
metrobus_arima <- auto.arima(ts_metrobus_m)
Box.test(residuals(metrobus_arima), type = 'Ljung-Box') ## ma = 3, p-value = 0.8823
pronostico_metrobus <- forecast(metrobus_arima,12,level=95)
pronos_metrobus <- plot(pronostico_metrobus,main="Pronóstico para afluencia en el metrobús.")


### ecobici
ts_ecobici <- ts(movilidad$ecobici, start = c(2020, 3,24), 
                  end = c(2021, 1, 25), frequency = 307)
ecobici_fit <- auto.arima(ts_ecobici)
summary(ecobici_fit)
pronostico<-forecast(ecobici_fit,12,level=95)
plot(pronostico,main="Pronóstico con auto.arima")
Box.test(residuals(ecobici_fit), type = 'Ljung-Box')  ## p-value = 0.2678
ggtsdiag(ecobici_fit)

ts_ecobici_m <- ma(ts_ecobici, 3)
ecobici_arima <- auto.arima(ts_ecobici_m)
Box.test(residuals(ecobici_arima), type = 'Ljung-Box') ## ma = 2, p-value = 0.2999
                                                       ## ma = 3, p-value = 0.5439
pronostico_ecobici <- forecast(ecobici_arima,12,level=95)
pronos_ecobici <- plot(pronostico_ecobici,main="Pronóstico para afluencia en ecobici.")


### automovil
ts_autos <- ts(automoviles$transito, start = c(2020, 3,24), 
                 end = c(2021, 1, 25), frequency = 307)
autos_fit <- auto.arima(ts_autos)
summary(autos_fit)
pronostico<-forecast(autos_fit,12,level=95)
plot(pronostico,main="Pronóstico con auto.arima")
Box.test(residuals(autos_fit), type = 'Ljung-Box')  ## p-value = 0.9489

pronostico_autos <- forecast(autos_fit,12,level=95)
pronos_autos <- plot(pronostico_autos,main="Pronóstico para afluencia de autos particulares.")


par(mfrow=c(2,2))  
plot(pronostico_metro,main="Pronóstico para afluencia en el metro")
plot(pronostico_metrobus,main="Pronóstico para afluencia en metrobús.")
plot(pronostico_ecobici,main="Pronóstico para afluencia en ecobici.")
plot(pronostico_autos,main="Pronóstico para afluencia en autos.")
