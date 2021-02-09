---
  title: "Proyecto de Estadística y Programación con R."

author:
  - "Integrantes:"
- "- Viridiana Escarzaga Solis"
- "- Carlos Sebastián Madrigal Rodríguez"
- "- Diego Armando Morales Corona"
- "- Carlos Rodríguez Tenorio"

date: "03 de febrero de 2021"
output: 
  html_document:
  prettydoc::html_pretty:
  theme: tactile
highlight: github
toc: true
toc_float: true
code_folding: hide
---
  
  
  ```{r, echo=FALSE}
htmltools::img(src = knitr::image_uri(file.path('/cloud/project/BEDU.jpg')), 
               alt = 'logo', 
               style = 'position:absolute; top:0; right:0; padding:10px;')
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE, 
  comment = ' '
)
```

```{r message=FALSE, warning=FALSE, echo=FALSE}
library(knitr)
library(prettydoc)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(forecast)
library(ggfortify)
library(rsconnect)
```
### Introducción.

A partir de los primeros meses de 2020, México ha visto el impacto generado por la pandemia mundial de SARS-COV2. El primer caso confirmado, en México, se registró el 28 de febrero de 2020. Pocos días después el Gobierno Mexicano declaró la llamada [_Jornada Nacional de Sana Distancia_](https://www.gob.mx/salud/documentos/sana-distancia), en la cuál invitaban a la población a permanecer en casa con el fin de evitar una rápida propagación del virus. 

Esta medida, como era de esperarse, tuvo gran impacto en la normalidad de la sociedad implicando problemas socio-económicos, por mencionar algunos: _pérdida de enpleo por parte de algunos sectores de la población o dificultad para ejercerlo, disminución en el salario, imortante disminución en el consumo de servicios, etc._

Dado lo anterior resultá interesante preguntarse: _¿Cómo se ha visto afectados algunos indicadores socieconomicos a lo largo de este periodo de tiempo?_. El presente trabajo busca estudiarlos y analizarlos, mediante el uso de técnicas de series de tiempo, así como pronosticar el comportamiento de estos.

### Datos.

Se buscaron diversas fuentes de información de las cuales obtener las series de tiempo:
  
  - Los indicadores de movilidad se extrajeron de el [_Portal de Datos Abiertos_](https://datos.cdmx.gob.mx/group/movilidad), en el cual se encontraban series de tiempo de las afluencias en diversos medios de tránsporte en CDMX, registradas desde finales de marzo de 2020 hasta finales de enero de 2021. 

Los datos de esta serie son diferencias porcentuales diarias respecto a lo definido como un _día típico_. La definición de este día típico se baso en un histórico desde 2018 para calcular la afuencia promedio para cada día de la semana ajustados con la afluencia registrada durante la primera quincena de marzo de 2020. [Más información](https://www.semovi.cdmx.gob.mx/tramites-y-servicios/transparencia/preguntas-frecuentes/preguntas-frecuentes-covid-19/movilidad-durante-la-emergencia-sanitaria-covid-19)

- Los indicadores de movilidad de los vehículos particulares [(obtenidos aquí)](https://datos.cdmx.gob.mx/dataset/diferencias-porcentuales-en-el-transito-vehicular-en-la-cdmx-tomtom/resource/cd8b65b8-8cbf-4166-91c8-d72ac7e09a71), se calculan de una manera similar a los indices de movilidad en el transporte, con la diferencia de que la alfuencia en un _día típico_ se calculó respecto a un histórico desde la segunga semana de enero y febrero de 2020 [Más información](https://www.semovi.cdmx.gob.mx/tramites-y-servicios/transparencia/preguntas-frecuentes/preguntas-frecuentes-covid-19/movilidad-durante-la-emergencia-sanitaria-covid-19) 

---------- INFO DE VIRI Y SEBASTIAN ------------
  
  ### Consideraciones.
  
  Dado que se va a estudiar sobre series de tiempo, es importante tener en cuenta que el ajuste de modelos debe generar, en los residuales, ruido blanco cuyas características son el hecho de tener media cero, varianza constante y que estos no esten altamente correlacionados.

Considerando esto en la mayoría de los modelos se ultilizara para métrica la [_Prueba de hípotesis de Box-Ljung_](https://itl.nist.gov/div898/handbook/pmc/section4/pmc4481.htm), cuyas hipótesis son: 
  
  $H_0$: _Ruido blanco en residuales_ vs $H_1$: _Ausencia de ruido blanco en residuales_

### Movilidad en CDMX (Tránsporte público).

#### Análisis exploratorio.

La base de datos cuenta con las diferencias porcentuales del metro, metrobús, trolebús, rtp y ecobici, los cuales son los medios de trásporte mas recurrentes en la Ciudad, estos pueden presentar correlación entre ellos, por lo cual prodríamos omitir alguno para evitar estudiar información muy parecida en las series. 

Consideraremos una alta correlación aquellos por encima de 0.8 en coeficiente de correlación. 

```{r}
hospitalizados_transporte_movilidad <- read.csv('/cloud/project/hospitalizados-transporte-movilidad.csv')
movilidad <- hospitalizados_transporte_movilidad[, c('fecha', 'metro', 
                                                     'metrobus',
                                                     'trolebus',
                                                     'rtp', 
                                                     'ecobici')]
movilidad <- mutate(movilidad, fecha = as.Date(fecha, '%d/%m/%Y'))
corr <- cor(movilidad[,-c(1)])
corr
```
Dado esto, omitimos la información tanto de _rtp_ como de _trolebús_.
Y vemos ahora, de manera gráfica la correlación entre las variables resultantes, notamos que hay correlaciones hasta de 0.79 entre ecobici y metrobús, pero consevaremos estas variables ya que el ecobici es una forma más rápida de transporte y la mayor preferencia de algunas personas. Enfocado al estudio de sus afluencias, es interesante también estudiar este comportamiento.


```{r}
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
```

A continuación se presentan histogramas y densidades obtenidas de los datos, notamos que para el metro, los descensos más frecuentes rondan en tre el 40% y 50%, también hay una segunda moda alrededor de 70%, estos resgitros sucedieron al inicio de la restricción social.

El metrobus presenta descensos, en frecuencia relativamente más variables a lo largo del tiempo, siendo las más frecuentes entre 70% y 50%.

Los descensos de afluencia en el uso de ecobici, tienen una mayor frecuencia entre 60% y 70%, siento este rango el más frecuente desde finales de marzo de 2020.

En general, logramos apreciar un descenso importante en la afluencia de estos medios de transporte, lo cuál ers de esperarse. 

```{r}
par(mfrow=c(2, 2))
## Histograma y densidad del metro.
with(movilidad, hist(metro, freq = FALSE,breaks="Sturges", col="lightblue", title='Histograma de 
       dendsidad de la afluencia en el metro de CDMX'))
lines(density(movilidad$metro), col="blue")

## Histograma y densidad del metrobus.
with(movilidad, hist(metrobus, freq = FALSE, breaks="Sturges", col="orange",title='Histograma de 
      dendsidad de la afluencia en el metrobús de CDMX'))
lines(density(movilidad$metrobus), col="red")

## Histograma y densidad del ecobici.
with(movilidad, hist(ecobici, freq = FALSE, breaks="Sturges", col="lightgreen",title='Histograma de 
      dendsidad de la afluencia en ecobici de CDMX'))
lines(density(movilidad$ecobici), col="green")
```

#### Series de tiempo.

Iniciemos un pequeño analisis de las respectivas series de estos datos:
  
  Se puede notar la gran disminución significativa durante los ultimos días de marzo alcanzando los mayores mínimos alrededor de finales de abril e inicio de mayo.

Es importante señalar que en todas las series se presentan datos atípicos en las fechas:
  
  - 16 de septiembre de 2020
- 2 de noviembre de 2020
- 16 de noviembre de 2020
- 25 de diciembre de 2020
- 1 de enero de 2021

Estas fechas son considerados días festivos, y el hecho que es descenso sea más significativo se puede explicar por las medidas del Gobierno estos días, cerrando ciertas líneas y estaciones. Se trabajara con estos datos más adelante.

```{r}
####### Serie de tiempo de metro
metro_p <-ggplot(movilidad)+geom_line(aes(x=fecha,y=metro), color="orange", size=0.5)+
  geom_point(aes(x=fecha,y=metro), size=0.5)+
  ggtitle("Serie de tiempo afluencia en el metro")+
  labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="diferencia porcentual")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(metro_p)
```

Notamos que el comportamiento de las series de el metro como de metrobús son bastante parecidas, notando una disminución significativa en el mes de enero por el metro, debido a el problema de incendios presentados en estas fechas  [click](https://www.eluniversal.com.mx/metropoli/se-registra-incendio-en-oficinas-del-metro-reportan-un-muerto).

```{r}
####### Serie de tiempo de metrobus
metrobus_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=metrobus), color="red", size=0.5)+
  geom_point(aes(x=fecha,y=metrobus), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en el metrobús")+
  labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="diferencia porcentual")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(metrobus_p)
```

La serie de la afluencia en ecobici presenta un comportamiento más variable a lo largo del tiempo, presenta sus máximas disminuciones entre abril y julio, lo cual puede causar problemas al momento del ajuste. 

```{r}
####### Serie de tiempo de ecobici
ecobico_p<-ggplot(movilidad)+geom_line(aes(x=fecha,y=ecobici), color="green", size=0.5)+
  geom_point(aes(x=fecha,y=ecobici), size=0.5)+
  ggtitle("Serie de tiempo de afluencia en ecobici")+
  labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="diferencia porcentual") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(ecobico_p)

```

#### Modelos y predicciones.

Es importante recordar que, al ajustar un modelo, los residuales de este deben comportarse como ruido blanco.

##### Metro

Para el metro, realizamos la serie de tiempo, y utilizamos la función _auto.arima_ para ajustar el mejor modelo posible a los datos. Para comprobar que es un buen modelo, hacemos el test Ljung-Box, obteniendo un p_value = 0.7773, por lo cual los residuales se comportan como ruido blanco, por lo que el modelo puede ser usados para predecir.

```{r}
#### metro
ts_metro <- ts(movilidad$metro, start = c(2020, 3,24), 
               end = c(2021, 1, 25), frequency = 365)
metro_fit <- auto.arima(ts_metro)
summary(metro_fit)
Box.test(residuals(metro_fit), type = 'Ljung-Box')
```
```{r}
pronostico<-forecast(metro_fit,12,level=95)
plot(pronostico,main="Pronóstico para afluencia en el metro de CDMX.")
```

NOTA: Las predicciones con series de tiempo con fiables a corto plazo pero van convergiendo a la media al paso de los periodos, por lo que pierden eficacia.

```{r, eval=FALSE}
sliderInput(inputId='periodo', label ='Elija el periódo de predicción',
            min=1, max=35, value = 12)
renderPlot(plot(forecast(metro_fit,input$periodo,level=95),main="Pronóstico para afluencia en el metro de CDMX."))
```

##### Metrobús

El procedimiento es analógo, este modelo tampoco presento problemas para ajustar un buen modelo para predecir, ya que su p_value = 0.4748, por lo que no se rechaza que los residuales se comporten como ruido blanco, y presento los siguientes resultados:
  
  ```{r}
ts_metrobus <- ts(movilidad$metrobus, start = c(2020, 3,24), 
                  end = c(2021, 1, 25), frequency = 365)
metrobus_fit <- auto.arima(ts_metrobus)
summary(metrobus_fit)

Box.test(residuals(metrobus_fit), type = 'Ljung-Box')  ## p-value = 0.4748
```
```{r}
pronostico<-forecast(metrobus_fit,12,level=95)
plot(pronostico,main="Pronóstico para afluencia en el metrobús de CDMX.")
```


De igual manera, se pueden apreciar las predicciones para este modelo, a doce días.

#### Ecobici.

El modelo inciar con el método _auto.arima_ arrojo los siguientes resultados, cabe recordar que el comportamiento de esta serie era mucho más variables que las dos pasadas:
  
  ```{r}
ts_ecobici <- ts(movilidad$ecobici, start = c(2020, 3,24), 
                 end = c(2021, 1, 25), frequency = 365)
ecobici_fit <- auto.arima(ts_ecobici)
summary(ecobici_fit)
Box.test(residuals(ecobici_fit), type = 'Ljung-Box')  ## p-value = 2.516e-05
```

Observamos que la función arrojo un modelo ARIMA(4,1,4), con cuatro componentes autorregresivos, 4 medias móviles y una diferencia, sin embargo notamos que la prueba _Box-Ljung_ arrojo un p_value = 2.516e-05, por lo que los residuales de este modelo no se comportan como ruido blanco, y sería conveniente analizarlos:
  
  Podemos notar que la media relativamente no tiene nungún problema pero la varianza se mantiene muy variables. 

```{r}
plot(residuals(ecobici_fit))
```

El ajuste de esta serie se solucionó suavizando los datos atípicos mencionados anteriormente, sustituyendolo por el promedio entre el valor del día anterior y el siguinete a la observación. Dado que estos datos estan lejanos a las fechas de predicción el hecho de sustituirlos no presentará un gran problema a los datos.

También se aplicó una diferencia adicional al modelo inicial, ajustando un ARIMA(4,2,4), con dos diferencias, 4 componentes autorregresivos y 4 medias móviles. Se obtuvieron los siguientes resultados. 

Dado que con este modelo se obtuvo un p-value = 0.7053, los residuales del mismo se comportan como ruido blanco, por lo que el moodelo se ajusta bien a los datos.

```{r}
movilidad_2 <- movilidad
movilidad_2[movilidad_2$fecha == '2020-09-16', c(4)] <- 0.5*(movilidad_2[movilidad_2$fecha == '2020-09-15', c(4)] + movilidad_2[movilidad_2$fecha == '2020-09-17', c(4)]) 

movilidad_2[movilidad_2$fecha == '2020-11-02', c(4)] <- 0.5*(movilidad_2[movilidad_2$fecha == '2020-11-01', c(4)] + movilidad_2[movilidad_2$fecha == '2020-11-03', c(4)]) 

movilidad_2[movilidad_2$fecha == '2020-11-16', c(4)] <- 0.5*(movilidad_2[movilidad_2$fecha == '2020-11-15', c(4)] + movilidad_2[movilidad_2$fecha == '2020-11-17', c(4)])

movilidad_2[movilidad_2$fecha == '2020-12-25', c(4)] <- 0.5*(movilidad_2[movilidad_2$fecha == '2020-12-24', c(4)] + movilidad_2[movilidad_2$fecha == '2020-12-26', c(4)])

movilidad_2[movilidad_2$fecha == '2021-01-01', c(4)] <- 0.5*(movilidad_2[movilidad_2$fecha == '2020-12-30', c(4)] + movilidad_2[movilidad_2$fecha == '2021-01-02', c(4)])


ts_ecobici1 <- ts(movilidad_2$ecobici, start = c(2020, 3,24), 
                  end = c(2021, 1, 25), frequency = 365)
ar <- Arima(ts_ecobici1, order = c(4,2,4))
summary(ar)
Box.test(residuals(ar), type = 'Ljung-Box') #### p-value = 0.7053
```

```{r}
pronostico<-forecast(ar,12,level=95)
plot(pronostico,main="Pronóstico para afluencia en ecobici en CDMX.")
```

### Movilidad en CDMX (Tránsporte particular).

Es interesante igualmente, estudiar el comportamiento de los automoviles particulares en la CDMX, ya que es una de las manera de movílidad más comunes, y por ende, podría ser el más itulizado durante el periodo de pandemia.

#### Análisis exploratorio.

La diferencias porcentuales con mayor frecuencia para el uso de autos partuculares ronda, mayormente, en descensos entre 40% y 60%, se presentaron también descensos de 80% en movilidad de vehículos, podría ser explicado por la medidas de _Hoy no círcula_, implementadas por el Gobierno ya que, este programa se extendío a todos los coches para finales de abril [(Más información).](https://www.wibe.com/blog/viajero/hoy-no-circula-fase-3-covid/#:~:text=Desde%20el%2023%20de%20abril,los%20autos%20el%C3%A9ctricos%20o%20h%C3%ADbridos.)

```{r}
automoviles <- hospitalizados_transporte_movilidad[, c('fecha', 'transito')]
automoviles <- mutate(automoviles, fecha = as.Date(fecha, '%d/%m/%Y'))
with(automoviles, hist(transito, freq = FALSE, breaks="Sturges", col="gray"))
lines(density(automoviles$transito), col="black", lty=3, lwd=4)
```

#### Serie de tiempo.

Esta serie de tiempo ha presentado un comportamiento muy similar a las tres anteriores, presentando sus mayores descensos entre abril y julio, y teniendo una tendencia creciente a partir de este mes antes mencionado.

Los putno atípicos se presnetan en las mismas fechas que las series anteriores, es decir, en los días festivos.

Presenta también un gran descenso a finales de diciembre y principios de enero.

```{r}
autos_p<-ggplot(automoviles)+geom_line(aes(x=fecha,y=transito), color="purple", size=0.5)+
  geom_point(aes(x=fecha,y=transito), size=0.5)+
  ggtitle("Series de tiempo de afluencia en transportes particulares.")+
  labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="automovil") +
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
ggplotly(autos_p)
```

#### Modelos y predicciones.

El ajuste de esta serie de tiempo dio los siguientes resultados:
  
  - El mejor modelo ajustado es un ARIMA(5,1,2), con 5 componenetes autorregresivos, una diferencia y dos componenetes de medias móviles.
- EL p_value de la prueba Box-Ljung dio 0.9421, por lo que los residuales presentan un comportamiento de ruido blanco, y se puede decir que es un buen modelo, por lo que se puede utilizar para predicciones.

```{r}
ts_autos <- ts(automoviles$transito, start = c(2020, 3,24), 
               end = c(2021, 1, 25), frequency = 365)
autos_fit <- auto.arima(ts_autos)
summary(autos_fit)
Box.test(residuals(autos_fit), type = 'Ljung-Box')
```

```{r}
pronostico<-forecast(autos_fit,12,level=95)
plot(pronostico,main="Pronóstico para afluencia de vehículos particulares en CDMX")
```


