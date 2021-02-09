## Créditos: Carlos Rodríguez Tenorio

## Proyecto
library(shinydashboard)
library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)
library(forecast)
library(ggfortify)
library(ggcorrplot)
library(tidyr)
library(scales)
library(datasets)
library(taRifx)
library(astsa)

dsdolar <- read.csv("/cloud/project/proy/dolar_maydic2020.csv") 
datosdolar2 <- rename(dsdolar, max = Máximo )
datosdolar2 <- rename(datosdolar2, min = Mínimo)
datosdolar2<- rename(datosdolar2, fecha = Fecha, var =X..var.  )

datosdolar2 <- mutate(datosdolar2, fecha = as.Date(fecha, "%d.%m.%Y"))



datosdolar2 <- mutate(datosdolar2, var = as.numeric(var))

dolarmin <- select(datosdolar2,  min, max, Cierre)
datosmut <- read.csv("/cloud/project/proy/dolar_cov", header = TRUE,
                     sep = ",", check.names = FALSE)

hospitalizados_transporte_movilidad <- read.csv("/cloud/project/proy/hospitalizados-transporte-movilidad.xls")
movilidad <- hospitalizados_transporte_movilidad[, c('fecha', 'metro', 
                                                     'metrobus',
                                                     'trolebus',
                                                     'rtp', 
                                                     'ecobici')]
movilidad <- mutate(movilidad, fecha = as.Date(fecha, '%d/%m/%Y'))
month<- strftime(movilidad$fecha,"%m")
movilidad2<-movilidad %>% mutate(month)

automoviles <- hospitalizados_transporte_movilidad[, c('fecha', 'transito')]
automoviles <-mutate(automoviles, fecha = as.Date(fecha, '%d/%m/%Y'))

data<-read.csv("/cloud/project/proy/data.csv")
data<-as.data.frame(data)
daf<-as.data.frame(read.csv("/cloud/project/proy/match.data.csv"))

datos <- read.csv("/cloud/project/proy/monitoreo.csv", header = TRUE, 
                  sep = ",", check.names = FALSE)
datos <- mutate(datos, Fecha = as.Date(Fecha, "%m/%d/%Y"))
df <- datos %>%
  select(Fecha, Proveedor, Trafico_Datos_Local)

#Separamos los datos de la companias mas "Influyentes".
list_of_values <- c("CLARO", "MOVISTAR", "UNE", "DIRECTV")
filtered <- filter(datos, Proveedor %in% list_of_values)
movistar <- filter(datos, Proveedor == "MOVISTAR")
claro <- filter(datos, Proveedor == "CLARO")
une <- filter(datos, Proveedor == "UNE")
directv <- filter(datos, Proveedor == "DIRECTV")


#Esta parte es el análogo al ui.R
ui <-  fluidPage(
      
  tags$head(
        tags$link(rel = "stylesheet", type= "text/css", href="bootstrap.min.css")
      ),
  
  titlePanel(img(src='titulo3.png', height=90)),
  
  
        dashboardPage(
  skin="red",
            dashboardHeader(title = "Contenido"),
            
            dashboardSidebar(
                
                sidebarMenu(
                  menuItem("EL RETO", tabName = "reto", icon = icon("file-picture-o")),
                    menuItem("SOBRE LA MOVILIDAD", tabName = "movilidad", icon = icon("dashboard")),
                  menuItem("SERIES DE TIEMPO - MOVILIDAD", tabName = "series", icon = icon("file-picture-o")),
                  menuItem("PRONÓSTICOS DE MOVILIDAD", tabName = "pronos", icon = icon("file-picture-o")),
                    menuItem("SOBRE EL TRÁFICO DE INTERNET", tabName = "redes", icon = icon("table")),
                  menuItem("SERIES DE TIEMPO - INTERNET", tabName = "seriesinternet", icon = icon("file-picture-o")),
                  menuItem("PRONÓSTICOS DE INTERNET", tabName = "seriespronos", icon = icon("file-picture-o")),
                  menuItem("SOBRE EL DOLAR", tabName = "dols", icon = icon("file-picture-o"))
                  
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                  tabItem(tabName = "reto",
                          titlePanel("BEDU - Fase II 'Programación y Estadística con R'"), 
                          titlePanel(h3(" \n\n")),
                          titlePanel(h3("En esta página se presentan algunas gráficas interactivas, las cuales se desarrollaron como parte del proyecto del módulo 2 del programa Data Science de BEDU. En las pestañas de la izquierda se indican los diferentes módulos con los cuales se puede interactuar y en la última pestaña se presentan las conclusiones del proyecto. La descripción detallada del proyecto se encuentra en el siguiente link: https://rpubs.com/diego-corona/Proyecto_BEDU_RStudio ")),
                          img( src = "integrran.png", 
                               height = 300)
                          
                          
                  ),
                  tabItem(tabName = "series",
                          titlePanel("SERIES DE TIEMPO - MOVILIDAD"),
                          titlePanel(h3("A continuación se presentan las series de tiempo relacionadas con la movilidad en CDMX. Del lado izquierdo se indican aquellas relacionadas con el transporte público, mientras que del lado derecho se presenta la serie de tiempo a partir de la afluencia de vehículos públicos y privados.")),
                          selectInput("modo", " Modo de transporte ",
                                      choices = c("Metro", "metrobus", "ecobici")),
                          box(title = "Series de tiempo", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plot2", height = 300)
                          ),
                          box(title = "Series de tiempo", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plot3", height = 300)
                          )
                  ),
                  
                  
                  
                    # Histograma
                    tabItem(tabName = "movilidad",
                            
                            titlePanel("Movilidad en CDMX"),
                            titlePanel(h3("Interactuando con las gráficas sobre algunos modos de transporte de Ciudad de México:")),
      
                            selectInput("x", " Modo de transporte ",
                                        choices = c("Metro", "metrobus", "ecobici")),
                            selectInput("plot_type", "Tipo de Gráfica", 
                                        c("Gráfica de dispersión" = "Scatter", 
                                          "Histograma" = "histogram")) ,
                           
                                
                                box(title = "Gráfica", status = "primary", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput("plot1", height = 300)
                                    ),
                            titlePanel(h4("En las gráficas se observan datos que se recopilaron para diferentes modos de transporte, considerando como fecha inicial el 24 de Marzo de 2020 y terminando el 25 de Enero de 2021.
                                          
                                          El histograma muestra el comportamiento de los usuarios en forma cronológica; por otro lado la gráfica de dispersión muestra la agrupación de los cambios porcentuales por mes. De esta forma se observó que durante los primeros meses de la pandemia, los servicios presentaron una baja en la demanda; sin embargo, conforme se desarrolló la pandemia estos comenzaron a tener una mayor demanda para finalmente en Enero nuevamente mostrar una baja como resultado del semáforo rojo en CDMX.")),
                            box(title = "Controles", status = "success", solidHeader = TRUE,
                                collapsible = TRUE,
                                sliderInput("bins", "Modifica el intervalo para el histograma", 1, 30, 5),
                                sliderInput("bins2", "Modifica el rango de graficación (Cambios porcentuales)", -100, 0, 0)
                            )
                            
                                      
                                                  
                            
                            
                                
                                
                            
                    ),
                  
                  tabItem(tabName = "seriesinternet",
                          titlePanel("SERIES DE TIEMPO - INTERNET"),
                          titlePanel(h3("A continuación se presentan las series de tiempo para cada una de las 4 compañías analizadas con el propósito de facilitar la comparación de tráfico de internet por empresa.")),
                          selectInput("empresa1", " Selecciona la compañía 1 a comparar: ",
                                      choices = c("Movistar", "Claro", "DirecTV", "UNE")),
                          selectInput("empresa2", " Selecciona la compañía 2 a comparar: ",
                                      choices = c("Claro", "Movistar", "DirecTV", "UNE")),
                          box(title = "Series de tiempo", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plot9", height = 300)
                          ),
                          box(title = "Series de tiempo", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plot10", height = 300)
                          )
                  ),
                    
                    # PRONOSTICOS DE SERIES DE TIEMPO MOVILIDAD
                    tabItem(tabName = "pronos",
                            
                                titlePanel("PRONÓSTICOS DE MOVILIDAD"),
                                titlePanel(h3("A partir de los análisis de series de tiempo, se realizaron los pronósticos para la movilidad en CDMX. ¿Cuál será el comportamiento para cada modo de transporte? ¿Cómo será la afluencia de vehículos particulares en los días posteriores al 25 de Enero de 2021?")),
                            selectInput("modo_pron", " Modo de transporte / Afluencia de vehículos particulares ",
                                        choices = c("Metro", "metrobus", "ecobici", "Afluencia de vehículos")),
                            box(title = "Series de tiempo", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("plot4", height = 300)
                            ),
                            box(title = "Prediciendo para una fecha específica", status = "success", solidHeader = TRUE,
                                collapsible = TRUE,
                                titlePanel(h4("¿Qué pasa si modifico la fecha de predicción de datos? A continuación se presenta el slider con el cual es posible cambiar la cantidad de días posteriores sobre los cuales se calculará el pronóstico.")),
                                sliderInput("perio", "Días de pronóstico", 1, 30, 12)
                                
                                
                            )
                            
                                
                            
                    ),
                    
                    
                    
                    tabItem(tabName = "redes",
                            titlePanel("SOBRE EL TRÁFICO EN INTERNET"),
                            titlePanel(h3("Se analizaron los datos para cuatro compañías de internet: Claro, Directv, Movistar y UNE. Del lado izquierdo se puede seleccionar entre la gráfica de dispersión o el scatterplot de los datos a través del tiempo; mientras que del lado derecho se observan los bloxplots de la distribución de los datos para cada empresa.")),
                            selectInput("tipgraf", "Tipo de Gráfica", 
                                        c("Gráfica de dispersión" = "dispee", 
                                          "Gráfica de areas" = "areea")) ,
                            box(title = "Tráfico de datos", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("plot6", height = 300)
                            ),
                            box(title = "Boxplot por empresa", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("plot7", height = 300)
                            )
                            )
                    , 
                    
                  # PRONOSTICOS DE SERIES DE TIEMPO DE INTERNET
                  tabItem(tabName = "seriespronos",
                          
                          titlePanel("PRONÓSTICOS PARA EL TRÁFICO DE INTERNET"),
                          titlePanel(h3("A partir de los análisis de series de tiempo, se realizaron los pronósticos para cada una de las compañías. ¿Cuál será la demanda de datos móviles en los próximos días para cada una de ellas?")),
                          selectInput("var1", " Compañía ",
                                      choices = c("Movistar", "Claro", "DirecTV", "UNE")),
                          box(title = "Pronóstico", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plot8", height = 300)
                          ),
                          
                          box(title = "Prediciendo para una fecha específica", status = "success", solidHeader = TRUE,
                              collapsible = TRUE,
                              titlePanel(h4("¿Qué pasa si modifico la fecha de predicción de datos? A continuación se presenta el slider con el cual es posible cambiar la cantidad de días posteriores sobre los cuales se calculará el pronóstico.")),
                              sliderInput("perio2", "Días de pronóstico", 1, 30, 12))
                          
                          
                          
                          
                          
                  ),
                  
                    tabItem(tabName = "dols",
                            
                            titlePanel("SOBRE EL DÓLAR Y SU PRONÓSTICO"),
                                titlePanel(h3("A continuación se muestra la fluctuación del dólar en sus valores máximos (en color azul) y mínimos alcanzados por día (en color rojo). ¿Cuál se espera que sea su comportamiento para los siguientes días?")),
                            box(title = "Fluctuación", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("plot12", height = 300)
                            ),
                            box(title = "Pronóstico", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("plot13", height = 300)
                            ) ,
                            box(title = "Prediciendo para una fecha específica", status = "success", solidHeader = TRUE,
                                collapsible = TRUE,
                                titlePanel(h4("¿Qué pasa si modifico la fecha de predicción de datos? A continuación se presenta el slider con el cual es posible cambiar la cantidad de días posteriores sobre los cuales se calculará el pronóstico.")),
                                sliderInput("perr", "Días de pronóstico", 1, 30, 12))
                            
                            
                    
                            
                            
                    )
                            
                    )
                    
                )
            )
        )
    

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    
    
    
    #Gráfico de Barras
    output$plot1 <- renderPlot({
      if (input$x == "ecobici") {
        if (input$plot_type == "histogram") {
          ggplot(movilidad, aes(ecobici)) + geom_histogram(binwidth = input$bins,fill = "#FFDB6D", color = "#C4961A")+ xlab("Ecobici")+ ylab("Frecuencia")
          
        }
        else {
          ggplot(movilidad2%>%filter(ecobici<=input$bins2), aes(month, ecobici, color=month)) + geom_point()+ xlab("Mes")+ ylab("Cambios porcentuales")
          
        }
      }  
      else if (input$x == "metrobus")  {
        if (input$plot_type == "histogram") {
        ggplot(movilidad, aes(metrobus)) + geom_histogram(binwidth = input$bins,fill = "#00ABFD", color = "#00798c")+ xlab("Metrobus")+ ylab("Frecuencia")
        }
        else{
          ggplot(movilidad2%>%filter(metrobus<=input$bins2), aes(month, metrobus, color=month)) + geom_point()+ xlab("Mes")+ ylab("Cambios porcentuales")
          
        }
      }      
      else {
        if (input$plot_type == "histogram") {
        ggplot(movilidad, aes(metro)) + geom_histogram(binwidth = input$bins,fill = "#fc717F", color = "#d1495b")+ xlab("Metro")+ ylab("Frecuencia")
        }
        else {
          ggplot(movilidad2%>%filter(metro<=input$bins2), aes(month, metro, color=month)) + geom_point()+ xlab("Mes")+ ylab("Cambios porcentuales")
          
        }
          
      }
      
        
        
    })
    
    output$plot2 <- renderPlot({
      if (input$modo=="Metro"){
        ggplot(movilidad)+geom_line(aes(x=fecha,y=metro), color="orange", size=0.5)+
          geom_point(aes(x=fecha,y=metro), size=0.5)+
          ggtitle("Serie de tiempo afluencia en el metro", 
                  subtitle = 'Desde 24/marzo/2020 al 25/enero/2021')+
          labs(x="Tiempo", y="diferencias porcentuales")+ theme_bw()+theme(plot.title = element_text(hjust = 0.5))
      }
      else if (input$modo=="metrobus"){
        ggplot(movilidad)+geom_line(aes(x=fecha,y=metrobus), color="red", size=0.5)+
          geom_point(aes(x=fecha,y=metrobus), size=0.5)+
          ggtitle("Serie de tiempo de afluencia en el metrobus")+
          labs(x="Tiempo", y="metrobus")+
          theme_bw()+theme(plot.title = element_text(hjust = 0.5))
      }
      else{
        ggplot(movilidad)+geom_line(aes(x=fecha,y=ecobici), color="green", size=0.5)+
          geom_point(aes(x=fecha,y=ecobici), size=0.5)+
          ggtitle("Serie de tiempo de afluencia en ecobici")+
          labs(x="Tiempo", y="ecobici") +
          theme_bw()+theme(plot.title = element_text(hjust = 0.5))
      }
      
    })
      
      output$plot3<- renderPlot({
        
        ggplot(automoviles)+geom_line(aes(x=fecha,y=transito), color="purple", size=0.5)+
          geom_point(aes(x=fecha,y=transito), size=0.5)+
          ggtitle("Series de tiempo de afluencia en transportes públicos y privados.", 
                  subtitle = 'Valores en diferencias porcentuales')+
          labs(x="Tiempo desde 24/marzo/2020 al 25/enero/2021", y="automovil") +
          theme_bw()+theme(plot.title = element_text(hjust = 0.5))
      })
    
      output$plot4 <- renderPlot({
        if (input$modo_pron=="Metro"){
          
          ts_metro <- ts(movilidad$metro, start = c(2020, 3,24), 
                         end = c(2021, 1, 25), frequency = 307)
          metro_fit <- auto.arima(ts_metro)
          pronostico<-forecast(metro_fit,input$perio,level=95)
          plot(pronostico,main="Pronóstico con auto.arima para el metro")
          
          }
        else if (input$modo_pron=="metrobus"){
          ts_metrobus <- ts(movilidad$metrobus, start = c(2020, 3,24), 
                            end = c(2021, 1, 25), frequency = 307)
          metrobus_fit <- auto.arima(ts_metrobus)
          pronostico<-forecast(metrobus_fit,input$perio,level=95)
          plot(pronostico,main="Pronóstico con auto.arima para el Metrobús")
        }
        else if (input$modo_pron=="ecobici"){
          ts_ecobici <- ts(movilidad$ecobici, start = c(2020, 3,24), 
                           end = c(2021, 1, 25), frequency = 307)
          ecobici_fit <- auto.arima(ts_ecobici)
          pronostico<-forecast(ecobici_fit,input$perio,level=95)
          plot(pronostico,main="Pronóstico con auto.arima para Ecobici")
        }
        else{
          ts_autos <- ts(automoviles$transito, start = c(2020, 3,24), 
                         end = c(2021, 1, 25), frequency = 307)
          autos_fit <- auto.arima(ts_autos)
          pronostico<-forecast(autos_fit,input$perio,level=95)
          plot(pronostico,main="Pronóstico con auto.arima para afluencia de vehículos particulares")
          
        }
        
      })
      
      output$plot6 <- renderPlot({
        
        if (input$tipgraf=="areea"){
        
        
        ggplot(filtered, aes(x = Fecha, y = Trafico_Datos_Local)) +
          geom_area(aes(color = Proveedor, fill = Proveedor),
                    alpha = 0.5, position=position_dodge(0.8)) +
          ggtitle("Trafico de datos durante la pandemia") +
          xlab("Mes 2020") +
          ylab("Datos en GB") +
          theme_minimal() +
          scale_color_manual(values=c("#00AFBB", "#E7B800", "#CC0000", "#006600", 
                                      "#669999", "#00CCCC", "#660099", "#CC0066", 
                                      "#FF9999", "#FF99FF", "#559955", "#A990CC", 
                                      "#660099", "#CC0066")) +
          scale_fill_manual(values=c( "#00AFBB", "#E7B800", "#CC0000", "#006600", 
                                      "#669999", "#00CCCC", "#660099", "#CC0066", 
                                      "#FF9999", "#FF99FF", "#559955", "#A990CC", 
                                      "#660099", "#CC0066"))
      }
      else {
        
        ggplot(filtered, aes(x=Fecha, y=Trafico_Datos_Local, colour = Proveedor)) +
          ggtitle("Trafico de datos durante la pandemia")+ 
          xlab("Mes")+
          ylab("Datos en GB") +
          geom_point() +
          theme_minimal() 
      }
      })
      
      output$plot7 <- renderPlot({
        ggplot(filtered, aes(x=Proveedor, y=Trafico_Datos_Local, color=Proveedor)) +
          ggtitle("Trafico de datos durante la pandemia")+ 
          xlab("Mes")+
          ylab("Datos en GB") +
          geom_boxplot() +
          theme_light()
        
      })
      
      
      output$plot9 <- renderPlot({
        if (input$empresa1=="Movistar"){
          ggplot(movistar) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="purple", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Movistar") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
          
        }
        else if (input$empresa1=="Claro"){
          ggplot(claro) + geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="red", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Claro") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else if (input$empresa1=="UNE"){
          ggplot(une) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="blue", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Unefon") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else{
          ggplot(directv) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="green", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Directv") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        
      })
      
      
      output$plot10 <- renderPlot({
        if (input$empresa2=="Movistar"){
          ggplot(movistar) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="purple", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Movistar") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
          
        }
        else if (input$empresa2=="Claro"){
          ggplot(claro) + geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="red", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Claro") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else if (input$empresa2=="UNE"){
          ggplot(une) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="blue", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Unefon") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else{
          ggplot(directv) +
            geom_line(aes(x=Fecha, y= Trafico_Datos_Local), color="green", size=0.8) +
            geom_point(aes(x=Fecha, y= Trafico_Datos_Local), size=1) +
            ggtitle("Directv") +
            labs(x="Tiempo", y="Datos (GB)") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
        }
        
      })
      
      output$plot8 <- renderPlot({
        if (input$var1=="Movistar"){
          ts_movistar <- ts(movistar$Trafico_Datos_Local, start = c(2020,3,30),
                           end = c(2021,1,26), frequency = 305)
          ma_ts_movistar <- ma(ts_movistar,2)
          modelo_arima_movistar <- auto.arima(ma_ts_movistar)
          prediccion_movistar <- forecast(modelo_arima_movistar,input$perio2,level=95)
          plot(prediccion_movistar, main = "Movistar ARIMA", xlab="Tiempo", ylab = "Datos (GB)")
          
          
        }
        else if (input$var1=="Claro"){
          ts_claro <- ts(claro$Trafico_Datos_Local, start = c(2020,3,30),
                        end = c(2021,1,26), frequency=305)
          ar <- Arima(ts_claro, order = c(3,2,2))
          prediccion_claro <- forecast(ar,input$perio2,level=95)
          plot(prediccion_claro, main = "Clarovideo ARIMA", xlab="Tiempo", ylab = "Datos (GB)")
          
        }
        else if (input$var1=="UNE"){
          ts_unefon <- ts(une$Trafico_Datos_Local,start = c(2020,3,30),
                         end = c(2021,1,26), frequency=305)
          
          modelo_arima_unefon <- auto.arima(ts_unefon)
          prediccion_unefon <- forecast(modelo_arima_unefon, input$perio2, level=95)
          plot(prediccion_unefon, main = "Unefon ARIMA", xlab="Tiempo", ylab = "Datos (GB)")
        }
        else{
          ts_directv <- ts(directv$Trafico_Datos_Local, start = c(2020,3,30),
                          end = c(2021,1,26), frequency=305)
          modelo_arima_directv <- auto.arima(ts_directv, seasonal=TRUE)
          prediccion_directv <- forecast(modelo_arima_directv, input$perio2, level=95)
          plot(prediccion_directv, main = "Directv ARIMA", xlab="Tiempo", ylab = "Datos (GB)")
          
        }
        
      })
      
      output$plot12 <- renderPlot({
      
        ggplot(datosdolar2, aes(x=fecha, y=max)) + geom_line(color="blue") + 
          labs(x = "Tiempo", 
               y = "Valor mínimo y máximo respecto al peso mexicano.",
               title = paste("Fluctuación del dolar")) +
          geom_line(aes(y = min), color = "red") +
          theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1),
                axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1))
        
      })
      
      output$plot13 <- renderPlot({
        cierre_order <- rev(datosdolar2$Cierre)
        serie <- ts(cierre_order,frequency = 207, start = c(2020, 03,20))
        serie1 <- ma(serie, 3)
        prueba1 <- auto.arima(serie1)
        pronos_dolar <- forecast(prueba1,input$perr,level=95)
        plot(pronos_dolar, main="Pronostico para el precio del dólar. ")
        
      })
      
    # Agregando el dataframe
    output$table <- renderTable({ 
        data.frame(daf)
    })
    
    #Agregando el data table
  
    
}


shinyApp(ui, server)