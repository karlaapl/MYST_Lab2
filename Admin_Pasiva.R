### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

###librerias
suppressMessages(library(plotly)) # 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 

suppressMessages(library(knitr))  
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("xNriyB3ufAxdRmZgpq_p")
df
# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {

  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

# Tickers de accciones y datos a solicitar a QUANDL
#tk <- c("TSLA", "BBY", "HD")
#tk <- c("PYPL", "GOOGL", "NKE"
tk <- c("PYPL", "TSCO", "NKE") #usandotesco, paypal y nike
cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2015-08-01","2016-08-01") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk
Port1 <- portfolio.spec(assets=tk)
# Agregando restricciones

Port1 <- add.constraint(portfolio=Port1, type="full_investment")
Port1 <- add.constraint(portfolio=Port1, type="box", min=c(0.01, 0.01, 0.01), max=c(0.7, 0.7, 0.7))
Port1 <- add.objective(portfolio = Port1, type = "return", name = "mean")
Port1 <- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method = "random", trace = TRUE, search_size =5000)
Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))

for (i in 1:length(Port1$random_portfolio_objective_results)){
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights #para indexar listas se utliza [[]], ese elemento del portafolio se va creando desde el for
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  Portafolios[[i]]$Vars <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
  names(Portafolios[[i]]$Medias) <- NULL
}

# creamos un dataframe para poder almacenar todo tipo de valores 
  df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),ncol=3, data=0))
  colnames(df_Portafolios) <- c("Rend", "Var", "Clase")
  for (i in 1:length(Port1$random_portfolio_objective_results)){
    df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
    df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
    df_Portafolios$Clase[i] <- "No Frontera"
    for(k in 1:length(tk)){
      df_Portafolios[i,paste("Peso_",tk[k],sep="")] <- Portafolios[[i]]$Pesos[k]
      df_Portafolios[i,paste("Titulos_ini_", tk[k], sep="")] <-
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
  }
  }
  
  # grafica en el eje x la varianza, y, rend
  Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                              name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                              text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                            '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
    layout(title = "Portafolios (Markowitz)",
           xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                        showgrid = F),
           yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
           legend = list(orientation = 'h', y = -0.25))
  Plot_portafolios # hasta aqui se grafica
  
  # Se seleccionan los mejores portafolios que cumplan  con los diferentes requerimientos < var, > rendm y  >ratio Sharpe
  Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]
  # Portafolio con m?nima varianza
  Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),] # aunque dice var,son desviaciones
  # Tasa libre de riesgo
  rf <- 0.0253
  # Rendimiento de portafolio
  rp <- df_Portafolios$Rend
  # Varianza de portafolio
  sp <- df_Portafolios$Var
  # Indice de sharpe
  sharpe <- (rp-rf)/sp
  # Portafolio con m?ximo Sharpe ratio 
  Port_3 <- df_Portafolios[which.max(sharpe),]
  Ports <- cbind(rbind(Port_1, Port_2, Port_3),
                 "Portafolio" = c("Maximo Rendimiento","Minima Varianza","Maximo Sharpe Ratio"))
  Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                              name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                              text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                            '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
    layout(title = "Portafolios (Markowitz)",
           xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                        showgrid = F),
           yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
           legend = list(orientation = 'h', y = -0.25)) %>%
    add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
              mode = 'marker', marker = list(color="green", size=10)) %>%
    add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
              mode = 'marker', marker = list(color="blue", size=10)) %>%
    add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
              mode = 'marker', marker = list(color="orange", size=10))
  Plot_portafolios
  # Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
  Pesos_Titulos <- Ports[,-c(1,2,3)]
  # Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos m?s f?cil los t?tulos
  # por portafolio por activo
  Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos)) # la función de grep es buscar matches de argumentos 
  Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)
  # Crear data frame que contendr? los datos finales de cada estrategia
  for(i in 1:length(Ports[,1])) {
    Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
      (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
         Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
         Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
  }
  
  
  #####  Se utiliza esta parte más bien para series de tiempo
  
  plot_ly(Historicos_Ports) %>%
    add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'Máximo Rendimiento',
              line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
    add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'Mínima Varianza',
              line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
    add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'Máximo Sharpe Ratio',
              line = list(color = 'green'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
    layout(title = "3 Portafolios distintos objetivos",
           xaxis = list(title = "Fechas", showgrid = T),
           yaxis = list(title = "Balance"), 
           legend = list(orientation = 'h', y = -0.25, x = 0.5)) 
  
  # La función de este código es descargar los precios de 3 acciones, en este caso Tesco, Paypal y Nike.
  # El criterio de la elección de estas compañias es que las 3 empresas es que las 3 empresas han tenido un buen desempeño en la bolsa y tambien al ver sus estados financieros del último cuarto reportado, se ha visto un crecimiento en estas en empresas y una sólidad posición.  
  # 
  # Para formar los portafolios se utilizó el método random para la optimización del portafolio, despues de la optimización se crea un data frame para poder guardar pesos, número de títulos, rendimientos y desviación estándar de cada portafolio. 
  # Para continuar, se utiliza la paquetería de plotly para gráficar primero todos los portafolios armados, mostrando en el eje x la volatilidad y en el eje y el rendimiento. Se seleccionan los mejores 3 portafolios que cumplen con los criterios de mayor rendimiento, menor volatilidad y mayor ratio Sharpe.
  # Ratio Sharpe  es una medida de exceso de rentabilidad a comparación de un activo sin riesgo, en este caso comparando con la tasa de un año de EUA. Cuanto mayor sea, es mejor. 
  # Con  los activos utilizados , se obtuvo un 4.43% de rendimiento con 26.7% de volatilidad, este portafolio cumplió con  los criterios de mayor rendimiento y mayor ratio sharpe.  Este portafolio se forma con 70 % de Paypal,  1% de Tesc y 29% de Nike
  # Para el criterio de la menor volatilidad, se obtiene 2.32% de rendimiento con una volatilidad de 21.93%, para este portafolio se tiene una proporciónd e 15% Paypal, 59% Tesco y 33% Nike.
  # Personalmente creo que en los resultados obtenidos,  creo que vale más la pena el primer portafolio ya que duplica el rendimiento  a comparación del de mínima varianza y la diferencia de las varianzas de estos portafolios no es tan grande. 