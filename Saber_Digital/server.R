library(readxl)
require(plyr)
require(rCharts)
require(reshape2)
require(devtools)
library(radarchart)
library(rpivotTable)
library(ggplot2)

options(RCHART_LIB = 'polycharts')

shinyServer(
  function(input, output, session) {

  # Cargar las bases de datos
    #Aprendizaje
      #Dias Diez
    Dias_Diez <- read_excel("data/Dias_Diez.xlsx")
    ae <- as.data.frame(Dias_Diez)
    #Mapa en el inicio
    
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      m=leaflet() %>% addTiles() 
      m = m %>% setView(-74.14524, 4.583009, zoom = 10)                   
      
      rand_lng = function(n = 10) rnorm(n, -74.14524, .01)
      rand_lat = function(n = 10) rnorm(n, 4.583009, .01)
      m %>% addMarkers(
        c(-74.0846507, -74.0644102, -74.0644102, -74.0769041, -74.0894205, -74.1344117, -74.1662822, -74.1506637, -74.1695299, -74.1662822, -74.1005992, -74.0802431, -74.0766807, -74.0758646, -74.0858122, -74.1250207, -74.1175984, -74.1276161, -74.113203, -74.1176081, -74.1442677, -74.0863329, -74.1846839, -74.0714006, -74.0978877, -74.1214353, -74.1157556, -74.1404247, -74.1234964, -74.0852507, -74.1151542, -74.1461573, -74.0817106, -74.1895225, -74.0816487, -74.080697, -74.1678285, -74.2054324, -74.1901743, -74.2076043, -74.1842044),
        c(4.6541068, 4.5923033, 4.6176856, 4.5689444, 4.5645912, 4.4412805, 4.3863444, 4.3861149, 4.3887616, 4.3863444, 4.7126821, 4.6916962, 4.6729498, 4.6720338, 4.6054925, 4.6171448, 4.5797157, 4.5787847, 4.5724414, 4.4945327, 4.5371033, 4.6849893, 4.61135, 4.5982943, 4.5520583, 4.5798792, 4.5251372, 4.5508218, 4.5790149, 4.6867513, 4.7531913, 4.6379617, 4.5695217, 4.6083287, 4.6721553, 4.5693544, 4.3537527, 4.3393743, 4.3705632, 4.4112932, 4.3442325),
        icon = list(
          iconUrl = 'https://cdn3.iconfinder.com/data/icons/linecons-free-vector-icons-pack/32/world-512.png', iconSize = c(15)
        ), popup = c("COLEGIO SIMON RODRIGUEZ (IED)", "COLEGIO ANTONIO JOSE URIBE (IED)", "COLEGIO MANUEL ELKIN PATARROYO (IED)", "COLEGIO PANTALEON GAITAN PEREZ (CED)", "COLEGIO TECNICO TOMAS RUEDA VARGAS (IED)", "COLEGIO RURAL OLARTE (CED)", "COLEGIO RURAL LOS ARRAYANES (CED)", "COLEGIO RURAL LA ARGENTINA (CED)", "COLEGIO RURAL EL CURUBITAL (CED)", "COLEGIO RURAL EL HATO (CED)", "COLEGIO JOSE ASUNCION SILVA (IED)", "COLEGIO INSTITUTO TÉCNICO DISTRITAL JULIO FLOREZ (IED)", "COLEGIO REPUBLICA DE PANAMA (IED)", "COLEGIO RAFAEL BERNAL JIMENEZ (IED)", "COLEGIO LICEO NACIONAL AGUSTIN NIETO CABALLERO (IED)", "COLEGIO JOSE MANUEL RESTREPO (IED)", "COLEGIO QUIROGA ALIANZA (IED)", "COLEGIO REPUBLICA FEDERAL DE ALEMANIA (IED)", "COLEGIO JOSE MARTI (IED)", "COLEGIO USMINIA (IED)", "COLEGIO EL MINUTO DE BUENOS AIRES (IED)", "COLEGIO TECNICO JUAN DEL CORRAL", "COLEGIO EL LIBERTADOR (IED)", "COLEGIO INTEGRADA LA CANDELARIA (IED)", "COLEGIO JUAN EVANGELISTA GOMEZ (IED)", "COLEGIO BRAVO PAEZ (IED)", "COLEGIO SANTA MARTHA  (IED)", "COLEGIO LEON DE GREIFF (IED)", "COLEGIO RESTREPO MILLAN (IED)", "COLEGIO MAGDALENA ORTEGA DE NARIÑO (IED)", "COLEGIO DELIA ZAPATA OLIVELLA (IED)", "COLEGIO SAN JOSE DE CASTILLA (IED)", "COLEGIO SAN CRISTOBAL SUR (IED)", "COLEGIO FRANCISCO DE PAULA SANTANDER (IED)", "COLEGIO JORGE ELIECER GAITAN (IED)", "COLEGIO ESTANISLAO ZULETA", "INSTITUCION EDUCATIVA AGRUPACIÓN RURAL USME ALTO SEDE ANDES", "INSTITUCION EDUCATIVA AGRUPACIÓN RURAL USME ALTO SEDE CHIZACÁ", "INSTITUCION EDUCATIVA AGRUPACIÓN RURAL USME ALTO SEDE MAYORÍA", "INSTITUCION EDUCATIVA AGRUPACIÓN RURAL USME ALTO SEDE MERCEDES", "INSTITUCION EDUCATIVA AGRUPACIÓN RURAL USME ALTO SEDE UNION USME")
      )
    }) 
  
    # Sección de monitoreo
      #Aprendizaje
        #Dias Diez
    
    output$DiasDiez <- renderRpivotTable({
      rpivotTable(data = Dias_Diez)
      change_locale(rpivotTable(Dias_Diez), "es")
    })  
    
    
    #Sección de Ambiente Escolar
    
      # Sexo
    
    AmbienteEscolar.df <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.melt <- melt(AmbienteEscolar.df, id=c("SEXO"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6", "Factor7"))
    AmbienteEscolar.melt <- na.omit(AmbienteEscolar.melt)
    Factores.mean <- ddply(AmbienteEscolar.melt, .(SEXO, variable),  summarise, mean = mean(value))
    
    output$GraficoAE_Sex <- renderChart({
      GraficoAE_Sex <- nPlot(
        mean ~ SEXO,
        group = "variable",
        data = Factores.mean,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoAE_Sex$addParams(dom="GraficoAE_Sex", "nvd3")
      GraficoAE_Sex$chart(forceY = c(0, 10))
      
      return(GraficoAE_Sex)
      
    })
    
      # Rango de Edad
    
    AmbienteEscolar.df3 <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.melt3 <- melt(AmbienteEscolar.df3, id=c("Edad_R"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6", "Factor7"))
    AmbienteEscolar.melt3 <- na.omit(AmbienteEscolar.melt3)
    Factores.mean3 <- ddply(AmbienteEscolar.melt3, .(Edad_R, variable),  summarise, mean = mean(value))
    
    output$GraficoAE_Age <- renderChart({
      GraficoAE_Age <- nPlot(
        mean ~ Edad_R,
        group = "variable",
        data = Factores.mean3,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoAE_Age$addParams(dom="GraficoAE_Age", "nvd3")
      GraficoAE_Age$chart(forceY = c(0, 10))
      return(GraficoAE_Age)
      
    })
    
        # Nivel Educativo
    
    AmbienteEscolar.df4 <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.melt4 <- melt(AmbienteEscolar.df4, id=c("NIVEL_EDUCACION"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6", "Factor7"))
    AmbienteEscolar.melt4 <- na.omit(AmbienteEscolar.melt4)
    Factores.mean4 <- ddply(AmbienteEscolar.melt4, .(NIVEL_EDUCACION, variable),  summarise, mean = mean(value))
    
    output$GraficoAE_lev <- renderChart({
      GraficoAE_lev <- nPlot(
        mean ~ NIVEL_EDUCACION,
        group = "variable",
        data = Factores.mean4,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoAE_lev$addParams(dom="GraficoAE_lev", "nvd3")
      GraficoAE_lev$chart(forceY = c(0, 10))
      return(GraficoAE_lev)
      
    })    
    
      # Localidad
    AmbienteEscolar.df2 <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.melt2 <- melt(AmbienteEscolar.df2, id=c("LOCALIDAD"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6", "Factor7"))
    AmbienteEscolar.melt2 <- na.omit(AmbienteEscolar.melt2)
    Factores.mean2 <- ddply(AmbienteEscolar.melt2, .(LOCALIDAD, variable),  summarise, mean = mean(value))
    
    output$GraficoAE <- renderChart({
    GraficoAE <- nPlot(
      mean ~ LOCALIDAD,
      group = "variable",
      data = Factores.mean2,
      type= "multiBarChart",
      ylim=c(0, 10)
    )
    GraficoAE$addParams(dom="GraficoAE", "nvd3")
    GraficoAE$chart(forceY = c(0, 10))
    return(GraficoAE)
    
    })

  # Radar por IE
    output$radar <- renderChartJSRadar({
      AmbienteEscolar.df<- AmbienteEscolar <- read_excel("data/AmbienteEscolar.xls")
      AmbienteEscolar.df<- na.omit(AmbienteEscolar.df)
      Factores.mean.ie <- ddply(AmbienteEscolar.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                                "Desempeño Academico"=mean(Factor1),
                                "Institucionalización y liderazgo"=mean(Factor2),
                                "Seguridad y Respeto"=mean(Factor3),
                                "Educación inclusiva"=mean(Factor4),
                                "Ambientes de aprendizaje"=mean(Factor5),
                                "Comunidades de aprendizaje"=mean(Factor6),
                                "Retroalimentación"=mean(Factor7),
                                "Indice Ambiente Escolar"=mean(Indice)
      )
      AmbienteEscolar.melt4 <- melt(Factores.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Ambiente Escolar",
                                                                                                   "Desempeño Academico", 
                                                                                                   "Institucionalización y liderazgo",
                                                                                                   "Seguridad y Respeto",
                                                                                                   "Educación inclusiva",
                                                                                                   "Ambientes de aprendizaje", 
                                                                                                   "Comunidades de aprendizaje", 
                                                                                                   "Retroalimentación"))
      newdata <- AmbienteEscolar.melt4[ which(AmbienteEscolar.melt4$INSTITUCIONEDUCATIVA==input$input_type),] 
      newdata <- data.frame(newdata,
                            "Bogota" = c( 6.991967,  7.098642 ,  6.61943,  7.055721,  5.961371,  4.407299 , 5.923812  ,6.293873))
      newdata2<-data.frame(IE=newdata$value, SaberDigital=newdata$Bogota)      
      

      chartJSRadar(newdata2, labs = newdata$variable, showLegend=TRUE,
                   main = unique(newdata$INSTITUCIONEDUCATIVA), maxScale = 10, showToolTipLabel=TRUE)
    })
    
  # Tabla Dinamica
    AmbienteEscolar<- read_excel("data/AmbienteEscolar 2.xls")
    ae <- as.data.frame(AmbienteEscolar)
    
    output$AmbienteEscolar <- renderRpivotTable({
      rpivotTable(data = AmbienteEscolar)
      change_locale(rpivotTable(AmbienteEscolar), "es")
    }) 
    
    #Sección de Uso de TIC
    
    # Sexo
    
    Uso.df <- read_excel("data/UsoTIC.xls")
    Uso.melt <- melt(Uso.df, id=c("SEXO"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6"))
    Uso.melt <- na.omit(Uso.melt)
    FactoresU.mean <- ddply(Uso.melt, .(SEXO, variable),  summarise, mean = mean(value))
    
    output$GraficoUso_Sex <- renderChart({
      GraficoUso_Sex <- nPlot(
        mean ~ SEXO,
        group = "variable",
        data = FactoresU.mean,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoUso_Sex$addParams(dom="GraficoUso_Sex", "nvd3")
      GraficoUso_Sex$chart(forceY = c(0, 10))
      
      return(GraficoUso_Sex)
      
    })   
    
    # Rango Edad
    
    UsoA.df <- read_excel("data/UsoTIC.xls")
    UsoA.melt <- melt(UsoA.df, id=c("Edad_R"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6"))
    UsoA.melt <- na.omit(UsoA.melt)
    FactoresUA.mean <- ddply(UsoA.melt, .(Edad_R, variable),  summarise, mean = mean(value))
    
    output$GraficoUso_Age <- renderChart({
      GraficoUso_Age <- nPlot(
        mean ~ Edad_R,
        group = "variable",
        data = FactoresUA.mean,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoUso_Age$addParams(dom="GraficoUso_Age", "nvd3")
      GraficoUso_Age$chart(forceY = c(0, 10))
      
      return(GraficoUso_Age)
      
    }) 

    # Nivel Educativo
    
    UsoNE.df <- read_excel("data/UsoTIC.xls")
    UsoNE.melt <- melt(UsoNE.df, id=c("NIVEL_EDUCACION"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6"))
    UsoNE.melt <- na.omit(UsoNE.melt)
    FactoresUNE.mean <- ddply(UsoNE.melt, .(NIVEL_EDUCACION, variable),  summarise, mean = mean(value))
    
    output$GraficoUso_NE <- renderChart({
      GraficoUso_NE <- nPlot(
        mean ~ NIVEL_EDUCACION,
        group = "variable",
        data = FactoresUNE.mean,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoUso_NE$addParams(dom="GraficoUso_NE", "nvd3")
      GraficoUso_NE$chart(forceY = c(0, 10))
      
      return(GraficoUso_NE)
      
    })
    
    # Localidad
    
    UsoLO.df <- read_excel("data/UsoTIC.xls")
    UsoLO.melt <- melt(UsoLO.df, id=c("LOCALIDAD"), measure.vars=c("Indice", "Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6"))
    UsoLO.melt <- na.omit(UsoLO.melt)
    FactoresULO.mean <- ddply(UsoLO.melt, .(LOCALIDAD, variable),  summarise, mean = mean(value))
    
    output$GraficoUso_LO <- renderChart({
      GraficoUso_LO <- nPlot(
        mean ~ LOCALIDAD,
        group = "variable",
        data = FactoresULO.mean,
        type= "multiBarChart",
        ylim=c(0, 10)
      )
      GraficoUso_LO$addParams(dom="GraficoUso_LO", "nvd3")
      GraficoUso_LO$chart(forceY = c(0, 10))
      
      return(GraficoUso_LO)
      
    }) 
    # Radar por IE
    output$radar2 <- renderChartJSRadar({
      UsoR.df<- read_excel("data/UsoTIC.xls")
      UsoR.df<- na.omit(UsoR.df)
      FactoresUR.mean.ie <- ddply(UsoR.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                                "Intencionalidad"=mean(Factor1),
                                "Conocimiento"=mean(Factor2),
                                "Uso de TIC"=mean(Factor3),
                                "Beneficios uso TIC"=mean(Factor4),
                                "Tecnologia"=mean(Factor5),
                                "Institucionalizacion"=mean(Factor6),
                                "Indice Uso de TIC"=mean(Indice)
      )
      UsoR.melt4 <- melt(FactoresUR.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Uso de TIC",
                                                                                                   "Intencionalidad", 
                                                                                                   "Conocimiento",
                                                                                                   "Uso de TIC",
                                                                                                   "Beneficios uso TIC",
                                                                                                   "Tecnologia", 
                                                                                                   "Institucionalizacion")) 
      newdata2 <- UsoR.melt4[ which(UsoR.melt4$INSTITUCIONEDUCATIVA==input$input_type2),] 
      newdata2 <- data.frame(newdata2,
                            "Bogota" = c( 5.497955,  2.180533,  5.863003 , 8.200051 , 7.258293,  5.853239,  4.196026))
      newdata3<-data.frame(IE=newdata2$value, SaberDigital=newdata2$Bogota)      
      
      
      chartJSRadar(newdata3, labs = newdata2$variable, showLegend=TRUE,
                   main = unique(newdata2$INSTITUCIONEDUCATIVA), maxScale = 10, showToolTipLabel=TRUE)
    })
    # Tabla Dinamica
    Uso<- read_excel("data/UsoTIC 2.xls")
    Uso <- as.data.frame(Uso)
    
    output$Uso <- renderRpivotTable({
      rpivotTable(data = Uso)
      change_locale(rpivotTable(Uso), "es")
    }) 
   
    # Reportes IE
  
    AmbienteEscolar.df<- AmbienteEscolar <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.df<- na.omit(AmbienteEscolar.df)
    Factores.mean.ie <- ddply(AmbienteEscolar.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                              "Desempeño Academico"=mean(Factor1),
                              "Institucionalización y liderazgo"=mean(Factor2),
                              "Seguridad y Respeto"=mean(Factor3),
                              "Educación inclusiva"=mean(Factor4),
                              "Ambientes de aprendizaje"=mean(Factor5),
                              "Comunidades de aprendizaje"=mean(Factor6),
                              "Retroalimentación"=mean(Factor7),
                              "Indice Ambiente Escolar"=mean(Indice)
    )
    AmbienteEscolar.melt4 <- melt(Factores.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Ambiente Escolar",
                                                                                                 "Desempeño Academico", 
                                                                                                 "Institucionalización y liderazgo",
                                                                                                 "Seguridad y Respeto",
                                                                                                 "Educación inclusiva",
                                                                                                 "Ambientes de aprendizaje", 
                                                                                                 "Comunidades de aprendizaje", 
                                                                                                 "Retroalimentación"))
    
    
    UsoR.df<- read_excel("data/UsoTIC.xls")
    UsoR.df<- na.omit(UsoR.df)
    FactoresUR.mean.ie <- ddply(UsoR.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                                "Intencionalidad"=mean(Factor1),
                                "Conocimiento"=mean(Factor2),
                                "Uso de TIC"=mean(Factor3),
                                "Beneficios uso TIC"=mean(Factor4),
                                "Tecnologia"=mean(Factor5),
                                "Institucionalizacion"=mean(Factor6),
                                "Indice Uso de TIC"=mean(Indice)
    )
    UsoR.melt4 <- melt(FactoresUR.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Uso de TIC",
                                                                                        "Intencionalidad", 
                                                                                        "Conocimiento",
                                                                                        "Uso de TIC",
                                                                                        "Beneficios uso TIC",
                                                                                        "Tecnologia", 
                                                                                        "Institucionalizacion"))
    
    
    new <-  rbind(AmbienteEscolar.melt4, UsoR.melt4)
    AmbienteEscolar.df<- AmbienteEscolar <- read_excel("data/AmbienteEscolar.xls")
    AmbienteEscolar.df<- na.omit(AmbienteEscolar.df)
    Factores.mean.ie <- ddply(AmbienteEscolar.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                              "Desempeño Academico"=mean(Factor1),
                              "Institucionalización y liderazgo"=mean(Factor2),
                              "Seguridad y Respeto"=mean(Factor3),
                              "Educación inclusiva"=mean(Factor4),
                              "Ambientes de aprendizaje"=mean(Factor5),
                              "Comunidades de aprendizaje"=mean(Factor6),
                              "Retroalimentación"=mean(Factor7),
                              "Indice Ambiente Escolar"=mean(Indice)
    )
    AmbienteEscolar.melt4 <- melt(Factores.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Ambiente Escolar",
                                                                                                 "Desempeño Academico", 
                                                                                                 "Institucionalización y liderazgo",
                                                                                                 "Seguridad y Respeto",
                                                                                                 "Educación inclusiva",
                                                                                                 "Ambientes de aprendizaje", 
                                                                                                 "Comunidades de aprendizaje", 
                                                                                                 "Retroalimentación"))
    # Información por IE
    
      # Ambiente Escolar
    output$radarIE <- renderChartJSRadar({    
    UsoR.df<- read_excel("data/UsoTIC.xls")
    UsoR.df<- na.omit(UsoR.df)
    FactoresUR.mean.ie <- ddply(UsoR.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                                "Intencionalidad"=mean(Factor1),
                                "Conocimiento"=mean(Factor2),
                                "Uso de TIC"=mean(Factor3),
                                "Beneficios uso TIC"=mean(Factor4),
                                "Tecnologia"=mean(Factor5),
                                "Institucionalizacion"=mean(Factor6),
                                "Indice Uso de TIC"=mean(Indice)
    )
    UsoR.melt4 <- melt(FactoresUR.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Uso de TIC",
                                                                                        "Intencionalidad", 
                                                                                        "Conocimiento",
                                                                                        "Uso de TIC",
                                                                                        "Beneficios uso TIC",
                                                                                        "Tecnologia", 
                                                                                        "Institucionalizacion"))
    
    AppendData <-  rbind(AmbienteEscolar.melt4, UsoR.melt4)
    attach(AppendData)
    
    RadarAmbiente <- AppendData[which(variable=="Desempeño Academico" | variable=="Seguridad y Respeto" |
                                        variable=="Institucionalización y liderazgo" | variable=="Educación inclusiva" |
                                        variable=="Ambientes de aprendizaje" | variable=="Comunidades de aprendizaje" |
                                        variable=="Retroalimentación" | variable=="Indice Ambiente Escolar"),]
    
    RadarAmbiente <- data.frame(RadarAmbiente,
                                "Bogota" = c( 6.991967,  7.098642 ,  6.61943,  7.055721,  5.961371,  4.407299 , 5.923812  ,6.293873))
    RadarAmbiente <- RadarAmbiente[ which(RadarAmbiente$INSTITUCIONEDUCATIVA==input$input_type3),] 
    
    RadarAmbiente2<-data.frame(IE=RadarAmbiente$value, SaberDigital=RadarAmbiente$Bogota)  
    
    chartJSRadar(RadarAmbiente2, labs = RadarAmbiente$variable, showLegend=TRUE,
                 main = unique(RadarAmbiente$INSTITUCIONEDUCATIVA), maxScale = 10, showToolTipLabel=TRUE,  labelSize=6)
    })     
    
    # Uso de TIC 
    output$radarIE2 <- renderChartJSRadar({    
      UsoR.df<- read_excel("data/UsoTIC.xls")
      UsoR.df<- na.omit(UsoR.df)
      FactoresUR.mean.ie <- ddply(UsoR.df, .(INSTITUCIONEDUCATIVA),  summarise, 
                                  "Intencionalidad"=mean(Factor1),
                                  "Conocimiento"=mean(Factor2),
                                  "Uso de TIC"=mean(Factor3),
                                  "Beneficios uso TIC"=mean(Factor4),
                                  "Tecnologia"=mean(Factor5),
                                  "Institucionalizacion"=mean(Factor6),
                                  "Indice Uso de TIC"=mean(Indice)
      )
      UsoR.melt4 <- melt(FactoresUR.mean.ie, id=c("INSTITUCIONEDUCATIVA"), measure.vars=c("Indice Uso de TIC",
                                                                                          "Intencionalidad", 
                                                                                          "Conocimiento",
                                                                                          "Uso de TIC",
                                                                                          "Beneficios uso TIC",
                                                                                          "Tecnologia", 
                                                                                          "Institucionalizacion"))
      
      AppendData <-  rbind(AmbienteEscolar.melt4, UsoR.melt4)
      attach(AppendData)
      
      RadarAmbiente <- AppendData[which(variable=="Indice Uso de TIC" | variable=="Intencionalidad" | variable=="Conocimiento" |
                                          variable=="Uso de TIC" | variable=="Beneficios uso TIC" |
                                          variable=="Tecnologia" | variable=="Institucionalizacion" ),]
      
      RadarAmbiente <- data.frame(RadarAmbiente,
                                  "Bogota" = c( 5.497955,  2.180533,  5.863003 , 8.200051 , 7.258293,  5.853239,  4.196026))
      RadarAmbiente <- RadarAmbiente[ which(RadarAmbiente$INSTITUCIONEDUCATIVA==input$input_type3),] 
      
      RadarAmbiente2<-data.frame(IE=RadarAmbiente$value, SaberDigital=RadarAmbiente$Bogota)  
      
      chartJSRadar(RadarAmbiente2, labs = RadarAmbiente$variable, showLegend=TRUE,
                   main = unique(RadarAmbiente$INSTITUCIONEDUCATIVA), maxScale = 10, showToolTipLabel=TRUE,  labelSize=6)
    })  
    
    # Información de la IE
    
    output$Historia <- renderText({ 
      InformacionIE.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE <- InformacionIE.df[ which(InformacionIE.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE$HISTORIA)
    })
    
    output$Comunidad <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Comunidad)
      
    })   
  
    output$Necesidades <- renderPlot({ 
      InformacionIE3.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE3 <- InformacionIE3.df[ which(InformacionIE3.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      
      my_data <- data.frame(
        variable = c("Manejo de herramientas de ofimatica e internet", 
                     "Software educativo",
                     "Herramientas de comunicacion y redes sociales",
                     "Estrategias para integrar las TIC al curriculo",
                     "Creacion de contenidos y recursos educativos digitales"),
        value = c(InformacionIE3$N_Ofimatica, InformacionIE3$N_Software,
                  InformacionIE3$N_Comunicacion, InformacionIE3$N_Estrategias,
                  InformacionIE3$N_Contenidos))
      
      p <- ggplot(my_data, aes(x = variable, y = value, label=value)) + 
        geom_bar(stat = "identity", fill = "white", colour = "red", width = 0.4) +
        geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggtitle("Porcentaje de docentes según la necesidad de formación") + 
        theme_bw() +
        coord_flip()+
        theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(colour = "grey50"),
              plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
              axis.ticks.x = element_blank(),
              axis.title = element_blank())
      p
			
    }) 

    output$Formacion <- renderPlot({ 
      InformacionIE4.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE4 <- InformacionIE4.df[ which(InformacionIE4.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      
      my_data2 <- data.frame(
        variable = c("Presencial", 
                     "Virtual",
                     "Mixta"),
        
        value = c(InformacionIE4$F_Presencial, InformacionIE4$F_Virtual,
                  InformacionIE4$F_Mixta))
      
      pp <- ggplot(my_data2, aes(x = variable, y = value, label=value)) + 
        geom_bar(stat = "identity", fill = "white", colour = "red", width = 0.4) +
        geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggtitle("Porcentaje de docentes según la forma como quieren recibir la formación") + 
        theme_bw() +
        coord_flip()+
        theme(panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(colour = "grey50"),
              plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
              axis.ticks.x = element_blank(),
              axis.title = element_blank())
      pp
      
    }) 
    
      # Electricidad
    output$Planos <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Planos_electrico)
      
    })    
    
    output$Riesgo <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Plan_riesgo)
      
    }) 
    
    output$Protocolos <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Protocolos)
      
    }) 
    
    output$Recursos <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Recursos_electricos)
      
    }) 
    
    output$Mantenimiento <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Mantenimiento_electrico)
      
    }) 
    
    output$Electricista <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Electricista)
      
    }) 
    
    # Conectividad
    
    output$ProtocolosCon <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Protocolos_Conectividad)
      
    })    
    
    output$RecursosConectividad <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Recursos_Conectividad)
      
    }) 
    
    output$MantenimientoConectividad <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Mantenimiento_Conectividad)
      
    }) 
    
    output$ProfesionalRedes <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Profesional_Redes)
      
    }) 
    
    output$MejoramientoRedes <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Mejoramiento_Redes)
      
    }) 
    
    output$MantenimientoRedes <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Fecha_Mantenimiento)
      
    })     
    
        # Conectividad 2
    output$Proveedor <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Proveedor)
      
    })    
    
    output$TipoConexion <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Tipo_Conexion)
      
    }) 
    
    output$IPPublica <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$IpPublica)
      
    }) 
    
    output$AnchoDescarga <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Ancho_Descarga)
      
    }) 
    
    output$AnchoCarga <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Ancho_Carga)
      
    }) 
    
    output$Equipos <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Equipos)
      
    })   
   
    # Gestión
      # Sueños
    output$Sueño1 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Sueño1)
      
    }) 
    
    output$Sueño2 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Sueño2)
      
    }) 
    
    output$Sueño3 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Sueño3)
      
    }) 
    
    output$Sueño4 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Sueño4)
      
    }) 
    
    output$Sueño5 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$Sueño5)
      
    })       
    # Que tienen
    output$QueTenemos1 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueTenemos1)
      
    }) 
    
    output$QueTenemos2 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueTenemos2)
      
    }) 
    
    output$QueTenemos3 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueTenemos3)
      
    }) 
    
    output$QueTenemos4 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueTenemos4)
      
    }) 
    
    output$QueTenemos5 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueTenemos5)
      
    })    
    #Que Frena 
    output$QueFrena1 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueFrena1)
      
    }) 
    
    output$QueFrena2 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueFrena2)
      
    }) 
    
    output$QueFrena3 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueFrena3)
      
    }) 
    
    output$QueFrena4 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueFrena4)
      
    }) 
    
    output$QueFrena5 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueFrena5)
      
    })    
    # Que riesgos
    output$QueRiesgos1 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueRiesgos1)
      
    }) 
    
    output$QueRiesgos2 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueRiesgos2)
      
    }) 
    
    output$QueRiesgos3 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueRiesgos3)
      
    }) 
    
    output$QueRiesgos4 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueRiesgos4)
      
    }) 
    
    output$QueRiesgos5 <- renderText({ 
      InformacionIE2.df<- read_excel("data/InformacionIE.xlsx")
      InformacionIE2 <- InformacionIE2.df[ which(InformacionIE2.df$INSTITUCIONEDUCATIVA==input$input_type3),] 
      print (InformacionIE2$QueRiesgos5)
      
    })   
  })     