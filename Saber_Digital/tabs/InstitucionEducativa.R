library(radarchart)
library(shiny)
library(readxl)
require(plyr)
require(rCharts)
require(reshape2)
require(devtools)
library(radarchart)
library(rpivotTable)

UsoR.df<- read_excel("data/UsoTIC.xls")
InformacionIE <- read_excel("data/InformacionIE.xlsx")



tabPanel(title = "Informes institucionales", icon = icon("info-circle"),
         div(id = "home",
             
             br(),
             #tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
             #tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
             p(class = "lead", align="justify", "En esta sección encontrá información para la institución educativa."),
             selectizeInput(
               'input_type3', 'Aquí puede buscar la institución educativa', choices = unique(UsoR.df$INSTITUCIONEDUCATIVA), multiple = FALSE
               
             ),
             div(class="intro-divider"),             
             tabsetPanel(
               tabPanel("Información general",

                        h3("Descripción"),
                        textOutput("Historia"),
                        br(),
                        textOutput("Comunidad"),
                        br()
               ),
               
               navbarMenu("Tecnología",
               tabPanel("Electricidad",

                        
                        h3("Protocolos de mantenimiento electrico institucional"),
                        fluidRow(
                        column(6,
                               h6(align = "center", "¿La IE cuenta con planos eléctricos?"),
                               h1(align="center", style = "color:gray", textOutput("Planos", inline = TRUE))
                        ),
                        column(6,
                               h6(align = "center", "¿La IE cuenta con un plan de manejo de riesgo eléctrico?"),
                               h1(align="center", style = "color:gray", textOutput("Riesgo", inline = TRUE))
                        ),
                        column(6,
                               h6(align = "center", "¿La IE cuenta concon protocolos de mantenimiento eléctrico?"),
                               h1(align="center", style = "color:gray", textOutput("Protocolos", inline = TRUE))
                        ),
                        column(6,
                               h6(align = "center", "¿En la IE se ejecutan recursos económicos para el mantenimiento eléctrico?"),
                               h1(align="center", style = "color:gray", textOutput("Recursos", inline = TRUE))
                        ),
                        column(6,
                               h6(align = "center", "¿Con qué frecuencia se realiza mantenimiento électrico?"),
                               h1(align="center", style = "color:gray", textOutput("Mantenimiento", inline = TRUE))
                        ),
                        column(6,
                               h6(align = "center", "¿Es facil disponer de un electricista para la realización de Mantenimiento eléctrico en la IE?"),
                               h1(align="center", style = "color:gray", textOutput("Electricista", inline = TRUE))
                        )
                               )
                        

               ),
               tabPanel("Conectividad",
                        h3("Internet Academico"),
                        fluidRow(
                          column(6,
                                 h6(align = "center", "Empresa proveedora del servicio de Internet"),
                                 h1(align="center", style = "color:gray", textOutput("Proveedor", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Tipo de Conexión"),
                                 h1(align="center", style = "color:gray", textOutput("TipoConexion", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Direccionamiento internet académico (IP Pública)"),
                                 h1(align="center", style = "color:gray", textOutput("IPPublica", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Ancho de banda (descarga)"),
                                 h1(align="center", style = "color:gray", textOutput("AnchoDescarga", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Ancho de banda (carga)"),
                                 h1(align="center", style = "color:gray", textOutput("AnchoCarga", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Cantidad de computadoras conectadas a internet para uso académico"),
                                 h1(align="center", style = "color:gray", textOutput("Equipos", inline = TRUE))
                          )
                        ),
                        div(class="intro-divider"),             
                        
                        h3("Protocolos de Conectividad"),
                        fluidRow(
                          column(6,
                                 h6(align = "center", "¿La IE cuenta con protocolos de conectividad?"),
                                 h1(align="center", style = "color:gray", textOutput("ProtocolosCon", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "¿Se ejecutan recursos económicos en Mantenimiento en Conectividad?"),
                                 h1(align="center", style = "color:gray", textOutput("RecursosConectividad", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "¿Con qué frecuencia se realiza mantenimiento de conectividad?"),
                                 h1(align="center", style = "color:gray", textOutput("MantenimientoConectividad", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "¿Es fácil disponer de un profesional en redes para la realización de Mantenimiento de conectividad en la IE"),
                                 h1(align="center", style = "color:gray", textOutput("ProfesionalRedes", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "¿Existe algún plan de mejora para las redes cableadas o inalámbricas?"),
                                 h1(align="center", style = "color:gray", textOutput("MejoramientoRedes", inline = TRUE))
                          ),
                          column(6,
                                 h6(align = "center", "Fecha del último mantenimiento técnico"),
                                 h5(align="center", style = "color:gray", textOutput("MantenimientoRedes", inline = TRUE))
                          )
                        )

                        )

               ),
               tabPanel("Gestión",
                        h3("¿Cuáles son los sueños e ideas que quieren llevar a su institución educativa?"),
                        tags$ul(
                          tags$li(textOutput("Sueño1"), inline = TRUE, align="justify"), 
                          tags$li(textOutput("Sueño2"), align="justify"), 
                          tags$li(textOutput("Sueño3"), align="justify"),
                          tags$li(textOutput("Sueño4"), align="justify"),
                          tags$li(textOutput("Sueño5"), align="justify")
                        ),
                        
                        h3("¿Cuáles son los elementos que los impulsan y ayudan a alcanzar sus metas y sueños?"),
                        tags$ul(
                          tags$li(textOutput("QueTenemos1"), inline = TRUE, align="justify"), 
                          tags$li(textOutput("QueTenemos2"), align="justify"), 
                          tags$li(textOutput("QueTenemos3"), align="justify"),
                          tags$li(textOutput("QueTenemos4"), align="justify"),
                          tags$li(textOutput("QueTenemos5"), align="justify")
                        ),
                        h3("¿Qué cosas internas de la institución frenan o limitan el logro de estos sueños?"),
                        tags$ul(
                          tags$li(textOutput("QueFrena1"), inline = TRUE, align="justify"), 
                          tags$li(textOutput("QueFrena2"), align="justify"), 
                          tags$li(textOutput("QueFrena3"), align="justify"),
                          tags$li(textOutput("QueFrena4"), align="justify"),
                          tags$li(textOutput("QueFrena5"), align="justify")
                        ),
                        h3("¿Qué obstáculos o riesgos se presentan en el entorno y afectan su institución educativa?"),
                        tags$ul(
                          tags$li(textOutput("QueRiesgos1"), inline = TRUE, align="justify"), 
                          tags$li(textOutput("QueRiesgos2"), align="justify"), 
                          tags$li(textOutput("QueRiesgos3"), align="justify"),
                          tags$li(textOutput("QueRiesgos4"), align="justify"),
                          tags$li(textOutput("QueRiesgos5"), align="justify")
                        )
                        ),
               tabPanel("I+D+i",
                        p("Hola")
               ),
               tabPanel("Aprendizaje",
                        h2("Necesidades de Formación"),
                        
                        plotOutput("Necesidades"),
                        br(), br(),
                        plotOutput("Formacion"),
                        
                        br()
               ),    
               tabPanel("Indices Multivariados",
                        
                        fluidRow(
                         
                          column(2,
                                 h4("Indice multivariado de ambiente escolar"),
                                 chartJSRadarOutput("radarIE", width = "500", height = "500"), width = 6,
                                 br()
                          ),
                          column(2, 
                                 h4("Indice multivariado de uso y apropiación de TIC"),
                                 
                                 chartJSRadarOutput("radarIE2", width = "500", height = "500"), width = 6,
                                 br()
                          ) 
                        )
                        
                          )
              
     
                        
               
             )
         )  
)
