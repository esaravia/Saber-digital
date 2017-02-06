library(shiny)
library(readxl)
require(plyr)
require(rCharts)
require(reshape2)
require(devtools)
library(radarchart)
library(rpivotTable)

UsoR.df<- read_excel("data/UsoTIC.xls")

tabPanel(title = "Uso TIC", icon = icon("info-circle"),
         div(id = "home",
             
             br(),
             tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
             
             tabsetPanel(
               tabPanel("Introducción",
                        tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                        h2(class = "outer", "¿Por qué un indice de uso y apropiación de TIC?"),
                        p(align="justify","En la actualidad, se ha masificado el uso de herramientas y métodos que emplean las TecnologÍas de la Información y de las Comunicaciones en búsqueda de transformar los sistemas educativos acorde a los nuevos paradigmas de la educación. A pesar de esto, en la actualidad no existe un consenso en la literatura sobre la dirección del efecto de las TIC en términos de aprendizaje, retención y éxito de los estudiantes de educación básica y media. Esto puede estar ocasionado, porque en efecto, la apropiación y los resultados de las TIC dependen en gran medida del contexto y el tipo de uso que se les dé, porque las variables empleadas no necesariamente reflejan el estado tecnológico de las instituciones educativas, o simplemente, porque los procesos de incorporación de tecnología no han alcanzado el nivel de madurez suficiente para lograr cambios en el aprendizaje de los estudiantes."),
                        p(align="justify","En general las investigaciones que buscan proponer indicadores y mediadas estandarizadas relativas a las TIC se han enfocado principalmente en el acceso y el uso, de forma aislada y sin tener en cuenta otros tipos de determinantes. Lo anterior puede causar dificultad en el análisis de los resultados y efectos, además, es posible que existan variables altamente correlacionadas que estén ocasionando medicionaes del mismo fenómeno inconsiente, derivando en resultados sesgados o espureos."),
                        p(align="justify", "Asi, el indice de uso y apropiación de TIC es una medida multivariada fundamentada en los bloques temáticos y dimensiones del modelo UbiTAG. Para su contrucción se empleó una muestra compuesta por 10.937 docentes pertenecientes a 265 instituciones educativas oficiales pertenecientes al Plan Digital TESO (24), Plan Nacional Colegio 10TIC (200) y Plan Saber Digital (41). Para su estimación se siguió la metodología de Análisis Factorial Exploratorio a partir de la cual se pudo establecer la validez del instrumento y se cuantificó un índice."),
                        
                        br()
               ),
               
               tabPanel("En qué consiste",
                        
                        h3(class = "outer", "¿Qué es el indice multivariado de uso y apropiación de TIC?"),
                        p(align="justify", "El indice de uso y apropiación de TIC es una medida multivariada que permite determinar el el nivel de uso y apropiación de TIC de una institución educativa con respecto al resto de instituciones educativas en las que se desallorra el modelo UbiTAG."),
                        h3(class = "outer", "¿Qué elementos componen el indice multivariado de uso y apropiación de TIC?"),
                        p("Los elementos que considera este instrumento son:", align="justify"),

                        tags$ul(
                          tags$li(align="justify", strong("Intencionalidad:"), "Se refiere a los objetivos, estrategias y acciones que los estudiantes, docentes y directivos tienen al usar las TIC."),
                                  br(),
                          tags$li(align="justify", strong("Uso de las TIC:"),"Mide el tipo de actividades que se desarrollan con los recursos tecnológicos, y su frecuencia de uso. Para la medición del uso de las TIC se consideran las siguientes definiciones:"),
                          tags$ul(
                            br(),
                            tags$li(align="justify", strong("Frecuencia de uso:"),"Periodicidad del uso de los recursos tecnológicos en el hogar, en la institución educativa y en el salón de clase."),
                            tags$li(align="justify", strong("Tipo de uso:"),"Actividades que realizan los estudiantes, docentes y directivos cuando usan las TIC (Comunicarse, Informarse, Trabar/Estudiar, Investigar, Conocer personas o Entretenerse).")
                                                      ),
                          br(),
                          tags$li(align="justify", strong("Conocimiento:"),"En términos generales, es la percepción de estudiantes, docente y directivos sobre su nivel de conocimiento en aspectos relacionados con las TIC. Para la medición se consideran las siguientes definiciones:"),
                          tags$ul(
                            br(),
                            tags$li(align="justify", strong("Conocimiento sobre herramientas tecnológicas:"),"Percepción sobre el nivel de conocimiento en aspectos básicos de informática (herramientas de ofimática)"),
                            tags$li(align="justify", strong("Conocimiento sobre estrategias y metodologías del uso de las TIC en educación:"),"Percepción sobre el nivel de conocimiento en aspectos relacionados con el uso de las TIC para la gestión de las actividades dentro del aula de clase, el desarrollo de proyectos colaborativos y producción de contenidos.")
                          ),
                          br(),
                          tags$li(align="justify", strong("Beneficios del uso de las TIC:"), "Beneficios que percibe el docente y que puede asociarse al uso de las TIC en su práctica pedagogica."),
                          br(),
                          tags$li(align="justify", strong("Disponibilidad tecnologica:"), "Existencia de recursos tecnológicos en el hogar o en la institución educativa (computador, internet, tabletas, cámaras, celular inteligente) a los que el usuario tiene acceso y que se encuentren en buen estado."),
                          br(),
                          tags$li(align="justify", strong("Institucionalización:"), "Indica la manera cómo se planea, evalúa y fomenta el uso de las TIC desde la gestión de la institución educativa."),
                         br()
                        ),
                        h3(class = "outer", "¿Cuales son las ventajas de indice multivariado de uso y apropiación de TIC?"),
                        p(align="justify", "Los índices multivariados son apropiados cuando se cuenta con una gran cantidad de variables que explican un mismo fenómeno, ya que:"),
                        
                        tags$ul(
                          tags$li("Tienen un alto poder de reducción de variables redundantes y facilitan el diseño y agregación de variables", align="justify"), 
                          tags$li("Permiten identificar dimensiones subyacentes a los datos que pueden ser relevantes para la intervención", align="justify"), 
                          tags$li("Permite ponderar adecuadamente los atributos que están altamente correlacionados", align="justify"),
                          tags$li("Permite la comparabilidad y ordenación entre docentes ", align="justify"),
                          tags$li("Permite identificar posibles errores de medición en los instrumentos", align="justify"),
                          tags$li("Los resultados son comparables con cerca de 300 instituciones donde se aplicó el instrumetno", align="justify")
                          
                        ),
                         br()
                        
               ),
               
               tabPanel("Cómo se construyó",
                        
                        h2(class = "outer", "¿Cómo se construyó el indice de uso y apropiación de TIC?"),
                        p(align="justify", "Siguiendo la metodología de", strong("Análisis Factorial Exploratorio,"), " y con el fin de contrastar a los docentes de las instituciones eductaivas del Plan Saber Digital con otra población docente, se emplearon 66 preguntas del instrumento aplicado a un grupo de 10.937 docentes pertenecientes a 265 instituciones educativas oficiales pertenecientes al Plan Digital TESO (24), Plan Nacional Colegio 10TIC (200) y Plan Saber Digital (41)."),
                        p(align="justify", "Para el primer caso, el valor estimado del KMO es de 0.938 lo cual indica que existe evidencia estadística sobre un alto grado de varianza común en el grupo de variables a agrupar; por otro lado, el test de esfericidad de Barlett rechaza la hipótesis nula confirmando la existencia de combinaciones lineales en la matriz y el valor del determinante de la matriz da evidencia sobre la pertinencia del método para analizar este grupo de variables.",
                          "Una vez estimado el AFE mediante el método del factor principal y verificados estos supuestos, se obtuvieron 55 factores de los cuales 5 de ellos cumplen con la regla de Kaiser, es decir, tienen valores propios mayores a uno. Adicionalmente y debido a la consistencia teorica, se eligió un sexto factor que tiene una proporción de varianza explicada de 4%; así, estos valores que son calculados como la sumatoria de las cargas factoriales al cuadrado explican el 98.4% de la varianza conjunta."),
                        p(align="justify", " Una vez garantizado esto, se realiza la rotación oblicua y se calculan las predicciones lineales de cada uno de los 6 factores seleccionados para cada uno de los cuales se realizó el test de normalidad de Jarque Bera y se rechazó la hipótesis nula de no normalidad en todos los factores seleccionados."),
                        p(img(src='Plot4.png', align = "center", width = 500)),
                        p(align="justify", "La nomenclatura e interpretación de los factores está fundamentada en la observación de la correlación de cada uno de estos con las variables originales y se deben identificar las variables cuyas correlaciones con el factor son elevadas, acorde a la literatura, se definió considerar como significativos los coeficientes de correlación superiores a 0.3. Una vez validados los supuestos, se calcularon las puntuaciones factoriales de cada uno de los constructos derivados del AFE empleando el método de regresión."),
                        p(align="justify", "Finalmente y con el objetivo de mejorar la interpretación del indice, se hizó un rescalamiento entre 0 y 10 para lo cual, para esto se calculó la diferencia entre el valor del indice y el mínimo de su distribución, se divide por el rango y se multiplica por 10.",
                          "De esta forma, el siguiente gráfico muestra la función de distribución del Índice Multivariado con el nuevo rango lo cual facilita su interpretación y lectura. El valor de este índice resultante representa el nivel de ambiente escolar según la autopercepción de los docentes y su valor es relativo a los otros docentes que realizaron la encuesta, es decir, provee solo mediciones relativas a la muestra y no permite extraer información en niveles absolutos"),
                        p(img(src='Plot5.png', align = "center", width = 400)),
                        
                        br()
             ),
             tabPanel("Resultados",
                      h2(class = "outer", "Resultados generales del indice"),
                      p(class = "lead", align="justify", "En esta sección encontrará los resultados generales del indice multivariado de uso y apropiación de TIC, adicionalmente, podrá consultar la información para cada institución educativa que participá del proyecto"),
                      div(class="intro-divider"),
                      tags$ul(
                        tags$li(align="justify", strong("Factor 1 (Intencionalidad):"), "Se refiere a los objetivos, estrategias y acciones que los estudiantes, docentes y directivos tienen al usar las TIC."),
                        br(),
                        tags$li(align="justify", strong("Factor 2 (Uso de las TIC):"),"Mide el tipo de actividades que se desarrollan con los recursos tecnológicos, y su frecuencia de uso. Para la medición del uso de las TIC se consideran las siguientes definiciones:"),
                        tags$ul(
                          br(),
                          tags$li(align="justify", strong("Frecuencia de uso:"),"Periodicidad del uso de los recursos tecnológicos en el hogar, en la institución educativa y en el salón de clase."),
                          tags$li(align="justify", strong("Tipo de uso:"),"Actividades que realizan los estudiantes, docentes y directivos cuando usan las TIC (Comunicarse, Informarse, Trabar/Estudiar, Investigar, Conocer personas o Entretenerse).")
                        ),
                        br(),
                        tags$li(align="justify", strong("Factor 3 (Conocimiento):"),"En términos generales, es la percepción de estudiantes, docente y directivos sobre su nivel de conocimiento en aspectos relacionados con las TIC. Para la medición se consideran las siguientes definiciones:"),
                        tags$ul(
                          br(),
                          tags$li(align="justify", strong("Conocimiento sobre herramientas tecnológicas:"),"Percepción sobre el nivel de conocimiento en aspectos básicos de informática (herramientas de ofimática)"),
                          tags$li(align="justify", strong("Conocimiento sobre estrategias y metodologías del uso de las TIC en educación:"),"Percepción sobre el nivel de conocimiento en aspectos relacionados con el uso de las TIC para la gestión de las actividades dentro del aula de clase, el desarrollo de proyectos colaborativos y producción de contenidos.")
                        ),
                        br(),
                        tags$li(align="justify", strong("Factor 4 (Beneficios del uso de las TIC):"), "Beneficios que percibe el docente y que puede asociarse al uso de las TIC en su práctica pedagogica."),
                        br(),
                        tags$li(align="justify", strong("Factor 5 (Disponibilidad tecnologica):"), "Existencia de recursos tecnológicos en el hogar o en la institución educativa (computador, internet, tabletas, cámaras, celular inteligente) a los que el usuario tiene acceso y que se encuentren en buen estado."),
                        br(),
                        tags$li(align="justify", strong("Factor 6 (Institucionalización):"), "Indica la manera cómo se planea, evalúa y fomenta el uso de las TIC desde la gestión de la institución educativa."),
                        br()
                      ),
                      p(align="justify", "A continuación podrá consultar los resultados según el sexo, rango de edad y máximo nivel educativo del docente y según la localidad donde se encuentra ubicada la institución educativa."),
                      showOutput("GraficoUso_Sex", "nvd3"),
                      showOutput("GraficoUso_Age", "nvd3"),
                      showOutput("GraficoUso_NE", "nvd3"),
                      showOutput("GraficoUso_LO", "nvd3"),
                      p(class = "lead", align="justify", "A continuación podrá consultar información del indice multivariado de ambiente escolar para cada institución educativa del proyecto"),
                      
                      
                      sidebarPanel(
                        
                        selectizeInput(
                          'input_type2', 'Aquí puede buscar la institución educativa', choices = unique(UsoR.df$INSTITUCIONEDUCATIVA), multiple = FALSE
                          
                        )
                      ), 
                      chartJSRadarOutput("radar2", width = "450", height = "300"), width = 7,
                      br(), br()
                      
             ),
             tabPanel("Explore las variables",
                      p(class = "lead", align="justify", "En esta sección podrá explorar y cruzar todas las variables que componen el indice multivariado de Uso y Apropiación de TIC"),
                      div(class="intro-divider"),
                      rpivotTableOutput("Uso"),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),


                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br(),
                      br(), br(), br(), br(), br(), br(), br(), br()
             )
             )
         )
)