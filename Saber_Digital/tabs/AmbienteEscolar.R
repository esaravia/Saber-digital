library(shiny)
library(readxl)
require(plyr)
require(rCharts)
require(reshape2)
require(devtools)
library(radarchart)
library(rpivotTable)


options(RCHART_LIB = 'polycharts')
AmbienteEscolar.df<- read_excel("data/AmbienteEscolar.xls")

tabPanel(title = "Ambiente Escolar", icon = icon("info-circle"),
         div(id = "home",
        
             br(),
             tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
             
             tabsetPanel(
               tabPanel("Introducción",
                        tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                        
                        h2(class = "outer", "¿Por qué un indice de ambiente escolar?"),
p(align="justify","Un objetivo recurrente en la política pública educativa de los últimos años es la necesidad de mejorar la calidad educativa a través de la intervención de los recursos físicos y humanos disponibles en las instituciones educativas. En última instancia, estas mejoras buscan crear una sería de condiciones que afectan directa o indirectamente las relaciones entre los actores de la comunidad educativa y que determinan finalmente el Ambiente Escolar en el que se desarrolla el proceso de enseñanza aprendizaje. Ospina et al. (2015) citando a otros autores afirma que este componente determina algunos proceso y resultados educativos y su cuantificación puede aportar información útil para la toma de decisiones y a mejorar la comprensión sobre los aspectos que afectan la calidad educativa."),

p(align="justify","A pesar de esto y al igual que todos los indicadores que incluyen factores multidimensionales, la cuantificación y definición de un índice de Ambiente Escolar es un tema de discusión en la literatura y por ende cuenta con múltiples aproximaciones, instrumentos y metodologías para su medición. Dejando de lado la discusión sobre este último aspecto, este articulo propone la cuantificación de un índice multivariado de ambiente escolar construido bajo la metodología de Análisis Factorial Exploratorio a partir del instrumento diseñado y validado por Ospina et al (2015) para un grupo de 1.336 pertenecientes a 41 instituciones educativas oficiales de la ciudad de Bogotá. La selección de este grupo de instituciones obedece a su participación en el proyecto “Saber Digital” el cual es liderado por la Secretaría de Educación Distrital en asocio con la Universidad EAFIT y que busca transformar los ambientes de aprendizaje a partir del uso de tecnologías digitales."),    

p(align="justify","Los resultados muestran que la muestra cumple con los supuestos psicométricos para la construcción del índice el cual está compuesto por las dimensiones de Desempeño académico, Institucionalización y Liderazgo, Seguridad y Respeto, Educación inclusiva, Ambiente de Aprendizaje, Comunidad de Aprendizaje y Retroalimentación de los resultados, retos y logros con los demás integrantes de la comunidad educativa."),   
br()
                        ),

                tabPanel("En qué consiste",
                         h3(class = "outer", "¿Qué es el indice multivariado de ambiente escolar?"),
p(align="justify", "El indice de ambiente escolar es una medida multivariada que permite determinar el ambiente escolar de una institución educativa con respecto al resto de instituciones educativas que hacen parte del estudio."),
                         h3(class = "outer", "¿Qué elementos componen el indice multivariado de ambiente escolar?"),

p("Los elementos que considera este instrumento son:", align="justify"),

tags$ul(
  tags$li(align="justify", strong("Desempeño academico:"), "Indaga por la percepción del docente frente al nivel de motivación que la IE le brinda al estudiante para cumplir con éxito su formación, a través de tener objetivos académicos rigurosos y coherentes"), 
  br(),
  tags$li(align="justify", strong("Institucionalización y liderazgo:"),"Este factor refleja la percepción de los docentes frente al liderazgo de los directivos en terminos de prácticas institucionales que favorezcan la comunicación y la participación de los integrantes de la comunidad educatva."),
  br(),
  tags$li(align="justify", strong("Seguridad y respeto:"),"Analiza la percepción del docente con relación a la seguridad física y emocional que ofrece la IE. Condiciones que son indispensables para garantizar un buen proceso de aprendizaje su importancia  radica en el enlace directo que hay entre el entorno escolar y el éxito académico."),
  br(),
  tags$li(align="justify", strong("Educacion inclusiva:"), "Desde la perspectiva del docente, se busca determinar si la institución educativa tiene una organización que garantice la inclusión y reconocimiento de diversas culturas, mezcla de sub-grupos, población en condición de discapacidad o estudiantes con dificultades académicas."),
  br(),
  tags$li(align="justify", strong("Ambientes de aprendizaje:"), "Busca establecer si existen condiciones fisicas adecuadas y disponibilidad de materiales y herramientas que faciliten el desarrollo de la labor docente."),
  br(),
  tags$li(align="justify", strong("Comunidades de aprendizaje:"), "Indaga desde la perspectiva del docente el nivel de inclusión, participación y colaboración de la comunidad educativa en la construcción de los modelos y prácticas de enseñanza que se desallorran en la institución educativa."),
  br(),
  tags$li(align="justify", strong("Retroalimentación:"), "Indaga sobre la existencia de un pocesos de retroalimentación de logros y resultados de los estudiantes con los padres de familia."),
  br()  
),
h3(class = "outer", "¿Cuales son las ventajas de indice multivariado de ambiente escolar?"),
p(align="justify", "Los índices multivariados son apropiados cuando se cuenta con una gran cantidad de variables que explican un mismo fenómeno, ya que:"),

tags$ul(
  tags$li("Tienen un alto poder de reducción de variables redundantes y facilitan el diseño y agregación de variables", align="justify"), 
  tags$li("Permiten identificar dimensiones subyacentes a los datos que pueden ser relevantes para la intervención", align="justify"), 
  tags$li("Permite ponderar adecuadamente los atributos que están altamente correlacionados", align="justify"),
  tags$li("Permite la comparabilidad y ordenación entre docentes ", align="justify"),
  tags$li("Permite identificar posibles errores de medición en los instrumentos", align="justify")
)
                        ),

                tabPanel("Cómo se construyó",
                         
                          h2(class = "outer", "¿Cómo se construyó el indice de ambiente escolar?"),
p(align="justify", "Siguiendo la metodología de", strong("Análisis Factorial Exploratorio,"), "para la construcción del indice se emplearon las 65 preguntas que contenia el instrumento aplicado a 1336 docentes de las 41 instituciones educativas que participaron en la fase de caracterización del Plan “Saber Digital”. A partir de esto se realizó la estimación insesgada de la matriz de correlaciones y una vez garantizado esto, se realizarón las pruebas de adecuación muestral (KMO), el test de esfericidad de Bartlett y el determinante de la matriz de correlaciones."),
p(align="justify", "Para el primer caso, el valor estimado del KMO es de 0.949 lo cual indica que existe evidencia estadística sobre un alto grado de varianza común en el grupo de variables a agrupar; por otro lado, el test de esfericidad de Barlett rechaza la hipótesis nula confirmando la existencia de combinaciones lineales en la matriz y el valor del determinante de la matriz da evidencia sobre la pertinencia del método para analizar este grupo de variables.",
"Una vez estimado el AFE mediante el método del factor principal y verificados estos supuestos, se obtuvieron 64 factores de los cuales 7 de ellos cumplen con la regla de Kaiser, es decir, tienen valores propios mayores a uno; estos valores son calculados como la sumatoria de las cargas factoriales al cuadrado y explican el 91.21%% de la varianza conjunta."),
p(align="justify", " Una vez garantizado esto, se realiza la rotación oblicua y se calculan las predicciones lineales de cada uno de los 7 factores seleccionados para cada uno de los cuales se realizó el test de normalidad de Jarque Bera y se rechazó la hipótesis nula de no normalidad en todos los factores seleccionados."),
p(img(src='Plot2.png', align = "center", width = 500)),
p(align="justify", "La nomenclatura e interpretación de los factores está fundamentada en la observación de la correlación de cada uno de estos con las variables originales y se deben identificar las variables cuyas correlaciones con el factor son elevadas, acorde a la literatura, se definió considerar como significativos los coeficientes de correlación superiores a 0.3. Una vez validados los supuestos, se calcularon las puntuaciones factoriales de cada uno de los constructos derivados del AFE empleando el método de regresión."),
p(align="justify", "Finalmente y con el objetivo de mejorar la interpretación del indice, se hizó un rescalamiento entre 0 y 10 para lo cual, para esto se calculó la diferencia entre el valor del indice y el mínimo de su distribución, se divide por el rango y se multiplica por 10.",
  "De esta forma, el siguiente gráfico muestra la función de distribución del Índice Multivariado con el nuevo rango lo cual facilita su interpretación y lectura. El valor de este índice resultante representa el nivel de ambiente escolar según la autopercepción de los docentes y su valor es relativo a los otros docentes que realizaron la encuesta, es decir, provee solo mediciones relativas a la muestra y no permite extraer información en niveles absolutos"),
p(img(src='Plot3.png', align = "center", width = 400)),

br()
                         ),
                
                tabPanel("Resultados",
                         h2(class = "outer", "Resultados generales del indice"),
                         p(class = "lead", align="justify", "En esta sección encontrará los resultados generales del indice multivariado de ambiente escolar, adicionalmente, podrá consultar la información para cada institución educativa que participá del proyecto"),
                         div(class="intro-divider"),
                           tags$ul(
                             tags$li(align="justify", strong("Factor 1 (Desempeño academico):"), "Indaga por la percepción del docente frente al nivel de motivación que la IE le brinda al estudiante para cumplir con éxito su formación, a través de tener objetivos académicos rigurosos y coherentes"), 
                             br(),
                             tags$li(align="justify", strong("Factor 2 (Institucionalización y liderazgo):"),"Este factor refleja la percepción de los docentes frente al liderazgo de los directivos en terminos de prácticas institucionales que favorezcan la comunicación y la participación de los integrantes de la comunidad educatva."),
                             br(),
                             tags$li(align="justify", strong("Factor 3 (Seguridad y respeto):"),"Analiza la percepción del docente con relación a la seguridad física y emocional que ofrece la IE. Condiciones que son indispensables para garantizar un buen proceso de aprendizaje su importancia  radica en el enlace directo que hay entre el entorno escolar y el éxito académico."),
                             br(),
                             tags$li(align="justify", strong("Factor 4 (Educacion inclusiva):"), "Desde la perspectiva del docente, se busca determinar si la institución educativa tiene una organización que garantice la inclusión y reconocimiento de diversas culturas, mezcla de sub-grupos, población en condición de discapacidad o estudiantes con dificultades académicas."),
                             br(),
                             tags$li(align="justify", strong("Factor 5 (Ambientes de aprendizaje):"), "Busca establecer si existen condiciones fisicas adecuadas y disponibilidad de materiales y herramientas que faciliten el desarrollo de la labor docente."),
                             br(),
                             tags$li(align="justify", strong("Factor 6 (Comunidades de aprendizaje):"), "Indaga desde la perspectiva del docente el nivel de inclusión, participación y colaboración de la comunidad educativa en la construcción de los modelos y prácticas de enseñanza que se desallorran en la institución educativa."),
                             br(),
                             tags$li(align="justify", strong("Factor 7 (Retroalimentación):"), "Indaga sobre la existencia de un pocesos de retroalimentación de logros y resultados de los estudiantes con los padres de familia."),
                             br()  
                           ),
                        p(align="justify", "A continuación podrá consultar los resultados según el sexo, rango de edad y máximo nivel educativo del docente y según la localidad donde se encuentra ubicada la institución educativa."),

                         
                        showOutput("GraficoAE_Sex", "nvd3"),
                        showOutput("GraficoAE_Age", "nvd3"),
                        showOutput("GraficoAE_lev", "nvd3"),
                        showOutput("GraficoAE", "nvd3"),
            
                        p(class = "lead", align="justify", "A continuación podrá consultar información del indice multivariado de ambiente escolar para cada institución educativa del proyecto"),


                        
                        sidebarPanel(
                          
                            selectizeInput(
                              'input_type', 'Aquí puede buscar la institución educativa', choices = unique(AmbienteEscolar.df$INSTITUCIONEDUCATIVA), multiple = FALSE
                            
                                                      )
                          ), 
                        chartJSRadarOutput("radar", width = "450", height = "300"), width = 7,
                        br(), br()
                        ),

                tabPanel("Explore las variables",
                         p(class = "lead", align="justify", "En esta sección podrá explorar y cruzar todas las variables que componen el indice multivariado de Ambiente Escolar"),
                         div(class="intro-divider"),
                         rpivotTableOutput("AmbienteEscolar"),
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