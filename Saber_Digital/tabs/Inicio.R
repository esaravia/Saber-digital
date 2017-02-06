library(devtools)
library(leaflet)

# UI-elements for Home tab
tabPanel(title = "Home", icon = icon("home"),
         div(id = "home",
             div(class="pull-right",
                 span(class='st_twitter', displayText='Tweet'),
                 span(class='st_facebook', displayText='Facebook'),
                 span(class='st_email', displayText='Email')
             ),
             br(),
             tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
             a(href ="http://www.educacionbogota.edu.co/",div(class="simple-ss",id="simple-ss")),
             br(),
             p(class = "lead", align="justify", "Bienvenidos al sitio de ", strong("monitoreo y evaluación"),"del proyecto: en este espacio encontrá información que les sera útil para la toma de decisiones."),
             div(class="intro-divider"),             
             h2(class = "outer", "¿De qué se trata el proyecto?"),
             p(align="justify", "La ciudad de Bogota, D.C. en su Plan de Desarrollo 2016 - 2019 establece una serie de programas que 
               buscan garantizar el derecho a una educación de calidad que brinde oportunidades de aprendizaje para la 
               vida y ofrezca a todos los niños, niñas y jovenes  de la ciudad, el desarrollo de competencias básicas, 
               el entorno y protagonistas del progreso y desarrollo de la ciudad. Además reconoce a sus maestros, maestras 
               y directivos como líderes de transformación educativa que cumplen un rol fundamental en el proceso formativo
               de los estudiantes."),

             p(align="justify", "En esta línea, la Secretaría de Educación del Distrito y la Universidad EAFIT establecen un contrato de ciencia
                          y tecnología que tiene como objeto la transferencia del modelo de aprendizaje y gestión UbiTAG en instituciones 
               educativas distritales, como parte del fortalecimiento y acompañamiento en la implementación de estrategias que
               aporten al mejoramiento de los ambientes de aprendizaje y del conocimiento, promoviendo el desarrollo de 
               capacidades en el uso inteligente de las TIC."),             
             
            
            h2(class = "outer", "¿Qúe estrategias se desarrollan?"),
             
             tags$ul(
               tags$li("Direccionamiento estratégico, el cual permite acompañar a la Secretaría de Educación del Distrito en la 
                       construcción de un plan de apropiación social del proyecto y un proceso de caracterización de las instituciones
                       en el uso y apropiación de las TIC", align="justify"), 
               tags$li("Transferencia del modelo UbiTAG", align="justify"), 
               tags$li("Conformación de una Red de tutores TIC que acompañan y promueven el fortalecimiento de los ambientes de aprendizaje 
                       con el uso de tecnologías de información y comunicación", align="justify")
             ),
            h2(class = "outer", "¿Qué instituciones educativas participan?"),
            
               p(align="justify", "La primera fase se desarrolla en 40 instituciones educativas de la ciudad"),             
               leafletOutput("mymap"),
             br()
               
               
               
             
         )
)
