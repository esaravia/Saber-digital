# Plot libraries for outputs
require(rCharts)
require(leaflet)
require(DT)
require(plotly)
library(radarchart)

options(RCHART_LIB = 'polycharts')

shinyUI(  
  navbarPage(title = strong("Saber Digital"), windowTitle = "Saber Digital - Fortalecimiento de ambientes de aprendizaje con uso de tecnologias digitales", 
             fluid = TRUE, footer = includeHTML("tools/footer.html"), id = "nav",
             source("tabs/Inicio.R", local = TRUE)$value,
             #source("tabs/Monitoreo.R", local = TRUE)$value
             source("tabs/AmbienteEscolar.R", local = TRUE)$value,
             source("tabs/UsoTIC.R", local = TRUE)$value,
             source("tabs/InstitucionEducativa.R", local = TRUE)$value
             
             
             )
)