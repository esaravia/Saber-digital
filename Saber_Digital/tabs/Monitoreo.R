tabPanel(title = "Monitoreo", icon = icon("info-circle"),
  div(class="about",
     tabsetPanel(
        tabPanel("Aprendizaje",
          tabsetPanel(
            tabPanel("Tablero de control",
              h2(class = "outer", "¿Qué encontrará en esta sección?"),
              p(align="justify", "En esta sección encontrará toda la información relacionada con los espacios de
                                  formación realiados en las instituciones eductativas"),
              rpivotTableOutput('DiasDiez')
                        
            )
          )
        )
      )
    )
)