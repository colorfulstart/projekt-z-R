ui = fluidPage(
  tabsetPanel(
    tabPanel("Dane",
             dataTableOutput("summary")),
    
    tabPanel("Rozkład zmiennej",
             selectInput("one_var",
                         label = "Wybierz zmienną do narysowania",
                         choices = column_names),
             plotOutput("one_var_plot"))    
  )
)