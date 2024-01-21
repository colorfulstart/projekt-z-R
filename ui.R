ui = fluidPage(
  tabsetPanel(
    tabPanel("Dane",
             dataTableOutput("summary"))    
  )
)
