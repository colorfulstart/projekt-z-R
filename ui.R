ui = fluidPage(
  tabsetPanel(
    tabPanel("Dane",
             dataTableOutput("summary")),
    
    tabPanel("Rozkład zmiennej",
             selectInput("one_var",
                         label = "Wybierz zmienną do narysowania",
                         choices = column_names),
             plotOutput("one_var_plot")),
    
    tabPanel("Modelowanie",
             fluidRow(
               column(12, h3("Wartość informacyjna (IV) jest miarą stosowaną w scoringu kredytowym do oceny mocy predykcyjnej zmiennej. Określa, jak dobrze zmienna może przewidzieć zmienną docelową. Wartość IV jest często obliczana dla każdej zmiennej.
Poniżej możesz wybrać dowolną ilość cech, która według Ciebie jest najbardziej znacząca przy tworzeniu modelu. Chcesz aby pole pod krzywą ROC było możliwie największe, a otrzymane statystyki deviance i Aic jak najmniejsze.", style = "font-size: 15px;")),
               column(12, selectInput("kolumny_uzytkownik", "Wybierz kolumny:", choices = column_names, multiple = TRUE)),
               column(12, plotOutput("roc_plot")),
               column(12, verbatimTextOutput("wyniki_modelu"))
             ))
  )
)