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
               column(12, selectInput("kolumny_uzytkownik", "Wybierz kolumny:", choices = column_names_bez_default, multiple = TRUE)),
               column(12, plotOutput("roc_plot")),
               column(12, verbatimTextOutput("wyniki_modelu"))
             )),
    
    tabPanel("IV 10 zmiennych",
             fluidRow(
               column(4, 
                      h3("Najlepsze dopasowanie"),
                      dataTableOutput("IV_10_max_output")),
               column(4, 
                      h3("Losowy wybór zmiennych"),
                      dataTableOutput("IV_10_los_output")),
               column(4, 
                      h3("Najgorsze dopasowanie"),
                      dataTableOutput("IV_10_min_output")),
               
               column(4,
                      verbatimTextOutput("znaczace_max")),
               column(4,
                      verbatimTextOutput("znaczace_los")),
               column(4,
                      verbatimTextOutput("znaczace_min")),
               
               column(4,
                      plotOutput("roc_plot_max")),
               column(4,
                      plotOutput("roc_plot_los")),
               column(4,
                      plotOutput("roc_plot_min"))
             )
    ),
    
    tabPanel("Szansa na dostanie kredytu",
             fluidRow(
               column(12, h3("Na podstawie dotychczasowych danych zebranych przez nasz bank jesteśmy w stanie oszacować Twoje prawdopodobieństwo na uzyskanie kredytu. Po wybraniu odpowiednich wartości i wybraniu przycisku 'Sprawdź'.", style = "font-size: 15px;")),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("wnioskowana_kwota", "Wnioskowana kwota:", choices = unique(dane$Wnioskowana_kw_kat)),
                   selectInput("dochod", "Dochód:", choices = unique(dane$Dochod_kat)),
                   selectInput("wyksztalcenie", "Wykształcenie:", choices = unique(dane$WYKSZTAlCENIE)),
                   selectInput("sektor", "Sektor zatrudnienia:", choices = unique(dane$SEKTOR)),
                   
                   actionButton("sprawdz", "Sprawdź")
                 ),
                 
                 mainPanel(
                   textOutput("wynik")
                 )
               )
             )),
    
    
  
  
    
  )
)
