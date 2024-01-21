server = function(input, output) {
  output[["summary"]] = renderDT({
    dane 
    })
  
  output[["one_var_plot"]] = renderPlot({
    one_var_plot = ggplot(dane, aes_string(x = input[["one_var"]])) +
      theme_bw() +
      xlab(input[["one_var"]])
    if (is.numeric(dane[[input[["one_var"]]]])) {
      one_var_plot + geom_density(color='blue')
    } else {
      one_var_plot + geom_bar(fill='lightblue', fill='black')
    }
  })
  
  model_uzytkownika <- reactive({
    buduj_model(proba_train_2, input[["kolumny_uzytkownik"]], def_waga)
  })
  
  output[["roc_plot"]] = renderPlot({
    model <- model_uzytkownika()
    rysuj_ROC(proba_train_2$DEFAULT, model)
    
  })
  
  output[["wyniki_modelu"]] = renderText({
    model <- model_uzytkownika()
    deviance <- summary(model)$deviance
    aic <- summary(model)$aic
    paste("Deviance:", deviance, "\nAic: ", aic)
  })
}
