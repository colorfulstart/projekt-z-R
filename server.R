server = function(input, output) {
  output[["summary"]] = renderDataTable({
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
  
  output[["IV_10_max_output"]] = renderDataTable({
    IV_max_10[,1, drop=FALSE]
  })
  
  output[["IV_10_min_output"]] = renderDataTable({
    IV_min_10[,1, drop=FALSE]
  })
  
  output[["IV_10_los_output"]] = renderDataTable({
    tabela = t(data.frame(losowe_kolumny))
    rownames(tabela) = c()
    tabela
  })
  
  output[["znaczace_max"]] = renderText({
    paste("Deviance:", round(max_1_znaczaca,2), "AIC: ", round(max_2_znaczaca,2))
  })
  
  output[["znaczace_min"]] = renderText({
    paste("Deviance:", round(min_1_znaczaca,2), "AIC: ", round(min_2_znaczaca,2))
  })
  
  output[["znaczace_los"]] = renderText({
    paste("Deviance:", round(los_1_znaczaca,2), "AIC: ", round(los_2_znaczaca,2))
  })
  
  output[["roc_plot_max"]] = renderPlot({
    rysuj_ROC(proba_train_2$DEFAULT, model_max)
  })
  
  output[["roc_plot_min"]] = renderPlot({
    rysuj_ROC(proba_train_2$DEFAULT, model_min)
  })
  
  output[["roc_plot_los"]] = renderPlot({
    rysuj_ROC(proba_train_2$DEFAULT, model_los)
  })
  
  observeEvent(input$sprawdz, {
    output[["wynik"]] = renderText({
      wylicz_zero_jeden(
        war_wnioskowana_kwota = input$wnioskowana_kwota,
        war_dochod = input$dochod, 
        war_wyksztalcenie = input$wyksztalcenie,
        war_sektor = input$sektor
      )
    })
  })
}
