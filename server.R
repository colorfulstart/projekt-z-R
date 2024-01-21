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
}
