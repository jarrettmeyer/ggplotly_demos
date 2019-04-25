scatterplot_2 <- function (input, output, session) {

    df <- iris

    url <- "https://plot.ly/r/line-and-scatter/"
    help <- paste0('This is a Scatterplot created with <a href="',
                   url,
                   '">Plotly</a>. Plotly does use the selected color scheme, ',
                   'but does not use themes.')

    p <- plot_ly(data = df,
                 type = "scatter",
                 mode = "markers",
                 x = ~Sepal.Length, y = ~Petal.Length, color = ~Species,
                 colors = input$colorscheme,
                 marker = list(size = 10,
                               opacity = 0.6,
                               line = list(opacity = 0.8,
                                           width = 2))) %>%
        layout(title = "Scatterplot",
               xaxis = list(zeroline = FALSE),
               yaxis = list(zeroline = FALSE))

    return(list(data = df,
                help = help,
                plotly = p))
}
