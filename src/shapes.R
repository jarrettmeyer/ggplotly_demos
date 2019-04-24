shapes <- function (input, output, session) {
    df <- data.frame(shape = 0:24)

    p <- ggplot(df, aes(x = 0, y = 0)) +
        geom_point(aes(shape = shape), size = 6, fill = brewer.pal(1, input$colorscheme)[1]) +
        scale_shape_identity() +
        ggtitle("Available Shapes in GGPlot") +
        get(input$theme)() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank()) +
        facet_wrap(~shape)

    return(list(data = df, plot = p))
}
