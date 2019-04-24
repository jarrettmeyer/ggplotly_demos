scatterplot <- function (input, output, session) {
    set.seed(123)
    data(diamonds)

    df <- diamonds %>%
        filter(carat <= 3.0) %>%
        sample_n(200)

    p <- ggplot(df, aes(x = carat, y = price, color = cut)) +
        geom_point() +
        scale_color_brewer(name = "Cut", palette = input$colorscheme) +
        scale_y_continuous(labels = comma) +
        xlab("Carats") +
        ylab("Price") +
        get(input$theme)()

    return(list(data = df, plot = p))
}
