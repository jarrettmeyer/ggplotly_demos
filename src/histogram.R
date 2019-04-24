histogram <- function (input, output, session) {
    set.seed(123)
    count <- 1000

    # Create a random data frame.
    df <- data.frame(x = rnorm(count))

    # Create a ggplot object.
    p <- ggplot(df, aes(x = x)) +
        geom_histogram(stat = "bin", binwidth = 0.25, aes(fill = ..count..)) +
        scale_fill_distiller(name = "Count", palette = input$colorscheme) +
        scale_y_continuous(labels = comma) +
        ggtitle("Histogram") +
        xlab("Value") +
        get(input$theme)() +
        theme(axis.title.y = element_blank())

    return(list(data = df, plot = p))
}
