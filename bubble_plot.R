suppressPackageStartupMessages({
    library(gapminder)
    library(ggplot2)
})

bubble_plot <- function (input, output, session) {
    max_year <- max(gapminder$year)

    # Create a data frame.
    df <- gapminder %>%
        mutate(pop = as.numeric(pop),
               pop = pop / 1e6) %>%
        filter(year == max_year)

    # Create a ggplot object.
    p <- ggplot(df, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
        geom_point(alpha = 0.7, shape = 16) +
        scale_color_brewer(name = "Continent", palette = input$colorscheme) +
        scale_size_continuous(name = "Population (M)", labels = comma, range = c(5, 20)) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous() +
        ggtitle(paste0("Bubble Plot for Gapminder Data, year = ", max_year)) +
        xlab("Gross Domestic Product") +
        ylab("Life Expectancy") +
        get(input$theme)()

    return(list(data = df,
                help = "A bubble plot allows you to put a third (or fourth) dimension on a two dimensional plot. The size of the bubble encapsulates the third dimension. Data is based on the gapminder data set.",
                plot = p))
}
