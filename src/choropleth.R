suppressPackageStartupMessages({
    library(ggplot2)
    library(maps)
    library(shiny)
})

choropleth <- function (input, output, session) {
    set.seed(123)
    state_map <- map_data("state")

    # Create a fake data set.
    df <- state_map %>%
        select(region) %>%
        distinct() %>%
        mutate(value = sample(100, size = n(), replace = TRUE)) %>%
        inner_join(state_map, by = "region") %>%
        select(-subregion)

    p <- ggplot(df) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = value),
                     color = "white") +
        scale_fill_distiller(name = "Value",
                             palette = input$colorscheme) +
        coord_quickmap() +
        ggtitle("US Map") +
        xlab("Longitude") +
        ylab("Latitude") +
        get(input$theme)() +
        theme(legend.position = "bottom",
              legend.direction = "horizontal")

    return(list(data = df, plot = p))
}
