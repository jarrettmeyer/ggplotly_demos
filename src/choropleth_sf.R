choropleth_sf <- function (input, output, session) {
    set.seed(123)

    world <- ne_countries(scale = "medium", returnclass = "sf")
    num_countries <- length(unique(world$name))

    p <- ggplot(data = world) +
        geom_sf(aes(fill = pop_est)) +
        scale_fill_distiller(name = "Population", palette = input$colorscheme, labels = comma) +
        ggtitle("World Map", subtitle = paste0("(", num_countries, " countries)")) +
        xlab("Longitude") +
        ylab("Latitude") +
        get(input$theme)()

    # Remove the geometry list from the data set before returning.
    world$geometry <- NULL

    return(list(data = world,
                help = "Creating maps with the sf (simple features for spatial data) library. Data comes from the rnaturalearth library.",
                plot = p))
}
