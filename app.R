suppressPackageStartupMessages({
    library(plotly)
    library(RColorBrewer)
    library(scales)
    library(shiny)
    library(tidyverse)
})


# Define the UI.
ui <- fluidPage(

    # Application title
    titlePanel("GGPlot Demos"),

    fluidRow(
        column(3,
            wellPanel(
                selectInput("plotname",
                            label = "Plot Name",
                            choices = c("Choropleth",
                                        "Histogram",
                                        "Scatterplot"),
                            selected = "Scatterplot"),
                selectInput("colorscheme",
                            label = "Color Scheme",
                            choices = sort(rownames(brewer.pal.info)),
                            selected = "Set1")
            )
        ),
        column(9,
            plotlyOutput("plot", height = "800px")
        )
    )
)

# Define the server.
server <- function(input, output) {

    # Draw the selected plot.
    output$plot <- renderPlotly({
        # Initialize the my_plotly object to NULL.
        my_plotly <- NULL
        switch(input$plotname,
               "Choropleth" = {

                   # Create a fake data set.
                   data <- map_data("state") %>%
                       select(region) %>%
                       distinct() %>%
                       mutate(value = sample(100, size = n(), replace = TRUE))

                   choropleth <- map_data("state") %>%
                       inner_join(data, by = "region") %>%
                       arrange(group, order)

                   p <- ggplot(choropleth) +
                       geom_polygon(aes(x = long, y = lat, group = group, fill = value),
                                color = "white") +
                       scale_fill_distiller(name = "Value",
                                            palette = input$colorscheme) +
                       coord_quickmap() +
                       theme_minimal() +
                       ggtitle("US with Random Values") +
                       xlab("Longitude") +
                       ylab("Latitude") +
                       theme(legend.position = "bottom",
                             legend.direction = "horizontal")

                   my_plotly <- ggplotly(p)
               },
               "Histogram" = {
                   count <- 10000

                   # Create a random data frame.
                   df <- data.frame(x = rnorm(count))

                   # Create a ggplot object.
                   p <- ggplot(df, aes(x = x)) +
                       geom_histogram(stat = "bin", binwidth = 0.25, aes(fill = ..count..)) +
                       scale_fill_distiller(name = "Count", palette = input$colorscheme) +
                       scale_y_continuous(labels = comma) +
                       xlab("Value") +
                       ggtitle("Histogram of Random Normal Data (m = 0, sd = 1)")

                   # Create a plotly object.
                   my_plotly <- ggplotly(p) %>%
                       style(hoverinfo = "text",
                             text = "count")
               },
               "Scatterplot" = {
                   # Add text to the iris data frame.
                   iris <- iris %>%
                       mutate(Text = paste0("Width: ", Petal.Width, "<br>Length: ", Petal.Length, "<br>Species: ", Species))

                   # Create a ggplot object.
                   p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
                       geom_point() +
                       scale_color_brewer(palette = input$colorscheme) +
                       ggtitle("Petal Length vs. Petal Width") +
                       xlab("Petal Width") +
                       ylab("Petal Length")

                   # Create a plotly object.
                   my_plotly <- ggplotly(p) %>%
                       style(hoverinfo = "text",
                             text = iris$Text)
               }
        )
        return(my_plotly)
    })
}

# Run the application.
shinyApp(ui = ui,
         server = server)
