suppressPackageStartupMessages({
    library(ggthemes)
    library(maps)
    library(plotly)
    library(RColorBrewer)
    library(scales)
    library(shiny)
    library(tidyverse)
})


plotWidth <- "100%"
plotHeight <- "800px"
themes <- c("theme_bw", "theme_calc", "theme_classic", "theme_dark", "theme_economist",
            "theme_few", "theme_fivethirtyeight", "theme_gdocs", "theme_gray",
            "theme_light", "theme_linedraw", "theme_minimal", "theme_tufte",
            "theme_void", "theme_wsj")


# Define the UI.
ui <- fluidPage(

    # Application title
    titlePanel("GGPlot Demos"),

    fluidRow(
        column(3,
            wellPanel(
                selectInput("plotname",
                            label = "Plot Name",
                            choices = c("Bar Chart",
                                        "Choropleth",
                                        "Histogram",
                                        "Scatterplot"),
                            selected = "Bar Chart"),
                selectInput("theme",
                            label = "Theme",
                            choices = themes,
                            selected = "theme_gray"),
                selectInput("colorscheme",
                            label = "Color Scheme",
                            choices = sort(rownames(brewer.pal.info)),
                            selected = "Set1")
            )
        ),
        column(
            9,
            tabsetPanel(
                tabPanel("GGPlot", plotOutput("ggplot", width = plotWidth, height = plotHeight)),
                tabPanel("Plotly", plotlyOutput("plotly", width = plotWidth, height = plotHeight))
            )
        )
    )
)

# Define the server.
server <- function(input, output) {

    output$ggplot <- renderPlot(
        switch(
            input$plotname,
            "Bar Chart" = {
                group <- rep(c("A", "B", "C", "D"), each = 4)
                time <- rep(c(1, 2, 3, 4), times = 4)
                value <- rnorm(16, mean = 20, sd = 2)

                df <- data.frame(group, time, value)

                ggplot(df, aes(x = time, y = value, fill = group)) +
                    geom_bar(stat = "identity") +
                    scale_fill_brewer(name = "Group", palette = input$colorscheme) +
                    ggtitle("Bar Chart") +
                    xlab("Time") +
                    ylab("Value") +
                    get(input$theme)()
            },
            "Choropleth" = {
                state_map <- map_data("state")

                # Create a fake data set.
                data <- state_map %>%
                    select(region) %>%
                    distinct() %>%
                    mutate(value = sample(100, size = n(), replace = TRUE))

                choropleth <- state_map %>%
                    inner_join(data, by = "region") %>%
                    arrange(group, order)

                ggplot(choropleth) +
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
            },
            "Histogram" = {
                count <- 10000

                # Create a random data frame.
                df <- data.frame(x = rnorm(count))

                # Create a ggplot object.
                ggplot(df, aes(x = x)) +
                    geom_histogram(stat = "bin", binwidth = 0.25, aes(fill = ..count..)) +
                    scale_fill_distiller(name = "Count", palette = input$colorscheme) +
                    scale_y_continuous(labels = comma) +
                    ggtitle("Histogram") +
                    xlab("Value") +
                    get(input$theme)() +
                    theme(axis.title.y = element_blank())
            },
            "Scatterplot" = {
                count <- 500

                df <- data.frame(
                    x = rexp(count, rate = 20),
                    y = rexp(count, rate = 10),
                    cat = sample(c("A", "B", "C", "D", "E"), size = count, replace = TRUE)
                )

                # Create a ggplot object.
                ggplot(df, aes(x = x, y = y, color = cat)) +
                    geom_point() +
                    scale_color_brewer(name = "Category",
                                       palette = input$colorscheme) +
                    ggtitle("Scatterplot") +
                    xlab("X") +
                    ylab("Y") +
                    get(input$theme)()
            }
        )
    )

    # Draw the selected plot.
    output$plotly <- renderPlotly({
        switch(
            input$plotname,
            {
                plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width)
            }
        )
    })
}

# Run the application.
shinyApp(ui = ui,
         server = server)
