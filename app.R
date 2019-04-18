suppressPackageStartupMessages({
    library(gapminder)
    library(ggthemes)
    library(maps)
    library(plotly)
    library(RColorBrewer)
    library(scales)
    library(shiny)
    library(survival)
    library(survminer)
    library(tidyverse)
})


plot_width <- "100%"
plot_height <- "800px"
plots <- c("Bar Chart", "Bubble Plot", "Choropleth", "Histogram", "Scatterplot", "Shapes", "Survival Plot")
selected_plot <- "Bubble Plot"
themes <- c("theme_bw", "theme_calc", "theme_classic", "theme_dark", "theme_economist",
            "theme_few", "theme_fivethirtyeight", "theme_gdocs", "theme_gray",
            "theme_light", "theme_linedraw", "theme_minimal", "theme_tufte",
            "theme_void", "theme_wsj")

# Define the UI.
ui <- fluidPage(

    # Application title
    titlePanel("GGPlot Demos"),

    fluidRow(
        column(
            3,
            wellPanel(
                selectInput(
                    "plotname",
                    label = "Plot Name",
                    choices = plots,
                    selected = selected_plot
                ),
                selectInput(
                    "theme",
                    label = "Theme",
                    choices = themes,
                    selected = "theme_gray"
                ),
                selectInput(
                    "colorscheme",
                    label = "Color Scheme",
                    choices = sort(rownames(brewer.pal.info)),
                    selected = "Set1"
                ),
                helpText("The code for this project is available in ",
                         a("Github", href="https://github.com/jarrettmeyer/ggplotly_demos"),
                         ".")
            )
        ),
        column(
            9,
            tabsetPanel(
                tabPanel("GGPlot", plotOutput("ggplot", width = plot_width, height = plot_height)),
                tabPanel("Plotly", plotlyOutput("plotly", width = plot_width, height = plot_height))
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
            "Bubble Plot" = {
                max_year = max(gapminder$year)

                # Create a data frame.
                df <- gapminder %>%
                    mutate(pop = as.numeric(pop),
                           pop = pop / 1e6) %>%
                    filter(year == max_year)

                # Create a ggplot object.
                ggplot(df, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
                    geom_point(alpha = 0.7, shape = 16) +
                    scale_color_brewer(name = "Continent", palette = input$colorscheme) +
                    scale_size_continuous(name = "Population (M)", labels = comma, range = c(5, 20)) +
                    scale_x_continuous(labels = comma) +
                    scale_y_continuous() +
                    ggtitle(paste0("Bubble Plot for Gapminder Data, year = ", max_year)) +
                    xlab("Gross Domestic Product") +
                    ylab("Life Expectancy") +
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
                    cat = sample(c("A", "B", "C", "D", "E"), size = count, replace = TRUE))

                # Create a ggplot object.
                ggplot(df, aes(x = x, y = y, color = cat)) +
                    geom_point() +
                    scale_color_brewer(name = "Category",
                                       palette = input$colorscheme) +
                    ggtitle("Scatterplot") +
                    xlab("X") +
                    ylab("Y") +
                    get(input$theme)()
            },
            "Shapes" = {
                df <- data.frame(shape = 0:24)

                ggplot(df, aes(x = 0, y = 0)) +
                    geom_point(aes(shape = shape), size = 6, fill = brewer.pal(1, input$colorscheme)[1]) +
                    scale_shape_identity() +
                    ggtitle("Available Shapes in GGPlot") +
                    get(input$theme)() +
                    theme(axis.title = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid = element_blank()) +
                    facet_wrap(~shape)
            },
            "Survival Plot" = {
                num_subjects <- 1000

                # Create a data frame.
                subject <- seq(from = 1, to = num_subjects)
                risk <- c("A", "B")
                lambda <- 1 / runif(2, min = 90, max = 360)
                df <- data.frame(subject,
                                 risk = sample(risk, size = num_subjects, replace = TRUE),
                                 event = sample(c(0, 1), size = num_subjects, replace = TRUE),
                                 last_date = rep(NA, times = num_subjects))

                # Assign last date for each risk.
                df[df$risk == "A",]$last_date <- ceiling(rexp(sum(df$risk == "A"), lambda[1]))
                df[df$risk == "B",]$last_date <- ceiling(rexp(sum(df$risk == "B"), lambda[2]))

                # Create a survival fit object.
                fit <- survfit(Surv(last_date, event) ~ risk, data = df)

                # Create the survival plot.
                p <- ggsurvplot(fit,
                                conf.int = TRUE,
                                palette = input$colorscheme,
                                ggtheme = get(input$theme)(),
                                legend.title = "Risk",
                                legend.labs = risk,
                                censor.shape = NA)

                # Make modifications to the survival plot.
                p$plot <- p$plot +
                    scale_x_continuous(labels = comma) +
                    scale_y_continuous(labels = percent) +
                    ggtitle("Kaplan-Meier Survival Plot") +
                    xlab("Time (days)") +
                    ylab("Overall Survival Probability") +
                    theme(legend.position = "bottom")

                return(p$plot)
            }
        )
    )

    # Draw the selected plot.
    output$plotly <- renderPlotly({
        switch(
            input$plotname,
            "Scatterplot" = {
                count <- 500

                df <- data.frame(
                    x = rexp(count, rate = 20),
                    y = rexp(count, rate = 10),
                    cat = sample(c("A", "B", "C", "D", "E"), size = count, replace = TRUE))
                plot_ly(data = df, x = ~x, y = ~y)
            }
        )
    })
}

# Run the application.
shinyApp(ui = ui,
         server = server)
