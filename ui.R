suppressPackageStartupMessages({
    library(shiny)
    library(shinyAce)
})

source("./util.R")

plot_width  <- "640px"
plot_height <- "480px"
selected_plot <- plot_choices[1]

#' Defines the UI for a shiny application.
ui <- fluidPage(

    # Application title
    tags$h1("Plot Demos"),

    fluidRow(
        column(
            3,
            wellPanel(
                selectInput(
                    "plotname",
                    label = "Plot Name",
                    choices = plot_choices,
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
                    choices = palettes,
                    selected = "Set1"
                ),
                helpText("The code for this project is available in ",
                         a("Github", "https://github.com/jarrettmeyer/ggplotly_demos"),
                         ".")
            )
        ), # Closes column(3).
        column(
            9,
            tabsetPanel(
                tabPanel(
                    "Plot",
                    tags$h2(htmlOutput("plot_title")),
                    helpText(
                        textOutput("help")
                    ),
                    plotOutput("plot", width = plot_width, height = plot_height)
                ),
                tabPanel(
                    "Data",
                    tableOutput("data")
                ),
                tabPanel(
                    "Code",
                    aceEditor("code",
                              autoComplete = "disabled",
                              height = plot_height,
                              readOnly = TRUE,
                              showLineNumbers = TRUE)
                )
            ) # Closes tabsetPanel().
        ) #Closes column(9).
    ) # Closes fluidRow().
) # Closes fluidPage().