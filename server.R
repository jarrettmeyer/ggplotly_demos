suppressPackageStartupMessages({
    library(ggthemes)
    library(RColorBrewer)
    library(shiny)
})

#' Load util.R.
source("./util.R")

#' Defines the server function for a Shiny application.
server <- function(input, output, session) {

    output$plot_title <- renderText(input$plotname)

    observe({
        file_name <- get_file_name(sources[input$plotname])
        txt <- readChar(file_name, file.info(file_name)$size)
        updateAceEditor(session, "code", txt, readOnly = TRUE)
    })

    observe({
        # Source the requested file.
        file_name <- get_file_name(sources[input$plotname])
        source(file_name)

        # The name of the function matches the name of the file.
        result <- get(sources[input$plotname])(input, output, session)

        # Update the UI.
        output$plot <- renderPlot(result$plot)
        output$data <- renderTable(result$data)
        output$help <- renderText(result$help)
    })
}
