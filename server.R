#' Load util.R.
source("./R/util.R")

#' Defines the server function for a Shiny application.
server <- function(input, output, session) {

    observe({
        # Update the plot title.
        output$plot_title <- renderText(input$plotname)

        # Source the requested file.
        file_name <- get_file_name(sources[input$plotname])
        source(file_name)

        # Update the Ace editor.
        txt <- readChar(file_name, file.info(file_name)$size)
        updateAceEditor(session, "code", txt, readOnly = TRUE)

        # The name of the function matches the name of the file.
        result <- get(sources[input$plotname])(input, output, session)

        # Update the UI.
        if (length(result$plotly) > 0) {
            # Using plotly. Hide the standard ggplot. Show plotly.
            shinyjs::hide(selector = "#plot")
            shinyjs::show(selector = "#plotly")
            output$plotly <- renderPlotly(result$plotly)
        } else {
            # Using ggplot. Hide plotly. Show the standard ggplot.
            shinyjs::hide(selector = "#plotly")
            shinyjs::show(selector = "#plot")
            output$plot <- renderPlot(result$plot)
        }

        # Update the data and help sections.
        output$data <- renderTable(result$data)
        output$help <- renderText(result$help)
    })
}
