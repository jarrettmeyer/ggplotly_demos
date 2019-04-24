suppressPackageStartupMessages({
    library(RColorBrewer)
    library(shiny)
    library(tidyverse)
})

bar_chart <- function (input, output, session) {
    set.seed(123)

    # Create a data set.
    count_rows <- 200
    group <- c("A", "B", "C", "D")
    time <- c(1, 2, 3, 4, 5)
    df <- data.frame(group = sample(group, size = count_rows, replace = TRUE),
                     time = sample(time, size = count_rows, replace = TRUE),
                     value = rnorm(count_rows, mean = 20, sd = 2)) %>%
        group_by(group, time) %>%
        summarise(value = sum(value)) %>%
        ungroup()

    p <- ggplot(df, aes(x = time, y = value, fill = group)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(name = "Group", palette = input$colorscheme) +
        ggtitle("Bar Chart") +
        xlab("Time") +
        ylab("Value") +
        get(input$theme)()

    return(list(data = df,
                help = paste0("A simple stacked bar chart with ", length(group), " groups. This data has been randomly generated."),
                plot = p))
}
