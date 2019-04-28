error_bars <- function (input, output, session) {

    # Create our data frame. Add columns for n, mean, sd, and se.
    df <- ToothGrowth %>%
        group_by(supp, dose) %>%
        summarise(n = n(),
                  mean = mean(len),
                  sd = sd(len)) %>%
        ungroup() %>%
        mutate(se = sd / sqrt(n))

    y_breaks <- 0:20 * 4

    pd <- position_dodge(0.1)

    p <- ggplot(df, aes(x = dose, y = mean, color = supp, group = supp)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = "black", width = 0.1, position = pd) +
        geom_line(position = pd) +
        geom_point(position = pd, size = 3, shape = 21, fill = "white") +
        scale_color_brewer(palette = input$colorscheme,
                           name = "Supplement",
                           breaks = c("OJ", "VC"),
                           labels = c("Orange Juice", "Ascorbic Acid")) +
        scale_y_continuous(breaks = y_breaks) +
        expand_limits(y = 0) +
        get(input$theme)() +
        ggtitle("Tooth Growth in Guinea Pigs") +
        xlab("Dose") +
        ylab("Tooth Length") +
        theme(legend.justification = c(1, 0),
              legend.position = c(1, 0))

    return(list(data = df,
                help = 'A graph with error bars representing the S.E. of the mean. This is example is from <a href="http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/">R-Cookbook</a>.',
                plot = p))
}
