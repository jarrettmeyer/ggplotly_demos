survival <- function (input, output, session) {
    set.seed(123)
    num_subjects <- 200

    # Create a data frame.
    df <- data.frame(subject = seq(from = 1, to = num_subjects),
                     risk = sample(c("A", "B"), size = num_subjects, replace = TRUE),
                     event = sample(c(0, 1), size = num_subjects, replace = TRUE),
                     last_date = rep(NA_integer_, times = num_subjects))

    # Assign last date for each risk.
    lambda <- 1 / c(90, 360)
    df[df$risk == "A",]$last_date <- ceiling(rexp(sum(df$risk == "A"), lambda[1]))
    df[df$risk == "B",]$last_date <- ceiling(rexp(sum(df$risk == "B"), lambda[2]))

    # Create a survival fit object.
    fit <- survfit(formula = Surv(last_date, event) ~ risk,
                   data = df)

    # Create the survival plot.
    p <- ggsurvplot(fit,
                    data = df,
                    conf.int = TRUE,
                    palette = input$colorscheme,
                    ggtheme = get(input$theme)(),
                    legend.title = "Risk",
                    legend.labs = c("A", "B"),
                    censor.shape = NA)

    # Make modifications to the survival plot.
    p$plot <- p$plot +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = percent) +
        ggtitle("Kaplan-Meier Survival Plot") +
        xlab("Time (days)") +
        ylab("Overall Survival Probability") +
        theme(legend.position = "bottom")

    return(list(data = df, plot = p$plot))
}
