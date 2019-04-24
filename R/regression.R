regression <- function (input, output, session) {
    set.seed(123)
    data(diamonds)
    conf_bounds <- qnorm(c(0.025, 0.975))

    df <- diamonds %>%
        filter(carat <= 3.0) %>%
        sample_n(200)

    fit <- lm(price ~ carat, data = df)
    pred <- predict(fit, se.fit = TRUE)

    df$pred <- pred$fit
    df$min_pred <- pred$fit + conf_bounds[1] * pred$se.fit
    df$max_pred <- pred$fit + conf_bounds[2] * pred$se.fit

    fit_sum <- summary(fit)
    r_sq <- fit_sum$r.squared
    adj_r_sq <- fit_sum$adj.r.squared

    p <- ggplot(df, aes(x = carat, y = price)) +
        geom_point() +
        geom_smooth(method = "lm") +
        annotate(geom = "text", label = paste0("italic(R)^2 == ", round(r_sq, 4)), parse = TRUE,
                 x = 0.5, y = 18000, hjust = "inward") +
        annotate(geom = "text", label = paste0("italic(Adj~R)^2 == ", round(adj_r_sq, 4)), parse = TRUE,
                 x = 0.5, y = 17000, hjust = "inward") +
        scale_y_continuous(labels = comma) +
        xlab("Carats") +
        ylab("Price") +
        get(input$theme)()

    return(list(data = df, plot = p))
}
