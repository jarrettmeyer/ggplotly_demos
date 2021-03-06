#' Load necessary libraries.
library(gapminder)
library(ggthemes)
library(maps)
library(plotly)
library(RColorBrewer)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(sf)
library(shiny)
library(shinyAce)
library(shinyjs)
library(survminer)
library(survival)
library(tidyverse)


#' Define a list of available palettes. Based on palettes
#' available as part of the RColorBrewer package.
palettes <- sort(rownames(brewer.pal.info))

#' Define sources for each plot type.
sources <- c("Bar Chart"       = "bar_chart",
             "Bubble Plot"     = "bubble_plot",
             "Choropleth"      = "choropleth",
             "Choropleth (SF)" = "choropleth_sf",
             "Error Bars"      = "error_bars",
             "Histogram"       = "histogram",
             "Regression"      = "regression",
             "Scatterplot"     = "scatterplot",
             "Scatterplot 2"   = "scatterplot_2",
             "Shapes"          = "shapes",
             "Survival"        = "survival")

#' Define the selected plot. This is very useful for testing new plots.
selected_plot <- "Scatterplot 2"

#' List of available themes.
themes <- c("theme_bw",
            "theme_calc",
            "theme_classic",
            "theme_dark",
            "theme_economist",
            "theme_few",
            "theme_fivethirtyeight",
            "theme_gdocs",
            "theme_gray",
            "theme_light",
            "theme_linedraw",
            "theme_minimal",
            "theme_tufte",
            "theme_void",
            "theme_wsj")

#' Get all chart choices.
plot_choices <- sort(names(sources))

#' Get a file name for the given source.
#'
#' @param src Source file to load.
#' @examples
#' get_file_name("bar_chart")
get_file_name <- function (src) {
    return(paste0("./R/", src, ".R"))
}


#' Load all source files. This is necessary for server.R.
#'
#' @examples
#' load_sources()
load_sources <- function () {
    for (x in sources) {
        source(get_file_name(x))
    }
    rm(x)
    return(sources)
}
