############################# LIBRARIES ################################

suppressMessages(library(sankeyD3))
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tidyverse))

################################# UI ###################################

shinyUI(bootstrapPage(
     theme = "styles.css",
     "Hello, world!!!"
))
