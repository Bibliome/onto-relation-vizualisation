############################# LIBRARIES ################################


library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(sankeyD3)

################################# UI ###################################

shinyUI(bootstrapPage(
     theme = "styles.css",
     "Hello, world!!!"
))
