#### Shiny App Basic set up

# 2/9/24

# Libraries
library(shiny)
library(tidyverse)


############# USER INTERFACE ########################################

ui <- fluidPage(
  titlePanel("Historic Land Use Changes and Fires for the Islands of Hawai'i"),
  sidebarLayout(
    sidebarPanel("Widget 1 here"),
    mainPanel("Output of Widget 1 here")
  ) ### End of sidebar layout
)### End of fluidPage function




############ SERVER FUNCTION #########################################

server <- function(input, output) {}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










