#### Shiny App Basic set up

# 2/9/24

# Libraries
library(shiny)
library(tidyverse)
library(here)
library(tmap)
library(broom)
library(sf)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(bslib)

############# USER INTERFACE ########################################

ui <- fluidPage(
  theme = bs_theme(bootswatch = 'spacelab'),
  titlePanel("Land Use Changes and Fires for the Islands of Hawai'i"),
  tabsetPanel(

    tabPanel(
      title = 'Introduction',
      p('Introduction text')
    ), ################################################ END OF TAB 1

    tabPanel(
      title = "Fires Map",
      fluidPage(
        fluidRow(
          column(width = 4,
                 selectInput("select", label = h3("Choose to Display on graph"),
                             choices = list("Year" = 1, "Island" = 2, "Ahupua'a (Watershed)" = 3),
                             selected = 1),
                 ), #End of column 1 tab 2
          column(width = 8,
                 h3('Fires map (tmap)'),
          ) #End of column 2 tab 2
        )#End of fluid row tab 2
      ) #End of fluid page tab 2
    ), ####################################################### END OF TAB 2

    tabPanel(
      title = "Seasonallity",
      fluidPage(
        fluidRow(
          column(width = 4,
                 h3('Widget select box to select which year to highlight'),
          ), #End of column 1 tab 3
          column(width = 8,
                 h3('Seasonal plots of past fires'),
          ) #End of column 2 tab 3
        )#End of fluid row tab 3
      ) #End of fluid page tab 3
    ), ################################## END OF TAB 3

    tabPanel(
      title = "Tab 4",
      fluidPage(
        fluidRow(
          column(width = 4,
                 h3('column 1 header'),
          ), #End of column 1 tab 4
          column(width = 8,
                 h3('column 2 header'),
          ) #End of column 2 tab 4
        )#End of fluid row tab 4
      ) #End of fluid page tab 4
    ), ################################### END OF TAB 4

    tabPanel(
      title = "Citations",
      p('Citations text')
    ), ####################################### END OF TAB 5
  ), #end of tabsetPanel
)### End of fluidPage function




############ SERVER FUNCTION #########################################

server <- function(input, output) { #plot 1
}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










