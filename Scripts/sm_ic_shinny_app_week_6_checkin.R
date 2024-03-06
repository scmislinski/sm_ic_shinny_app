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

################ DATA ###############################################

fires_all_sf <- read_sf(here("data","fires_tmap_sf.shp"))
fires_tmap_sf <- fires_all_sf %>%
  select(year, fire_month, day, area_ha, county, island, geometry) %>%
  mutate(year = as.integer(year))
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
                 selectInput("year_tmap", "Choose Year",
                   choices = fires_tmap_sf$year %>% unique() %>% sort(),
                   selected = "1999", multiple = TRUE, selectize = TRUE
                 )
               ), #End of column 1 tab 2
          column(width = 8,
                 h3('Map'),
                 tmapOutput(outputId = 'fires_plot')
          ) #End of column 2 tab 2
        )#End of fluid row tab 2
      ) #End of fluid page tab 2
    ), ####################################################### END OF TAB 2

    tabPanel(
      title = "Seasonality",
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
    ), ####################################### END OF TAB 3

    tabPanel(
      title = "Political Response: Text Analysis",
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
    ), ############################################# END OF TAB 4

    tabPanel(
      title = "Citations",
      p('Citations text')
    ), ############################################ END OF TAB 5
  ), #end of tabsetPanel
)### End of fluidPage function




############ SERVER FUNCTION #########################################

server <- function(input, output) {
  year_tmap <- reactive({
    message("in_tmap", class(input$year_tmap))

    filtered_data <- fires_tmap_sf %>%
      filter(year %in% as.integer(input$year_tmap))
    return(filtered_data)
  }) ### end of year_tmap reactive
  output$fires_plot <- renderTmap({
    tmap_mode(mode = "view")
    tm_shape(year_tmap()) +
      tm_fill("area_ha", palette = "OrRd") +
      tm_layout(title = "Fires by Area for the Islands of Hawai'i 1999-2022", title.size = 1)
  }) #### end of tmap plot
}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










