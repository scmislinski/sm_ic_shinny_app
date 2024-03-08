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
library(janitor)

################ DATA ###############################################
#### for the tmap
fires_all_sf <- read_sf(here("data","2022_1999_Hawaii_large_Fire_Perimeters_UH_NREM", "fires_1999_2022.shp")) %>%
  clean_names() %>%
 mutate(year = as.integer(year))


fires_tmap_sf <- fires_all_sf %>%
  select(year, fire_month, day, area_ha, county, island, geometry)


##### for the seasons plot

fires_df <- st_drop_geometry(fires_all_sf) %>%
  group_by(year, month, island) %>%
  summarize(area_per_month = sum(area_ha)) %>%
  mutate(date = make_date(year, month))

fires_ts <- fires_df %>%
  mutate(date = lubridate :: ymd(date)) %>%
  as_tsibble(key = island,
             index = date) %>%
  group_by(year)

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
                 selectInput("year_season", "Choose Year",
                             choices = fires_ts$year %>% unique() %>% sort(),
                             selected = "1999", multiple = TRUE, selectize = TRUE
                 )
          ), #End of column 1 tab 3
          column(width = 8,
                 h3('Seasonal Plots of Fires From 1999 to 2022'),
                 plotOutput(outputId = 'seasonal_plot')
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

  year_season <- reactive({
    filtered_season <- fires_ts %>%
      filter(year %in% as.integer(input$year_season))
    return(filtered_data)
  }) ####### End of Season plot reactive
  output$seasonal_plot <- renderPlot({
    ggplot(fires_ts, aes(x = month, y = area_per_month, color = year)) +
      geom_line() +
      labs(x = 'Month of the Year',
           y = 'Area Burned (ha)',
           title = 'Seansonality Trends') +
      theme_classic()
  })

}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










