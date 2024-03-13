#### Shiny App Basic set up

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
library(tidytext)
library(pdftools)
library(textdata)


################ DATA ###############################################
#### for the tmap
fires_all_sf <- read_sf(here("data","2022_1999_Hawaii_large_Fire_Perimeters_UH_NREM", "fires_1999_2022.shp")) %>%
  clean_names() %>%
 mutate(year = as.integer(year))


fires_tmap_sf <- fires_all_sf %>%
  select(year, fire_month, day, area_ha, county, island, geometry)


##### for the seasons plot

fires_df <- st_drop_geometry(fires_all_sf) %>%
  group_by(year, month) %>%
  summarize(area_per_month = sum(area_ha)) %>%
  mutate(date = make_date(year, month))

fires_ts <- fires_df %>%
  mutate(date = lubridate :: ymd(date)) %>%
  as_tsibble(key = NULL,
             index = date) %>%
  group_by(year)

########### Text analysis data #######################
#read in the data:
policy_2023_text <- pdftools::pdf_text(here('data', 'bills_2023_all.pdf'))

policy_2024_text <- pdftools::pdf_text(here('data', 'bills_2024_all.pdf'))

#Creating tokens
#######2023
p_2023_lines <- data.frame(policy_2023_text) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(policy_2023_text, pattern = '\\n')) %>%
  unnest(text_full) %>%
  mutate(text_full = str_trim(text_full)) %>%
  unnest_tokens(word, text_full) %>%
  anti_join(stop_words, by = 'word')

head(stop_words)

wordcount_clean_2023 <- p_2023_lines %>%
  anti_join(stop_words, by = 'word')

######2024
p_2024_lines <- data.frame(policy_2024_text) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(policy_2024_text, pattern = '\\n')) %>%
  unnest(text_full) %>%
  mutate(text_full = str_trim(text_full)) %>%
  unnest_tokens(word, text_full) %>%
  anti_join(stop_words, by = 'word')

head(stop_words)

wordcount_clean_2024 <- p_2024_lines %>%
  anti_join(stop_words, by = 'word')

nrc_lex <- get_sentiments(lexicon = "nrc")

#########2023
p_2023_nrc <- wordcount_clean_2023 %>%
  inner_join(nrc_lex, by = 'word')

p_2023_counts <- p_2023_nrc %>%
  group_by(sentiment) %>%
  summarize(n = n()) %>%
  mutate(percent = (n/sum(n))*100)

############2024

p_2024_nrc <- wordcount_clean_2024 %>%
  inner_join(nrc_lex, by = 'word')

p_2024_counts <- p_2024_nrc %>%
  group_by(sentiment) %>%
  summarize(n = n()) %>%
  mutate(percent = (n/sum(n))*100)



############# USER INTERFACE ########################################

ui <- fluidPage(
  theme = bs_theme(bootswatch = 'journal'),
  titlePanel("Land Use Changes and Fires for the Islands of Hawai'i"),
  tabsetPanel(
    tabPanel(
      title = 'Introduction',
      p(style = "font-size = 12px",
        "Fires in Hawai‘i are unique to the rest of the country because they occur year around (Trauernicht et al. 2015). Hawai‘i also has a high burn area in relation to total land mass, and many times higher than the US mainland or the western states (Trauernicht et al. 2015). Figure () shows the percent of land area that burned for the years 2005 to 2011 for  Hawai‘i, the US mainland and the Western States. For 2005, 2007, and 2010,  Hawai‘i had a higher percent of land area burned than the rest of the US. So why doesn’t  Hawai‘i get more attention when it comes to fires and why is there not more done to prevent or prepare for fires?"),
      img(src = "Precent_land.jpg",
          width = 550,
          height = 430),
      p(style = "font-size = 12px", "The August 2023 fire that occured in Lahaina was the most deadly fire in the United States in more than a century, killing 100 people and destroying most of the historic town of Lahaina (NPR). The cause of the fire was a deadly mixture of, climate change fueled drought, strong winds, major changes in land usage from historic conditions to agriculture, failed infrastructure, invasive species, and a lack of fire preparation (CBS). The 80 mph winds knock down live power lines that sparked the initial fire. The electrical company should have shut off the power knowing that there was going to be extremely strong winds. Fire Fighters thought that they had contained the fire. They left to fight other fires on the island. The strong winds reactivated the initial fire and spread."),
      p(style = "font-size = 12px", "Since last year’s fires on Maui and the loss of Lahaina, the threat of fires in Hawai‘i has gained more attention from the general public. The demand for accountability of the causes and for policy makers to take action has increased. Even after 6 months, the clean up and rebuilding of the town is still underway and officials estimate a full rebuild of the area could take 5 years (NPR). Many businesses and families have had to move to other parts of Maui, other islands and some even out of state. This fire was devastating to Hawai‘i and was a wake up call for the state and the country."),
      p(style = "font-size = 12px", "The Lahaina fire was the wake up call for Hawai‘i that it has a fire problem and this has begun to be reflected in policy. On Friday, March 8th the U.S. Department of Homeland security announced the deployment of new technology across Hawai‘i that would detect and alert authorities of fires quicker (StarAdvetiser).  This year, Hawai‘i’s legislators proposed 52 bills that directly related to climate change, fire management and emergency fire preparedness. In comparison, 16 bills were proposed in the 2023 legislation session. The Political Response: Text Analysis tab goes into sentiment analysis of the climate change, fire management and emergency fire preparedness of the 2023 and 2024 Hawai‘i State bills."),
      p(style = "font-size = 12px", "Fires Map Tab:"),
      p(style = "font-size = 12px", "Seasonality Tab:"),
      p(style = "font-size = 12px", "Political Response: Text Analysis Tab: This tab shows the results of the text analysis of the bill proposed in 2023 and 2024 for the state of Hawai‘i. The bills included in this analysis were about fire prevention, management, and recovery management as well as any bill relating to climate change adaptation in general. The climate change bills were included because in the future extreme weather events such as drought is expected to occur increasing the risk of fire (Otto 2023)."),
      p(style = "font-size = 12px", "Land Use Tab:")
    ), ################################################ END OF TAB 1

    tabPanel(
      title = "Fires Map",
      fluidPage(
        fluidRow(p(style = "font-size = 10px", "This interactive map shows the Hawaiian Islands and the area of burn for each year from 1999 to 2022. You can select any year or multiple years and it will display every fire of that year, where it occured and the total land area that the fire burned."),
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
        fluidRow(p(style = "font-size = 12px", "In this tab you can explor the seasonality of when fires accure from 1999 to 2022. You can choose as many years as you want, just one or compare a few. January is month 1, February is 2 and so on."),
          column(width = 4,
                 selectInput("year_season", "Choose Year",
                             choices = fires_ts$year %>% unique() %>% sort(),
                             selected = "1999", multiple = TRUE, selectize = TRUE
                 ), p(style = "font-size = 12px", "On average, fires were more previlent and larger during the summer months.")
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
      fluidPage( img(src = "sentiment_combined.jpg",
                     width = 650,
                     height = 550),
                 p(style = "font-size = 12px", "Talk about the sentiment analysis"),
        fluidRow(
          column(width = 4,
                 h3('Choose any word or phrase and it will tell you how many times it occurs in the pollicy for 2023 or 2024'),
                 textInput("text_analysis_inputID", label = h3("Text input"), value = "Enter key word"),

                 hr(),
                 fluidRow(column(3, verbatimTextOutput("value")))
          ), #End of column 1 tab 4
          column(width = 8,
                 h3('column 2 header'),

          ) #End of column 2 tab 4
        )#End of fluid row tab 4
      ) #End of fluid page tab 4
    ), ############################################# END OF TAB 4
    tabPanel(
      title = "Land Use",
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
    ), ############################################# End of tab 5
    tabPanel(
      title = "Citations",
      p(style = "font_size=5px",
        "text here")
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
    return(filtered_season)

  }) ####### End of Season plot reactive
  output$seasonal_plot <- renderPlot({
    ggplot(year_season(), aes(x = month, y = area_per_month, color = as.factor(year))) +
      geom_line() +
      labs(x = 'Month of the Year',
           y = 'Area Burned (ha)',
           title = 'Seansonality Trends') +
      theme_classic()
  }) ######### End of Season plot output
  output$value <- renderPrint({ text_analysis_inputID$text }) ########### End of Text Analysis output
}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










