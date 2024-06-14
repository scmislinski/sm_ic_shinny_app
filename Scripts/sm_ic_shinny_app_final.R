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
library(kableExtra)
library(janitor)
library(terra)
library(tidyterra)

################ DATA ###############################################
#### for the tmap
fires_all_sf <- read_sf(here("data","2022_1999_Hawaii_large_Fire_Perimeters_UH_NREM", "fires_1999_2022.shp")) %>%
  clean_names() %>%
 mutate(year = as.integer(year))


fires_tmap_sf <- fires_all_sf %>%
  select(year, fire_month, day, area_ha, county, island, geometry)


##### for the seasons plot ###########################

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


#########################Ignitions Data###################
#load data
st_layers(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"))

big_island <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "bigisland")

fires <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "firesbigisland")

layer3 <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "ignitionpoints_bigisland")

st_layers(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"))

big_island <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "bigisland")


fires <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "firesbigisland")

layer3 <- st_read(here::here("~/Desktop/ESM 244 Advanced Data Science/Shinny App/sm_ic_shinny_app/Data/GIS final project_Ivette_Trace/Final Project ArcPro/workingfinal1", "workingfinal1.gdb"), layer = "ignitionpoints_bigisland")


############# USER INTERFACE ########################################

ui <- fluidPage(
  theme = bs_theme(bootswatch = 'journal'),
  titlePanel("Land Use Changes and Fires for the Islands of Hawai‘i"),
  tabsetPanel(
    tabPanel(
      title = 'Introduction',
      p(style = "font-size = 12px",
        "In Hawai'i, wildland fire occurs frequently and extensively across the state. In Hawaiʻi, approximately 90% of wildfires are due to anthropogenic factors (Hawai'i Department of Natural Resources). The natural conditions in which the majority of the largest fires are produced are dry rural areas with low precipitation (Pacific Fire Exchange, 2023).  Fires in Hawai‘i are different from the rest of the country because they occur year around (Trauernicht et al. 2015). Hawai‘i also has a high burn area in relation to total land mass, and for some years this percentage is higher than the US mainland or the western states (Trauernicht et al. 2015)."),
      p(style = "font-size = 12px",
        "Figure 2 shows the percent of land area that burned for the years 2005 to 2011 for  Hawai‘i, the US mainland and the Western States. For 2005, 2007, and 2010,  Hawai‘i had a higher percent of land area burned than the rest of the US. The total percentage of land area burned is also increasing for Hawai‘i (figure 1). Fire is an ecological process and non managed fires can impact significantly in human lives, economic costs, biodiversity and forest loss. So why doesn’t  Hawai‘i get more attention when it comes to fires and why is there not more done to prevent or prepare for fires?"),
      img(src = "Precent_land.jpg",
          width = 750,
          height = 630),
      p(style = "font-size = 12px", "Figure 1:  The percent of area burned for each year from 1904 to 2022."),
      img(src = "IMG_0496.jpeg",
          width = 800,
          height = 630),
      p(style = "font-size = 12px", "Figure 2:  The percent of area burned for Hawai'i, the US mainland, and the Western United states from 2005 to 2011 (Trauernicht et al., 2015))."),
      p(style = "font-size = 12px", "The Lahaina fire that occurred August 2023 was the most deadly fire in the United States in more than a century, killing 100 people and destroying most of the historic town of Lahaina (NPR). Figures 3, 4, 5, and 6 show before and after satellite images of Lahaina. The cause of the fire was a deadly mixture of, climate change fueled drought, strong winds, major changes in land usage from historic conditions to agriculture, failed infrastructure, invasive species, and a lack of fire preparation (CBS). The 80 mph winds knock down live power lines that sparked the initial fire. The electrical company should have shut off the power knowing that there was going to be extremely strong winds. Fire Fighters thought that they had contained the fire. They left to fight other fires that had started on the island. If there had been more infrastructure for fighting fires, they would have been able to stay to make sure the fire in Lahaina was put out all the way. The strong winds were able to reactivate the initial fire and it spread. Fewer lives would have been lost if there were more than one road out of town. The highway got blocked forcing people to try to flee on foot. Many went to the shore and the water to try to escape the flames. Aid also did not get to Lahaina quickly enough after. Part of this is because it’s hard to transport supplies from the mainland or even from the other islands. If there had been more disaster supplies stored on Maui, aid could have gotten to the survivors more quickly."),
      img(src = "IMG_0497.jpeg",
          width = 800,
          height = 630),
      p(style = "font-size = 12px", "Figure 3: An image of Lahaina from before the fire (Apple Maps)."),
      img(src = "IMG_0499.jpeg",
          width = 800,
          height = 630),
      p(style = "font-size = 12px", "Figure 5: A zoomed in image of the harbor area of Lahaina from after the fire (Google Maps)."),
      img(src = "IMG_0500.jpeg",
          width = 800,
          height = 630),
      p(style = "font-size = 12px", "Figure 6: A zoomed image of the harbor area of Lahaina from before the fire (Apple Maps)."),
      img(src = "IMG_0502.jpeg",
          width = 800,
          height = 630),
      p(style = "font-size = 12px", "Figure 4: An image of Lahaina from after the fire (Google Maps)."),
      p(style = "font-size = 12px", "Since last year’s fires on Maui and the loss of Lahaina, the threat of fires in Hawai‘i has gained more attention from the general public. The demand for accountability of the causes and for policy makers to take action has increased. Even after 6 months, the clean up and rebuilding of the town is still underway and officials estimate a full rebuild of the area could take 5 years (NPR). Many businesses and families have had to move to other parts of Maui, other islands and some even out of state. This is heartbreaking because some of the families forced to leave had lived on Maui or in Lahaina for centuries."),
      p(style = "font-size = 12px", "The Lahaina fire also had major environmental justice implications. About 15% of Maui’s homes are rented out as short term vacation rentals, very few of which are legal. In addition, about 52% of the illegal short term vacation rentals are owned by people out of state. This has increased the rent for residents by about 5% (Hawai'i Civic Beat). This ends up forcing many residents and the victims of the Lahaina fire to move away from Maui or away from Hawai‘i. There are several proposed bills in this legislation session that are designed to help fix this issue. Even if they do pass, there has been a history of very little enforcement of illegal vacation rentals in Hawai‘i (Hawaii Civic Beat)."),
      p(style = "font-size = 12px", "A question that many residents had after the Lahaina fire was if legislators would finally take the fire risk in Hawai‘i and climate change seriously. It does appear that the Lahaina fire has had an effect and has begun to be reflected in policy. On Friday, March 8th the U.S. Department of Homeland Security announced the deployment of new technology across Hawai‘i that would detect and alert authorities of fires quicker (StarAdvetiser).  This year, Hawai‘i’s legislators proposed 52 bills that directly related to climate change, fire management and emergency fire preparedness. In comparison, 16 bills were proposed in the 2023 legislation session. Hopefully, many of the bills pass and are effectively implemented to help prevent fires, and when they do occur, to be able to quickly extinguish them."),
      p(style = "font-size = 12px", strong("Fires Map Tab:"), "This tab shows the area burned for each year and it’s location on the map. You can compare the size and location of the fires for the years 1999 to 2022."),
      p(style = "font-size = 12px", strong("Seasonality Tab:"),"This tab shows the time of year the fires took place from 1999 to 2022. You can compare different years."),
      p(style = "font-size = 12px", strong("Political Response:"), "Text Analysis Tab: This tab shows the results of the text analysis of the bill proposed in 2023 and 2024 for the state of Hawai‘i. The bills included in this analysis were about fire prevention, management, and recovery management as well as any bill relating to climate change adaptation in general. The climate change bills were included because in the future extreme weather events such as drought is expected to occur increasing the risk of fire (Otto 2023)."),
      p(style = "font-size = 12px", strong("Ignitions:"),"This section helps users to assess the relationship between ignitions and fires, especially the most significant in terms of extension.  It provides information about two variables: the geographical location of fires (points) from 2005 to 2020 and the fire perimeters that occurred between 1999 and 2022 in The Big Island.")
    ), ################################################ END OF TAB 1

    tabPanel(
      title = "Fires Map",
      fluidPage(
        fluidRow(p(style = "font-size = 10px", "This interactive map shows the Hawaiian Islands and the area of burn for each year from 1999 to 2022. You can select any year or multiple years and it will display every fire of that year, where it occured and the total land area that the fire burned. You can also change the background on the map to see differt landscapes."),
          column(width = 4,
                 selectInput("year_tmap", "Choose Year",
                   choices = fires_tmap_sf$year %>% unique() %>% sort(),
                   selected = "1999", multiple = TRUE, selectize = TRUE
                 ),
                 p(style = "font-size = 12px", strong("Analysis"),"Hawai‘i Island had the largest fires. There were more fires on the west sides of the islands than the east sides. This tracks with precipitation rates, which are heavier on the east sides of all of the islands.")
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
                 ), p(style = "font-size = 12px", "Fires occurred year around in Hawai‘i, however, the months that had the most fires were between June and September. There were four years that had major fires that were much larger in total area burned than the rest of the years. These years were 2005, 2018, 2021, and 2022. The fires occurred between June and September. 2021 had the highest area burned of all four years.")
          ), #End of column 1 tab 3
          column(width = 8,
                 h4('Seasonal Plots of Fires From 1999 to 2022'),
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
                 p(style = "fon-size = 12px", "The sentiment analysis was done using the word-emotion association (NRC) lexicon. This lexicon associates words with eight basic emotions: anger, fear, anticipation, trust, surprise, sadness, joy, and disgust and the two sentiments of negative and positive. The NRC lexicon was used to test if the Lahaina fire impacted the legislators in how they wrote the bills. The emotions that we were most interested in were anger, fear, sadness, and negative. "),
                 p(style = "font-size = 12px", strong("Anger Sentiment:"),"fell slightly from 2023 to 2024. The decrease was about 1%. This change is surprising and was expected to be higher in 2024 than 2023."),
                 p(style = "font-size = 12px", strong("Anticipation Sentiment:"),"fell slightly from 2023 to 2024."),
                 p(style = "font-size = 12px", strong("Disgust Sentiment:"),"rose slightly from 2023 to 2024. The rise was about half a percentage.
Fear Sentiment: rose from 2023 to 2024. The rise was about 1.5 percent. This is not surprising that it rose. It suggests that the legislators do fear another fire and are more fearful of the impacts of climate change."),
                 p(style = "font-size = 12px", strong("Fear Sentiment:"),"fell quite a bit from 2023 to 2024. This fall was about 2 and a half percent. This is not surprising given the impact Lahaina had on residents."),
                 p(style = "font-size = 12px", strong("Joy Sentiment:"),"rose quite a bit from 2023 to 2024. This rise was about 3 percent."),
                 p(style = "font-size = 12px", strong("Negative Sentiment:"),"fell from 2023 to 2024. This fall was about 3 percent."),
                 p(style = "font-size = 12px", strong("Positive Sentiment:"),"fell from 2023 to 2024. This fall was about 3 percent."),
                 p(style = "font-size = 12px", strong("Sadness Sentiment:"),"rose quite a bit from 2023 to 2024. This rise was about 3 percent. It’s not surprising that the legislators were more sad when writing the bills considering the Lahaina fire."),
                 p(style = "font-size = 12px", strong("Surprise Sentiment:"),"almost doubled from 2023 to 2024. The trust sentiment fell slightly from 2023 to 2024. It is not surprising that the legislators, who’s primary knowledge is policy and not science, were surprised that the impacts of climate change and poor infrastructure finally had serious consequences. It’s unfortunate that the legislators did not heed the warnings from scientists about the threat that fires have in Hawai‘i."),
                 p(style = "font-size = 12px", strong("Trust Sentiment:"),"fell a bit. The fall was about a percent. This sentiment is harder to figure out a potential cause. It could be that the legislators have less trust in the science, that their own bills will be strong enough to prevent another Lahaina, or even trust in themselves. This could show the self doubt of the legislators for not doing more to prevent the Lahaina fire."),
        fluidRow(
          column(width = 4,
                 h5('Choose any word or phrase and it will tell you how many times it occurs in the pollicy for 2023 or 2024. Make sure to put any words in undercase.'),
                 textInput("text_analysis_inputID", label = h3("Text input"), value = "Enter key word"),

          ), #End of column 1 tab 4
          column(width = 8,
                 h5('The Number of Times a Word was in the 2023 and 2024 Bills'),
                 tableOutput('text_analysis')


          ) #End of column 2 tab 4
        )#End of fluid row tab 4
      ) #End of fluid page tab 4
    ), ############################################# END OF TAB 4 Ignitions
    tabPanel(
      title = "Ignitions",
      fluidPage(
        fluidRow(p(style = "font-size = 12px", "Fires in Hawaii are mostly from human caused ignitions, land use legacies and change, and pronounced dry seasons and strong rain shadow effects However, wildfire management in Hawaii has only become part of the environmental agenda in the last few years, especially after the Lahaina fire occurred only in 2023. Wildfire management is a prominent challenge in the environmental agenda in the U.S. that has received attention in the last decades."),
          column(width = 4,
                 h3('column 1 header'),
                 radioButtons(inputId = "ignitions_inputId",
                              label= "Choose Years to Compare",
                              choices = c("2005", "2006", "2007", "2008", "2009","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                              selected = NULL)
          ), #End of column 1 tab 4
          column(width = 8,
                 h3('column 2 header'),
                 plotOutput(outputId = "ignition_plot")
          ) #End of column 2 tab 4
        )#End of fluid row tab 4
      ) #End of fluid page tab 4
    ), ############################################# End of tab 5
    tabPanel(
      title = "Citations",
      p(style = "font-size = 12px", strong("Data was sourced from the Pacific Fire Exchange. Part of the Univeristy of Hawai'i Mānoa's Hawai'i Wildfire Managment Program. Data was also sourced from Hawai'i GIS Program and from the Hawai'i State Government Legislative Information Page.")),
      p(style = "font_size=5px",
        "Data: https://pacificfireexchange.org/region/hawaii/"),
      p(style = "font_size=5px",
        "Data: https://geoportal.hawaii.gov/search?collection=Dataset"),
      p(style = "font_size=5px",
        "Data: https://capitol.hawaii.gov"),
      fluidRow(p(style = "font-size = 12px", "‘A Statewide Ban on Vacation Rentals? The Idea is Gaining Steam After the Maui Fires.’ Civil Beat, https://www.civilbeat.org/2024/02/a-statewide-ban-on-vacation-rentals-the-idea-is-gaining-steam-after-the-maui-fires/. Accessed 3 Mar. 2024."),
               p(style = "font-size = 12px", "Otto, Friederike EL. ‘Attribution of Extreme Events to Climate Change.’  Annual Review of Environment and Resources 48 (2023): 813-828."),
               p(style = "font-size = 12px", "‘Six Months Since Maui Fires, an Uncertain Economy.’ NPR, https://www.npr.org/2024/02/08/1230237974/six-months-since-maui-fires-an-uncertain-economy. Accessed 3 Mar. 2024."),
               p(style = "font-size = 12px", "‘How did Maui fire start? Cause of Lahaina, Hawaii wildfire.’ CBS News, https://www.cbsnews.com/news/how-did-maui-fire-start-cause-lahaina-hawaii-wildfire/. Accessed 3 Mar. 2024."),
               p(style = "font-size = 12px", "‘Wildfire detection sensors to be deployed on Maui and around state.’ Honolulu Star-Advertiser, https://www.staradvertiser.com/2024/03/09/hawaii-news/wildfire-detection-sensors-to-be-deployed-on-maui-and-around-state/. Accessed 3 Mar. 2024."),
               p(style = "font-size = 12px", " Trauernicht, Clay, et al. ‘The contemporary scale and context of wildfire in Hawai‘i1.’ Pacific Science 69.4 (2015): 427-444."),
               p(style = "font-size = 12px", "Department of Natural Resources Hawaii. Division of Forestry and Wildlife: Forestry Program. Fire management. https://dlnr.hawaii.gov/"),
               p(style = "font-size = 12px", "")

    ), ############################################ END OF TAB 5
  ), #end of tabsetPanel
))### End of fluidPage function




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
      tm_fill("area_ha", palette = c("orange","red","darkred")) +
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
  text_function <- reactive({
    text_filter_2023 <- wordcount_clean_2023 %>%
      filter(word == input$text_analysis_inputID)
    return(text_filter_2023)

  })
  text_function_2 <- reactive({
    text_filter_2024 <- wordcount_clean_2024 %>%
      filter(word == input$text_analysis_inputID)
    return(text_filter_2024)
  })##### End of Text Analysis reactive
  output$text_analysis <- renderTable({
    output_2023 <- nrow(text_function())
    output_2024 <- nrow(text_function_2())

    word_count_df <- data.frame(year_2023 = output_2023, year_2024 = output_2024)
    word_count_df %>%
      kbl(caption = "The Number of Times a Word was in the 2023 and 2024 Bills") %>%
      kable_classic(full_width = F,
                    html_font = "Cabria") %>% print
    word_count_df


    }) ########### End of Text Analysis output

  ingitions_reactive <- reactive({
    layer3 <- year %>%
      filter(year %in% as.integer(input$ignitions_inputId))
    return(layer3)
  }) ####### End of ignitions reactive
  output$ignition_plot <- renderPlot({
    ggplot(data = ingitions_reactive()) +
      geom_sf(data = big_island) +
      geom_sf(data = fires, aes(fill = Year))  +
      geom_sf(data = layer3, color = "white", size = 0.1) + theme_void() +
      labs(title = "Ignitions and fire perimeters in Hawaii - The Big Island (1999-2022)")
  }) ###### end of ignitions plot
}


### The following line of code makes this into a shiny app
shinyApp(ui = ui, server = server)










