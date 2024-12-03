library(janitor)
library(dplyr)
library(tidyverse)
library(tidyr)
library(leaflet)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(rsconnect)
library(readxl)
library(geojsonio)
library(leaflet.extras)
library(shapefiles)
library(tmap)
library(maps)
library(usmap)
library(sf)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(httr)
library(readr)

ardmappal <- colorFactor(
  palette = c("red", "#FFBF00", "green"),  # Red for low, Amber for mid, Green for high
  levels = c("-2 and lower", "-1, 0, 1", "+2 and above")     # Names for the groups
)

# Categorize the ytdincrease values into the three groups: "Low", "Medium", and "High"
ARD <- ARD %>%
  mutate(ytd_group = case_when(
    ytdincrease <= -2 ~ "-2 and lower",            # Group 1: -2 and lower
    ytdincrease >= 2  ~ "+2 and above",           # Group 3: +2 and above
    TRUE             ~ "-1, 0, 1"           # Group 2: -1, 0, 1
  ))

aermappal <- colorFactor(
  palette = c("red", "#FFBF00", "green"),  # Red for low, Amber for mid, Green for high
  levels = c("-2 and lower", "-1, 0, 1", "+2 and above")     # Names for the groups
)

pop_up_aer <- AER %>%
  mutate(pop_up_aer = paste("Geomarket: ", geomarket, "<br/>",
                            "YTD Increase: ", ytdincrease, "<br/>",
                            "Round: ", round, "<br/>",
                            sep = "")) %>%
  pull(pop_up_aer) %>%
  lapply(htmltools::HTML)

pop_up_ard <- ARD %>%
  mutate(pop_up_ard = paste("Geomarket: ", geomarket, "<br/>",
                            "YTD Increase: ", ytdincrease, "<br/>",
                            "Round: ", round, "<br/>",
                            sep = "")) %>%
  pull(pop_up_ard) %>%
  lapply(htmltools::HTML)

# Categorize the ytdincrease values into the three groups: "Low", "Medium", and "High"
AER <- AER %>%
  mutate(ytd_group = case_when(
    ytdincrease <= -2 ~ "-2 and lower",            # Group 1: -2 and lower
    ytdincrease >= 2  ~ "+2 and above",           # Group 3: +2 and above
    TRUE             ~ "-1, 0, 1"           # Group 2: -1, 0, 1
  ))

ShinyYTDMAP <-
  leaflet() %>%
  addTiles() %>%
  setView(lng = -97, lat = 37, zoom = 4.4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # light version CartoDB.Positron, dark version CartoDB.DarkMatter
  addCircleMarkers(data = ARD, 
                   lat = ~lat, 
                   lng = ~long,
                   stroke = TRUE,
                   fill = TRUE,
                   opacity = 10,
                   color = ~ardmappal(ytd_group),  # Use the ytd_group for colors
                   popup = ~pop_up_ard,
                   group = "Regular Decision Applicants") %>%
  addCircleMarkers(data = AER, 
                   lat = ~lat, 
                   lng = ~long,
                   stroke = TRUE,
                   fill = TRUE,
                   opacity = 10,
                   color = ~aermappal(ytd_group),  # Use the ytd_group for colors
                   popup = ~pop_up_aer,
                   group = "Early Round Applicants") %>%
  addLegend(data = ARD, 
            position = "topright", 
            pal = ardmappal,
            values = ~ytd_group,
            title = htmltools::HTML("<strong>Year-to-Date Increase by Geomarket</strong><br/><small>Regular Decision Applicants</small>"),
            opacity = 1,
            group = "Regular Decision Applicants") %>%
  addLegend(data = AER, 
            position = "topright", 
            pal = aermappal,
            values = ~ytd_group,
            title = htmltools::HTML("<strong>Year-to-Date Increase by Geomarket</strong><br/><small>Early Round Applicants</small>"),
            opacity = 1,
            group = "Early Round Applicants") %>%
  addLayersControl(# adds the box to select options
    overlayGroups = c("Regular Decision Applicants", "Early Round Applicants"),
    options = layersControlOptions(collapsed = FALSE,
                                   position = "bottomleft")
  ) %>%
  hideGroup("Regular Decision Applicants") %>%
  hideGroup("Early Round Applicants") %>% # dont show groups by default 
  page_sidebar(
    title = "Year to Date Applicants App") 

# Define server logic ----
server <- function(input, output) {
}
# Run the app ----
shinyApp(ui = ShinyYTDMAP, server = server)

--------------------------------------------------------------------------------------
#auto update data



#look at shinylive package. entirely web-based

#Try between Quarto and R.Markdown to decide what the best way to go about this is

#shinywebmaps

#move this to quarto. seperate in different code chunks, render it in quarto. put it in github

#learn python web scraping - needs to be used for the 

#might need to ask for API access to the site.


