# libraries needed for app to run
library(shiny)
library(tidyverse) 
library(janitor)
library(readxl)
library(writexl)
library(maps)
library(ggmap)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(sp)
library(htmltools)
library(htmlwidgets)
library(markdown)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(shinyjs) 
library(patchwork)
library(cowplot)
library(ggthemes)
library(fresh)
library(plotly)
library(gsheet)
library(googlesheets4)
library(MetBrewer)
library(devtools)
library(usethis)

# create color theme for dashboard
mytheme <- create_theme(adminlte_color(light_blue = "#FF1493"),
                        adminlte_sidebar(width = "300px",dark_color = "#444",
                                         dark_bg = "#FF1493",dark_hover_bg = "#444",
                                         dark_submenu_color = "#444"),
                        adminlte_global(content_bg = "#1E90FF",box_bg = "#FFFFFF",
                                        info_box_bg = "#FFFFFF"))

# Load Webkinz Google sheet
Webkinz <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1qV1gZpJbnjeP6cPLxpVRqQSA2jTnBP8aPhSSSwzpEDU/edit?usp=sharing")

SignData <- data.frame(name=c("Gemini","Libra","Aquarius","Scorpio","Cancer","Pisces","Leo","Aries","Sagittarius","Virgo","Taurus","Capricorn"),
                   value=c(8,4,5,4,7,8,5,2,3,7,8,4))
ElementData <- data.frame(name=c("Air","Water","Fire","Earth"),
                          value=c(17,19,10,19))
GenderData <- data.frame(name=c("girl","boy"),
                         value=c(31,34))
# Data Table Columns
# Pet Name
# Pet Type
# Pet Group
# Gender
# Adoption Day
# Birth Month
# Birthday
# Zodiac Sign
# Zodiac Element

# ui
ui <- dashboardPage(
        dashboardHeader(title = "Vienna's Webkinz", titleWidth = 250),
        dashboardSidebar(disable=T),
        dashboardBody(use_theme(mytheme),
                      shinyjs::useShinyjs(),
                      div(id = "my app",
                          fluidRow(valueBoxOutput("n_Pets", width = 3),
                                   infoBoxOutput("n_Types", width = 3),
                                   infoBoxOutput("n_Girls", width = 3),
                                   infoBoxOutput("n_Boys", width = 3)),
                          tabBox(title = "",
                                 width = 12,
                                 height = "100%",
                                 tabPanel(icon=tags$i(class="fa-solid fa-star"), "Zodiac Visualizations",
                                          fluidRow(box(title = "Zodiac Sign Distribution of Vienna's Webkinz",
                                                       solidHeader = T,
                                                       width = 12,
                                                       collapsible = T,
                                                       plotlyOutput("Sign", width="100%", height="400px"))),
                                          fluidRow(box(title = "Zodiac Element Distribution of Vienna's Webkinz",
                                                       solidHeader = T,
                                                       width = 12,
                                                       collapsible = T,
                                                       plotlyOutput("Element", width="100%", height="400px")))
                                          ), # close of first tab panel
                                 tabPanel(icon=tags$i(class="fa-solid fa-paw"), "Webkinz Groups",
                                          fluidRow(infoBoxOutput("n_Dogs", width = 3),
                                                   infoBoxOutput("n_Cats", width = 3),
                                                   infoBoxOutput("n_Dragons", width = 3),
                                                   infoBoxOutput("n_Prehistoric", width = 3)),
                                          fluidRow(infoBoxOutput("n_Frogs", width = 3),
                                                   infoBoxOutput("n_Horses", width = 3),
                                                   infoBoxOutput("n_Birds", width = 3),
                                                   infoBoxOutput("n_Googles", width = 3)),
                                          fluidRow(infoBoxOutput("n_Mammals", width = 3),
                                                   infoBoxOutput("n_Reptiles", width = 3),
                                                   infoBoxOutput("n_Marine", width = 3),
                                                   infoBoxOutput("n_Others", width = 3)),
                                          ) # close of second tab panel
                                 ) # close of tab box
                          ) # close of div
                      ) # close of dashboard body
                    ) #close of dashboard page

# server
server <- function(input, output, session) {

# Output info boxes at top of page    
  output$n_Pets = renderInfoBox({
    n = length(unique(Webkinz$`Pet Name`))
    infoBox(title = "My Pets",
            value = n,
            subtitle = "Webkinz",
            color= "light-blue",
            tags$i(class="fa-solid fa-heart"))})
  
  output$n_Types = renderInfoBox({
    n = length(unique(Webkinz$`Pet Type`))
    infoBox(title = "Webkinz",
            value = n,
            subtitle = "Types",
            color= "light-blue",
            tags$i(class="fa-solid fa-paw"))})
  
  output$n_Girls = renderInfoBox({
    n = 31
    infoBox(title = "Girls",
            value = n,
            subtitle = "Pets",
            color= "light-blue",
            icon = icon("venus"))})
  
  output$n_Boys = renderInfoBox({
    n = 34
    infoBox(title = "Boys",
            value = n,
            subtitle = "Pets",
            color= "light-blue",
            icon = icon("mars"))})
  
# Output Zodiac Sign Chart
  output$Sign = renderPlotly({
    data <- SignData
    
    ZodiacPlot = data %>%
      ggplot(aes(name, value, fill=name))+
      geom_col()+
      theme_minimal_vgrid(font_size = 10)+
      scale_fill_manual(values = c("Gemini" = "#ffaf37",
                                   "Libra" = "#ffaf37",
                                   "Aquarius" = "#ffaf37",
                                   "Scorpio" = "#007ba5",
                                   "Cancer" = "#007ba5",
                                   "Pisces" = "#007ba5",
                                   "Leo" = "#f24000",
                                   "Aries" = "#f24000",
                                   "Sagittarius" = "#f24000",
                                   "Virgo" = "#00b67e",
                                   "Taurus" = "#00b67e",
                                   "Capricorn" = "#00b67e"))+
      labs(x="Zodiac Sign",y="Number of Pets") +
      theme(panel.background = element_rect(color = "gray"),legend.position = "none")
    
    ggplotly(ZodiacPlot) %>%
      layout(yaxis = list(title = list(text = "Number of Pets", standoff = 10)),
             xaxis = list(title = list(text = "Zodiac Sign"))) %>%
      style(hoverinfo = 'none') %>%
      config(p, displaylogo = FALSE, toImageButtonOptions = list(format= 'svg', scale= 1),
             modeBarButtonsToRemove = c('autoScale','lasso2d','select','hoverCompareCartesian',
                                        'hoverClosestCartesian','zoom','pan','zoomIn','zoomOut','resetScale'))
  })

# Output Zodiac Sign Chart
  output$Element = renderPlotly({
    data <- ElementData
    
    ZodiacPlot = data %>%
      ggplot(aes(name, value, fill=name))+
      geom_col()+
      theme_minimal_vgrid(font_size = 10)+
      scale_fill_manual(values = c("Air" = "#ffaf37", "Water" = "#007ba5", "Fire" = "#f24000", "Earth" = "#00b67e"))+
      labs(x="Zodiac Element",y="Number of Pets",fill="Zodiac Element") +
      theme(panel.background = element_rect(color = "gray"),legend.position = "none")
    
    ggplotly(ZodiacPlot) %>%
      layout(yaxis = list(title = list(text = "Number of Pets", standoff = 10)),
             xaxis = list(title = list(text = "Zodiac Element"))) %>%
      style(hoverinfo = 'none') %>%
      config(p, displaylogo = FALSE, toImageButtonOptions = list(format= 'svg', scale= 1),
             modeBarButtonsToRemove = c('autoScale','lasso2d','select','hoverCompareCartesian',
                                        'hoverClosestCartesian','zoom','pan','zoomIn','zoomOut','resetScale'))
  })
  
# Output info boxes on second tab panel    
  output$n_Dogs = renderInfoBox({
    infoBox(title = "Dogs",
            value = 10,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-dog"))})
  output$n_Cats = renderInfoBox({
    infoBox(title = "Cats",
            value = 9,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-cat"))})
  output$n_Dragons = renderInfoBox({
    infoBox(title = "Dragons",
            value = 4,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-dragon"))})
  output$n_Prehistoric = renderInfoBox({
    infoBox(title = "Prehistoric",
            value = 5,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-volcano"))})
  output$n_Frogs = renderInfoBox({
    infoBox(title = "Frogs",
            value = 4,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-frog"))})
  output$n_Horses = renderInfoBox({
    infoBox(title = "Horses",
            value = 2,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-horse"))})
  output$n_Birds = renderInfoBox({
    infoBox(title = "Birds",
            value = 5,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-feather-pointed"))})
  output$n_Googles = renderInfoBox({
    infoBox(title = "Googles",
            value = 2,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-kiwi-bird"))})
  output$n_Mammals = renderInfoBox({
    infoBox(title = "Mammals",
            value = 12,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-cow"))})
  output$n_Reptiles = renderInfoBox({
    infoBox(title = "Reptiles",
            value = 2,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-worm"))})
  output$n_Marine = renderInfoBox({
    infoBox(title = "Marine",
            value = 7,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-fish-fins"))})
  output$n_Others = renderInfoBox({
    infoBox(title = "Other",
            value = 3,
            subtitle = "Pets",
            color= "light-blue",
            tags$i(class="fa-solid fa-snowman"))})
}

shinyApp(ui, server)