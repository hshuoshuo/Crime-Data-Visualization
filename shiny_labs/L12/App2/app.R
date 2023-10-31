library(shiny)
library(maps)
library(tidyverse)
library(tools) 
library(statebins)
library(sf)
source("statemap.R")
source("countymap.R")
source("binmap.R")


county_data <- read_sf("data/county_data.shp")

# get unique list of state
county_data %>% 
  as_tibble() %>% 
  distinct(state)


counties_map <- map_data("county") %>%
  select(long, lat, group, county = subregion, state = region) %>%
  left_join(., county_data, by = c("county" = "county", "state" = "state"))


bin_data <- county_data %>%
  as_tibble() %>% 
  mutate(
    white_pop = total_pop * white,
    black_pop = total_pop * black,
    hispanic_pop = total_pop * hispanic,
    asian_pop = total_pop * asian
  ) %>%
  select(state, total_pop, white_pop, black_pop, hispanic_pop, asian_pop) %>%
  drop_na() %>%
  group_by(state) %>%
  summarize_all(sum) %>%
  mutate(
    white = white_pop / total_pop,
    black = black_pop / total_pop,
    hispanic = hispanic_pop / total_pop,
    asian = asian_pop / total_pop,
    state = toTitleCase(state)
  ) %>%
  select(state, white, black, hispanic, asian)

choices <- c(state.name, "Contiguous 48 States", "Contiguous 48 States, Counties")

# User interface ----
ui <- fluidPage(
  titlePanel("County Demographic Map by State"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      selectInput("area",
                  label = "Select a state or the contiguous 48 states to display",
                  choices = choices, selected = choices[51]
      ),
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c(
                    "Percent White", "Percent Black",
                    "Percent Hispanic", "Percent Asian"
                  ),
                  selected = "Percent White"
      ),
      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)
      )
    ),
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  dataInput <- reactive({
    if (input$area %in% state.name) {
      counties_map %>%
        filter(state == tolower(input$area))
    } else if (input$area == choices[52]) {
      counties_map
    } else {
      bin_data
    }
  })
  
  genArgs <- reactive({
    args <- switch(input$var,
                   "Percent White" = list(dataInput()$white, "darkgreen", "% White"),
                   "Percent Black" = list(dataInput()$black, "dodgerblue", "% Black"),
                   "Percent Hispanic" = list(dataInput()$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(dataInput()$asian, "darkviolet", "% Asian")
    )
    args$perc_min <- input$range[1]
    args$perc_max <- input$range[2]
    args$area_name <- input$area
    args$map_data <- dataInput()
    args
  })
  
  output$map <- renderPlot({
    if (input$area %in% state.name) {
      do.call(state_map, genArgs())
    } else if (input$area == choices[52]) {
      do.call(county_map, genArgs())
    } else {
      do.call(bin_map, genArgs())
    }
  })
}

# Run app ----
shinyApp(ui, server)
