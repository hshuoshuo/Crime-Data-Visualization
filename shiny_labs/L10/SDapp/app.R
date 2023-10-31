library(shiny)
library(tidyverse)

# load map data
SD <- map_data("county", "south dakota") %>% 
  dplyr::select(lon = long, lat, group, id = subregion)

# Define UI ----
ui <- fluidPage(
  titlePanel("South Dakota"),
  sidebarLayout(
    sidebarPanel(
      h3("Fun Facts:"),
      p(
        strong("State Nickname:"),
        "The Mount Rushmore State (since 1992) formerly The Sunshine State (stupid Florida)"
      ),
      p(strong("State Motto:"), "\"Under God the People Rule\""),
      p(strong("Population:"), "858,469 (2015 est)"),
      p(
        strong("State Fossil:"),
        "Triceratops"
      ),
      p(strong("State Insect:"), "Honey Bee"),
      p(strong("State Flag:"), ""),
      img(src = "Flag_of_South_Dakota.svg.png", height = 130, width = 200),
      br(),
      br(),
      p(strong("State Seal:"), ""),
      img(src = "State_Seal_of_South_Dakota.svg.png", height = 200, width = 200),
    ),
    mainPanel(
      plotOutput(outputId = "statemap"),
      p("· South Dakota is home to the world's only", span("Corn Palace", style = "color:steelblue")),
      p("· South Dakota is", span("not", style = "color:red"), "the same as North Dakota"),
      p("· For more information visit",
        a("South Dakota's Wikipedia Page",
          href = "https://en.wikipedia.org/wiki/South_Dakota"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$statemap <- renderPlot({
    # make map
    ggplot(SD, aes(lon, lat, group = group)) +
      geom_polygon(fill = "lightblue", colour = "grey50") +
      coord_quickmap() +
      theme_void()
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)