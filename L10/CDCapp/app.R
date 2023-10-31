library(shiny)
library(tidyverse)

# laod cdc data
cdc_data <- read_delim("data/cdc.txt", delim = "|") %>% 
  mutate(
    genhlth = factor(
      genhlth,
      levels = c("excellent", "very good", "good", "fair", "poor")),
    gender = factor(gender, levels = c("f", "m"), labels = c("Female", "Male"))
  )

# Define UI for application that draws a histogram ----
ui <- fluidPage(
  
  # Application title
  titlePanel("CDC BRFSS: Histogram of Weight Grouped by Gender"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    # set position of sidebar panel
    position = "right",
    
    # slider widget for picking number of bins
    # provides the server with a number
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("cdc_histogram")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$cdc_histogram <- renderPlot({
    # generate bins based on input$bins from ui.R
    x_variable <- cdc_data %>% pull(weight)
    bin_breaks <- seq(
      min(x_variable),
      max(x_variable), 
      length.out = input$bins + 1)
    
    # build the plot
    ggplot(data = cdc_data, aes(x = weight, fill = gender)) +
      geom_histogram(
        color = "black",
        breaks = bin_breaks
      ) +
      # scale_fill_discrete(
      #  name = "Gender",
      #  limits = c("f", "m"),
      # labels = c("Female", "Male")
      # ) +
      labs(
        title = NULL,
        x = "Weight in Pounds",
        y = "Count",
        fill = "Gender"
      ) +
      theme_minimal() +
      theme(legend.position = c(0.48, 0.75))
  })
}


shinyApp(ui = ui, server = server)