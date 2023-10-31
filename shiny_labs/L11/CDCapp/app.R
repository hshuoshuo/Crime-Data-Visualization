# load package(s)
library(shiny)
library(tidyverse)
library(ggthemes)

# load cdc data
cdc_data <- read_delim("data/cdc.txt", delim = "|") %>% 
  mutate(
    exerany = factor(exerany, levels = c(1, 0), labels = c("Yes", "No")),
    hlthplan = factor(hlthplan, levels = c(1, 0), labels = c("Yes", "No")),
    smoke100 = factor(smoke100, levels = c(1, 0), labels = c("Yes", "No")),
    gender = factor(gender, levels = c("f", "m"), labels = c("Female", "Male")),
    genhlth = factor(
      genhlth,
      levels = c("excellent", "very good", "good", "fair", "poor"),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("CDC BRFSS Histograms"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # set position of sidebar panel
    position = "right",
    
    # side panel
    sidebarPanel(
      
      # selector to pick x-axis variable
      selectInput(
        inputId = "x_variable",
        label = "Select Variable:",
        choices = c(
          "Actual Weight" = "weight",
          "Desired Weight" = "wtdesire",
          "Height" = "height"
        )
      ), 
      
      # slider widget for picking number of bins
      # provides the server with a number
      sliderInput(
        inputId = "num_bins",
        label = "Number of bins:",
        min = 5,
        max = 50,
        value = 30,
        animate = TRUE
      ),
      
      # radio buttons for choosing fill variable
      radioButtons(
        inputId = "fill_title",
        label = "Select Fill/Legend Variable",
        choices = c(
          "General Health",
          "Health Coverage",
          "Exercised in Past Month",
          "Smoked 100 Cigarettes",
          "Gender"
        ),
        selected = "Gender"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("cdc_histogram")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$cdc_histogram <- renderPlot({
    # generate bins breaks based on input$bins
    x_variable <- cdc_data %>% pull(input$x_variable)
    bin_breaks <- seq(
      min(x_variable),
      max(x_variable), 
      length.out = input$num_bins + 1
    )
    
    # build x_axis title
    x_title <- case_when(
      input$x_variable == "weight" ~ "Actual Weight in Pounds",
      input$x_variable == "height" ~ "Height in Inches",
      input$x_variable == "wtdesire" ~ "Desired Weight in Pounds"
    )
    
    # identify fill variable
    fill_variable <- switch(
      input$fill_title,
      "General Health" = "genhlth",
      "Health Coverage" = "hlthplan",
      "Exercised in Past Month" = "exerany",
      "Smoked 100 Cigarettes" = "smoke100",
      "Gender" = "gender"
    )
    
    # build the plot
    ggplot(
      data = cdc_data,
      aes_string(x = input$x_variable, fill = fill_variable)
    ) +
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
        x = x_title,
        y = "Count",
        fill = fill_variable
      ) +
      theme_fivethirtyeight() +
      theme(
        axis.title = element_text(),
        legend.position = "top"
      ) + 
      guides(fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        title = input$fill_title
      )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)