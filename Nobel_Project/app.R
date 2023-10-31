library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

# Read the Nobel Prize dataset
nobel_data <- read.csv("nobel_final.csv")

# Calculate the proportion of USA born winners per decade
nobel_data$usa_born_winner <- nobel_data$born_country_code == 'US'
nobel_data$decade <- as.integer(floor(nobel_data$year / 10) * 10)
prop_usa_winners <- nobel_data %>%
  group_by(decade) %>%
  summarize(prop_usa_born_winner = mean(usa_born_winner))

# Calculate the proportion of female laureates per decade and category
nobel_data$female_winner <- nobel_data$gender == 'female'
prop_female_winners <- nobel_data %>%
  group_by(decade, category) %>%
  summarize(prop_female_winner = mean(female_winner))

ui <- fluidPage(
  titlePanel("Nobel Prize Analysis from 1901 till 2020"),
  theme = shinytheme("journal"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "plotType",
        label = "Select Plot Type",
        choices = c("Proportion of USA Born Winners", "Proportion of Female Winners", "Age of Nobel Prize Winners"),
        selected = "Proportion of USA Born Winners"
      ),
      img(src = "nobel.png", height = 200, width = 200)
    ),
    mainPanel(
      plotOutput('plot'),
      HTML("<p><b><span style='color:blue'>Core concept of this project:</span></b> This Shiny app analyzes and visualizes the Nobel Prize dataset. It allows users to select from three plot types: the proportion of USA-born winners per decade, the proportion of female winners per decade and category, and the age distribution of Nobel Prize winners over time. The app generates the corresponding plots using ggplot2 and provides customization options for titles, axis labels, and themes. It offers an interactive platform for exploring and understanding different aspects of the Nobel Prize dataset in a visually appealing manner.</p>
<p><b><span style='color:blue'>Data citation:</span></b> Jannesarr, B. (Year). Nobel Prize from 1901 till 2020 [Data set]. Kaggle. Retrieved from <a href='https://www.kaggle.com/datasets/bahramjannesarr/nobel-prize-from-1901-till-2020'>https://www.kaggle.com/datasets/bahramjannesarr/nobel-prize-from-1901-till-2020</a></p>")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if (input$plotType == "Proportion of USA Born Winners") {
      # Plotting the proportion of USA born winners per decade
      ggplot(prop_usa_winners, aes(x = decade, y = prop_usa_born_winner)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(x = "Decade", y = "Proportion of USA Born Winners", title = "Proportion of USA Born Winners per Decade") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")
    } else if (input$plotType == "Proportion of Female Winners") {
      # Plotting the proportion of female laureates per decade and category
      ggplot(prop_female_winners, aes(x = decade, y = prop_female_winner, color = category)) +
        geom_line() +
        labs(x = "Decade", y = "Proportion of Female Winners", title = "Proportion of Female Winners per Decade") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))
    } else if (input$plotType == "Age of Nobel Prize Winners") {
      # Plotting the age of Nobel Prize winners
      ggplot(nobel_data, aes(x = year, y = age_get_prize)) +
        geom_smooth(method = "loess", color = "black") +
        labs(x = "Year", y = "Age of Nobel Prize Winners", title = "Age of Nobel Prize Winners") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  })
}

shinyApp(ui = ui, server = server)
