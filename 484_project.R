# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)

# Load data 
data <- read_csv("data/mvp_cleaned.csv", col_types = cols(...1 = col_skip()))
data$Pos <- as.factor(data$Pos)
data$Year <- as.factor(data$Year)

# Segment datasets
Points <- data %>%
  select(Pos, Points, Year) %>%
  rename(v2 = Points)
Assists <- data %>% 
  select(Pos, Assists, Year) %>%
  rename(v2 = Assists)
Rebounds <- data %>%
  select(Pos, Rebounds, Year) %>%
  rename(v2 = Rebounds)

# UI
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("NBA Stats"),
                sidebarLayout(
                  sidebarPanel(
                    # select stat
                    selectInput(inputId = "dataset", label = strong("Statistic"),
                                choices = c("Points", "Assists", "Rebounds"), 
                                selected = "Points"),
                    # select season
                    sliderInput(inputId = "season", label = strong("Season Ending:"),
                                min = 1991, max = 2021, value = 2007, sep = "")),
                  # Output
                  mainPanel(
                    # display plot
                      plotOutput(outputId = 'plot')
                    )
                  )
                )
# Server
server <- function(input, output) {
  # data select
  datasetInput <- reactive({
    switch(input$dataset,
           "Points" = Points,
           "Assists" = Assists,
           "Rebounds" = Rebounds)
  })
  filterInput <- reactive({
    datasetInput() %>%
      filter(Year == input$season) %>%
      as.data.frame()
  })
  # create plot
  output$plot <- renderPlot({
      ggplot(data = filterInput(), aes(x = v2, 
                                       y = ..count..,
                                       fill = Pos)) + 
      geom_histogram(bins = 15, color = "gray6") +
      facet_wrap(~ Pos, nrow = 1) +
      scale_fill_viridis_d() +
      theme_few() + 
      theme(legend.position = "none", 
            axis.title = element_text(size = 18),
            plot.title = element_text(size = 25),
            strip.text = element_text(size = 15)) + 
      labs(x = "Statistic", y = "Count",
           title = "Comparing Statistics By Position")
  })
}
shinyApp(ui, server)
