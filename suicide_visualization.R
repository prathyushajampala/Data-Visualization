library(shiny)
library(plotly)
library(dplyr)

#Loading the Data
data <- read.csv("C://Users//spk26//Downloads//master.csv")

#Defining UI of the Dashboard
ui <- fluidPage(
  titlePanel("Suicides Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = unique(data$country)),
      sliderInput("year", "Year:", min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)), sep = ""),
     actionButton("Reset", "Reset")
     
    ),
    mainPanel(
      plotlyOutput("suicide_trend"),
      plotlyOutput("suicide_age",height = "500px", width = "100%"),
      plotlyOutput("suicide_gender",height = "500px", width = "100%"),
      plotlyOutput("suicide_gdp",height = "500px", width = "100%"),
      plotlyOutput("suicide_generation",height = "500px", width = "100%"),
      plotlyOutput("suicide_country",height = "500px", width = "100%"),
      plotlyOutput("suicide_sex_age",height = "500px", width = "100%")
    )
  )
)


#Defining the Server
server <- function(input, output, session) {
  
  #Filtering the Data on the basis of User Inputs
  filtered_data <- reactive({
    data %>%
      filter(country == input$country,
             year >= input$year[1],
             year <= input$year[2])
  })
  
  #Plot 1:Suicide trend over the years
  output$suicide_trend <- renderPlotly({
    filtered_data() %>%
      group_by(year) %>%
      summarise(suicides = sum(suicides_no),
                population = sum(population)) %>%
      mutate(rate = suicides / population * 100000) %>%
      plot_ly(x = ~year, y = ~rate, type = 'scatter', mode = 'lines', 
              line = list(width = 3, color = '#E69F00')) %>%
      layout(title = "Suicide Rate Trend",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Suicide Rate per 100k"))
  })
  
  #Plot 2:Suicide by age group
  output$suicide_age <- renderPlotly({
    filtered_data() %>%
      group_by(age) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(labels = ~age, values = ~suicides, type = 'pie') %>%
      layout(title = "Suicide by Age Group")
  })
  
  #Plot 3:Suicide by gender
  output$suicide_gender <- renderPlotly({
    filtered_data() %>%
      group_by(sex) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(labels = ~sex, values = ~suicides, type = 'pie') %>%
      layout(title = "Suicide by Gender")
  })
  
  # Plot 4:Suicide by GDP
  output$suicide_gdp <- renderPlotly({
    filtered_data() %>%
      group_by(gdp_per_capita....) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(x = ~gdp_per_capita...., y = ~suicides, type = 'scatter', mode = 'markers',
              marker = list(size = 10, color = '#56B4E9')) %>%
      layout(title = "Suicide by GDP per Capita",
             xaxis = list(title = "GDP per Capita"))
  })
  
  # Plot 5:Suicide by generation
  output$suicide_generation <- renderPlotly({
    filtered_data() %>%
      group_by(generation) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(x = ~generation, y = ~suicides, type = 'bar',
              marker = list(color = '#009E73')) %>%
      layout(title = "Suicide by Generation",
             xaxis = list(title = "Generation"),
             yaxis = list(title = "Suicides"))
  })
  
  # Plot 6:Suicide by country
  output$suicide_country <- renderPlotly({
    data %>%
      group_by(country) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(x = ~country, y = ~suicides, type = 'bar',
              marker = list(color = '#D55E00')) %>%
      layout(title = "Suicide by Country",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Suicides"))
  })
  
  # Plot 7:Suicide by sex and age
  output$suicide_sex_age <- renderPlotly({
    filtered_data() %>%
      group_by(sex, age) %>%
      summarise(suicides = sum(suicides_no)) %>%
      plot_ly(x = ~age, y = ~suicides, color = ~sex, type = 'bar') %>%
      layout(title = "Suicide by Sex and Age",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Suicides"))
  })
  
}

#Running the App
shinyApp(ui, server)

