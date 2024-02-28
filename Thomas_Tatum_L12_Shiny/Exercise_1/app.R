# Load required libraries
library(shiny)
library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
runGitHub("L12_App", "tatumcthomas")


# Read Data 
county_data = read_sf('data/county_data.shp') %>% 
  mutate(state = str_to_title(state))

# Define the user interface for the application
ui <- fluidPage(
  # Use tags for custom styling of the main title
  tags$head(
    tags$style(HTML("
      .main-title {
        font-size: 24px;
        font-weight: normal;
      }
    "))
  ),
  div(class = "main-title", "County Demographic Map By State"),
  
  sidebarLayout(
    sidebarPanel(
      HTML("<p style='color: grey;'>Create demographic maps with information from the 2010 US Census</p>"),
      selectInput("state_select", "Choose a state to display", choices = unique(county_data$state)),
      selectInput("variable_select", "Choose a Variable to display", 
                  choices = c("Percent White" = "white", 
                              "Percent Black" = "black", 
                              "Percent Hispanic" = "hispanic", 
                              "Percent Asian" = "asian")),
      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      
    ),
    mainPanel(
      plotOutput("mapOutput")
    )
  )
)

# Server logic to render the plot based on inputs
server <- function(input, output) {
  output$mapOutput <- renderPlot({
    if (is.null(input$state_select) || is.null(input$variable_select)) return()
    
    filtered_data <- county_data %>% 
      filter(state == input$state_select) 
    
    var_name <- input$variable_select
    color_map <- c("white" = "darkgreen", "black" = "black", 
                   "hispanic" = "darkorange", "asian" = "darkviolet")
    color <- color_map[var_name]
    
    # Use the input$variable_select directly to match the choices in selectInput
    legend_title <- switch(input$variable_select,
                           "white" = "% White",
                           "black" = "% Black",
                           "hispanic" = "% Hispanic",
                           "asian" = "% Asian")
    
    title <- paste0(toupper(substring(input$state_select, 1, 1)), 
                    tolower(substring(input$state_select, 2)))
    
    ggplot(filtered_data) +
      geom_sf(aes(fill = !!sym(var_name)), color = "black") +
      scale_fill_continuous(name = legend_title, low = "white", high = color, limits = c(0, 100), oob = scales::squish) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            legend.position = "right",
            plot.title = element_text(size = 24, hjust = 0.5, face = "bold"), # Ensure the state map title is bold
            axis.title.x = element_blank(),  
            axis.title.y = element_blank(),  
            axis.text.x = element_blank(),  
            axis.text.y = element_blank(),  
            axis.ticks = element_blank()) +  
      labs(title = title)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


  

