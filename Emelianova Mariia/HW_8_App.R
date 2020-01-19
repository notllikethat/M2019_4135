library(shiny)
library(ggplot2)
library(dplyr)

# Data
data <- mtcars

data$cyl <- as.factor(data$cyl)
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$carb <- as.factor(data$carb)


# Define UI for application
ui <- fluidPage(

    # Sidebar Layout:
    sidebarLayout(
      
        # Sidebar: for inputs here
        sidebarPanel(
          
          # Plot title
          textInput(inputId = "title",
                    label = "Plot title:",
                    placeholder = "Enter the title"),
          
          
          # Select data for x-axis
          selectInput(inputId = "x",
                      label = "X-axis",
                      choices = c("Miles / (US) gallon" = "mpg", 
                                  "Displacement (cu.in.)" = "disp", 
                                  "Gross horsepower" = "hp", 
                                  "Rear axle ratio" = "drat", 
                                  "Weight (1000 lbs)" = "wt", 
                                  "1/4 mile time" = "qsec"),
                       selected = "mpg"),
          
          # Select data for y-axis
          selectInput(inputId = "y",
                      label = "Y-axis",
                      choices = c("Miles / (US) gallon" = "mpg", 
                                  "Displacement (cu.in.)" = "disp", 
                                  "Gross horsepower" = "hp", 
                                  "Rear axle ratio" = "drat", 
                                  "Weight (1000 lbs)" = "wt", 
                                  "1/4 mile time" = "qsec"),
                      selected = "disp"),
          
          # Select data for color
          selectInput(inputId = "color",
                      label = "Color by:",
                      choices = c("Number of cylinders" = "cyl", 
                                  "Engine" = "vs", 
                                  "Transmission" = "am", 
                                  "Number of forward gears" = "gear", 
                                  "Number of carburators" = "carb"),
                      selected = "cyl"),
          
          sliderInput(inputId = "alpha",
                      label = "Alpha:",
                      min = 0, max = 1,
                      value = 0.5),
          
          numericInput(inputId = "size",
                       label = "Dot size:",
                       value = 3,
                       min = 1, max = 9),
          
          checkboxInput(inputId = "show_data",
                        label = "Show data?",
                        value = FALSE),
          
          checkboxInput(inputId = "show_summary",
                        label = "Show summary?",
                        value = FALSE),
          
          submitButton("Apply")
        ),

        
        # Main panel: for outputs here
        mainPanel(
          
          # Plot output
          plotOutput(outputId = "scatter"),
          
          fluidRow(
            column(width = 6,
                   tableOutput(outputId = "data_selected")),
            column(width = 6,
                   tableOutput(outputId = "summary"))
          )
        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$scatter <- renderPlot({
      req(input$size)
      
        ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
        geom_point(alpha = input$alpha, size = input$size)
    })
    
    new_data <- reactive ({
      data %>% 
        select_(input$x, input$y, input$color)
    })
    
    output$data_selected <- renderTable({
      if(input$show_data) {
        new_data()
      }
    })
    
    
    output$summary <- renderTable({
      if (input$show_summary) {
        new_data() %>% 
          group_by_(input$color) %>%
          summarise_all(funs(mean, sd))
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
