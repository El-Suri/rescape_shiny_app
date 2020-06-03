library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rescape Research Portal"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            radioButtons(inputId = 'firstone', 
                         label = 'Choose an option', 
                         choices = c('First',
                                     'Second',
                                     'Third',
                                     'Fourth')),
            
            radioButtons(inputId = 'secondone', 
                         label = 'Choose an option', 
                         choices = c('First',
                                     'Second',
                                     'Third',
                                     'Fourth'))
        # Show a plot of the generated distribution
       
        ),
        mainPanel(
            textOutput("selected_var"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$selected_var <- renderText({
        "You have selected this"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
