# 06-navlist.R

library(shiny)
library(shinythemes)
library(DT)
library(data.table)

df <- as.data.table(read.csv('~/Documents/Rescape/Shiny_app/Data/list_of_studies.csv'))

# A list of what people can choose in the drop down
#menu_choices

ui <- navbarPage("Rescape Research Portal",
    theme = shinytheme("cerulean"), 
    
 
    # Search Tab
    
    tabPanel('Search',
    sidebarLayout(
      sidebarPanel(width = 2,
        selectizeInput('search','Choose a research topic', choices = c('All','Anxiety','Biofeedback','Stress'), multiple = TRUE, )
      ),
      mainPanel(width = 9, helpText('Summaries created by the Rescape research team'),
        DT::dataTableOutput('search_table'),
        textOutput('selected_keyword'))
      )
    ),
    
    # VR & Anxiety Tab
    
    tabPanel('VR & Anxiety',
    sidebarLayout(
      sidebarPanel(
        radioButtons('anxiety_page_choice',"", c('Key Findings', 'Spotlight Research', 'Show All'))
      ),
        mainPanel(
          textOutput("selected_var_anx")) 
      )
    ),
    
    # VR & Sleep Tab
    
    tabPanel('VR & Sleep',
    sidebarLayout(
      sidebarPanel(
        radioButtons('sleep_page_choice','', c('Key Findings', 'Spotlight Research', 'Show All'))
      ),
    mainPanel(
      textOutput("selected_var_sleep"))
    )
    ),
    
    # Hide errros from user
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
    
    )


server <- function(input, output) {
  
  
  # Search Tab
  
  output$search_table <- DT::renderDataTable({
    if (is.null(input$search) || input$search == 'All'){
      disp_table <- df[,c('Title','Summary','Reference','DOI')]
      DT::datatable(disp_table)} 
    else {
  disp_table <- df[Key_word1 %in% (input$search) | Key_word2 %in% (input$search) | Key_word3 %in%(input$search)]
  disp_table <- disp_table[,c('Title','Summary','Reference','DOI')]
  DT::datatable(disp_table) 
    }
  })
  
  
  # VR & Anxiety Tab
  
  output$selected_var_anx <- renderText({
      displayed_txt = paste("You chose", input$anxiety_page_choice)
      print(displayed_txt)  
  })
  
  # VR & Sleep Tab
  
  # The if statement will be useful later for selecting markdown text to display. 
  output$selected_var_sleep <- renderText({
    if (input$sleep_page_choice == ''){
      displayed_txt = paste("You chose", input$sleep_page_choice) 
    } else {
      displayed_txt = paste("You chose", input$sleep_page_choice)
    }
    print(displayed_txt)  
  })
  
  }

shinyApp(server = server, ui = ui)