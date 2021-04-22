# 06-navlist.R

# Load packages

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinymanager)
library(DT)
library(data.table)

# This script is to timeout the login page after 2 mins of inactivity so you dont waste resources:

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


## Password stuff

# data.frame with credentials info
credentials <- data.frame(
  user = c("rescape"),
  password = c("vir2all"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

# Read in and manipulate data

df <- read.csv('Data/List_of_studies.csv')

# A list of what people can choose in the drop down menu. Selects input from columns and tidies it up for display. 
key_word_choices <- unique.array(df[c("Key_word1", "Key_word2", "Key_word3", "Key_word4", "Key_word5")])
key_word_choices <- data.frame(key_word_choices = c(t(key_word_choices)), stringsAsFactors=TRUE)
key_word_choices <- na.omit(key_word_choices)
key_word_choices <- matrix(key_word_choices)

# Now convert df to data table 
df <- as.data.table(df)

# Convert links into clickable hyperlinks

#https://doi.org/
df$Link <- paste0("<a href='",df$Link,"' target='_blank'>",df$Link,"</a>")


## APP STARTS HERE

ui <- secure_app(head_auth = tags$script(inactivity),
                 navbarPage("Rescape Research Portal",
    theme = shinytheme("cerulean"), 
    
 
    # About page
    tabPanel('About',
             tags$div(
               includeHTML('./About.html')
               
    )),
    
    
    # Search Tab
    tabPanel('Search',
    sidebarLayout(
      sidebarPanel(width = 2,
        selectizeInput('search','Choose a research topic', choices = key_word_choices[[1]], multiple = FALSE)
      ),
      mainPanel(width = 10, helpText('Select using multiple critera by entering seperate words into the search bar. For example, try typing \'Stress Biofeedback Experiment\'. Download a list of the displayed studies by clicking the \'Download\' button below.'),
        DT::dataTableOutput('search_table'),
        textOutput('selected_keyword')
        #downloadButton('downloadData', 'Download Selection'))
      ))
    )
    
    )
)


server <- function(input, output) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # Search Tab
  
  output$search_table <- DT::renderDataTable({
    if (is.null(input$search) || input$search == 'All'){
      disp_table <- df[,c('Title','Year','Journal','Authors','Study_Type','Link','Tags')]
      DT::datatable(disp_table,
                    escape = FALSE,
                    filter = 'top',
      extensions = 'Buttons',
      
      options = list(
        order = list(list(1, 'desc')),
        paging = TRUE,
        columnDefs = list(list(visible=FALSE, targets=c(6)),list(targets = c(5), searchable = FALSE)), # Hide tags column but still allow search to see the tags. Make links not searchable. 
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bftsp',
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv','excel','pdf'),
          text = 'Download'
      ))
      ),
      rownames = FALSE)
      
  
    }
  
    else {
      disp_table <- df[Key_word1 %in% (input$search) | Key_word2 %in% (input$search) | Key_word3 %in%(input$search) | Key_word4 %in%(input$search) | Key_word5 %in%(input$search) ]
      disp_table <- disp_table[,c('Title','Year','Journal','Authors','Study_Type','Link','Tags')]
      DT::datatable(disp_table,
                    escape = FALSE,
                    filter = 'top',
      extensions = 'Buttons',
                              
      options = list(
        order = list(list(1, 'desc')),
        paging = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(6)),list(targets = c(5), searchable = FALSE)), # Hide tags column but still allow search to see the tags. Make links not searchable.
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bftsp',
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv','excel','pdf'),
          text = 'Download'
        ))
     ),
     rownames = FALSE)
  
    }
    

    
})
  
}


    



shinyApp(server = server, ui = ui)