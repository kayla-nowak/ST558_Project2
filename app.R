library(shiny)
library(bslib)
library(tidyverse)

#-------------------------------------------------------------------------
# Data setup
#-------------------------------------------------------------------------


#-------------------------------------------------------------------------
# Define UI
#-------------------------------------------------------------------------
ui <- page_fillable(
  
  ## Title
  titlePanel("Music and Mental Health"),
  
  sidebarLayout(
    
    ## Sidebar + Widgets
    sidebarPanel(
      
      checkboxGroupInput(
        inputId = "pick_prof",
        label = "Respondent Musician Status",
        choiceNames = c("Instrumentalist", "Composer", "Other/Non-Musicians"),
        choiceValues = c("inst", "comp", "other"),
        selected = "other"
      ),
      br(),
      radioButtons(
        inputId ="pick_work",
        label = "Listens while working/studying?",
        choiceNames=c("All", "Yes", "No"),
        choiceValues=c("all", "yes", "no"),
        selected = "all"
      ),
      br(),
      sliderInput(
        inputId = "pick_age",
        label = "Age:",
        min = 10, max = 90, step=5,
        value = c(20,40)
      ),
      br(),
      sliderInput(
        inputId = "pick_hrs",
        label = "Listing Hours per Day:",
        min = 0, max = 24, step=2,
        value = c(0,4)
      ),
   
    ),
    
    ## Main Panel
    mainPanel(
      
      ## Tab Structure
      tabsetPanel(
        type="tabs",
        #About Tab
        tabPanel("About", br(),
                 layout_columns(
                   card("Background info..."),
                   card("Citations..."),
                   card("image"),
                   col_widths = c(12,6,6)
                 )
                 ),
        
        #Data Download Tab
        tabPanel("Data Download"),
        
        #Data Exploration Tab
        tabPanel("Data Exploration")
        )
      
    )
  )
)


#-------------------------------------------------------------------------
# Define Server
#-------------------------------------------------------------------------

server <- function(input, output, session) {
  
  
}


#-------------------------------------------------------------------------
# Run app
#-------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

