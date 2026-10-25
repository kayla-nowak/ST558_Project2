library(shiny)
library(bslib)
library(tidyverse)

#-------------------------------------------------------------------------
# Data setup
#-------------------------------------------------------------------------


#-------------------------------------------------------------------------
# Define UI
#-------------------------------------------------------------------------
ui <- fluidPage(
  
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
      
      radioButtons(
        inputId ="pick_work",
        label = "Listens while working/studying?",
        choiceNames=c("All", "Yes", "No"),
        choiceValues=c("all", "yes", "no"),
        selected = "all"
      ),
      
      sliderInput(
        inputId = "pick_age",
        label = "Age:",
        min = 10, max = 90, step=5,
        value = c(20,40)
      ),
      
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
        tabPanel("About",card(tags$br(),"Hello!")),
        
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

