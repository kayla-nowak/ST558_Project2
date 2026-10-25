library(shiny)
library(bslib)
library(tidyverse)

#-------------------------------------------------------------------------
# Data setup
#-------------------------------------------------------------------------
load("data/mxmh.RData")

#Derive Musician index
mxmh2 <- mxmh |>
  mutate(musician = case_when(
    instrumentalist == "Yes" & composer== "Yes" ~ "Instrumentalist and Composer",
    instrumentalist =="Yes"  ~ "Instrumentalist Only",
    composer == "Yes"  ~ "Composer Only",
    TRUE ~ "Other/Non-Musician"))

#Check derivation
mxmh2 |> group_by(musician, instrumentalist, composer) |> 
  summarize(n=n()) |> print()


#-------------------------------------------------------------------------
# Define UI
#-------------------------------------------------------------------------
ui <- page_fillable(
  
  ## Title
  titlePanel("Music and Mental Health"),
  
  sidebarLayout(
    
    ## Sidebar + Widgets
    sidebarPanel(
      
      "Select from the following data subsets, and then click 'Apply Subset' to update the data accordingly.",
      br(),
      br(),
     selectizeInput(
        inputId = "pick_prof",
        label = "Respondent Musician Status:",
        choices = c("Any", 
                        "Instrumentalist and Composer", 
                        "Instrumentalist Only", 
                        "Composer Only", 
                        "Other/Non-Musicians"),
        selected = "Any"
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
        value = c(20,60)
      ),
      br(),
      sliderInput(
        inputId = "pick_hrs",
        label = "Listing Hours per Day:",
        min = 0, max = 24, step=2,
        value = c(0,24)
      ),
      br(),
      actionButton("pick_apply","Apply Subset")
   
    ),
    
    ## Main Panel
    mainPanel(
      
      ## Tab Structure
      tabsetPanel(
        type="tabs",
        
        #About Tab
        tabPanel(
          "About", br(),
          layout_columns(
            card("Background info..."),
            card("Citations..."),
            card("image"),
            col_widths = c(12,6,6)
            )
          ),
        
        #Data Download Tab
        tabPanel(
          "Data Download",
          card(downloadLink("downloadData", "Download as CSV")),
          DT::dataTableOutput(outputId="table_download")
          ),
        
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
  
  ### Data Subset ###
  #when pick_apply
 
 
  ### Data Download ###
  
  #Table Print
  output$table_download <- DT::renderDataTable({
    mxmh
  })
  
  #Download Action
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('mxmh_data', Sys.Date(),'.csv', sep='')
    },
    content = function(con){
      write.csv(mxmh, con)
    }
  )

  
}


#-------------------------------------------------------------------------
# Run app
#-------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

