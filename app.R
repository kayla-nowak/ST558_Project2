library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)

source("helpers.R")
vars_all <- c(vars_lfbg, vars_mh, vars_oth)

#-------------------------------------------------------------------------
# Data setup
#-------------------------------------------------------------------------
load("data/mxmh.RData")

#Reorder columns for cleaner prints
mxmh <- mxmh |>
  select(musician, while_working, hours_per_day, age, 
         fav_genre, exploratory, foreign_languages, bpm, primary_streaming_service,
         starts_with("frequency"), everything() ) 


#-------------------------------------------------------------------------
# Define UI
#-------------------------------------------------------------------------
ui <- page_fluid(
  
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
                    "Other/Non-Musician"),
        selected = "Any"
      ),
      br(),
      radioButtons(
        inputId ="pick_work",
        label = "Listens while working/studying?",
        choices=c("Any", "Yes", "No"),
        selected = "Any"
      ),
      br(),
      sliderInput(
        inputId = "pick_hrs",
        label = "Listing Hours per Day:",
        min = 0, max = 24, step=2,
        value = c(0,24)
      ),
      br(),
      sliderInput(
        inputId = "pick_age",
        label = "Age:",
        min = 10, max = 90, step=5,
        value = c(20,60)
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
        tabPanel(
          "Data Exploration",
          navset_bar(
            nav_panel("Numeric Data"),
            nav_panel(
              "Categorical Data",
              fluidRow(column(4, selectizeInput("cat1", 
                                                label="Primary Variable:", 
                                                choices=vars_mh)),
                       column(4, selectizeInput("cat2", 
                                                label="Secondary Variable:", 
                                                choices=c("--",vars_lfbg)))),
              plotOutput(outputId = "barchart"),
              DT::dataTableOutput(outputId="table_cat")
            )
          )
          
        ) #close Tab: Data Exploration
      ) #close tabset
    ) #close main
  ) #close sidebar
) #close ui


#-------------------------------------------------------------------------
# Define Server
#-------------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Data Subset ###
  
  mxmh_sub <- eventReactive(input$pick_apply, {
    
    #filter by age and hour/day inputs
    sub <- mxmh |>
      filter(age >= input$pick_age[1] & age <= input$pick_age[2]) |>
      filter(hours_per_day >= input$pick_hrs[1] & hours_per_day <= input$pick_hrs[2]) 
    
    #filter further (if needed)
    if(input$pick_prof != "Any" & input$pick_work !="Any"){
       filter(sub, musician == input$pick_prof & while_working == input$pick_work)
    } else if (input$pick_prof != "Any" ){
       filter(sub, musician == input$pick_prof)
    } else if (input$pick_work !="Any"){
       filter(sub, while_working == input$pick_work)
    } else{ sub }
    
  })
 
  ### Data Download ###
  
  #Table Print
  output$table_download <- DT::renderDataTable({
    mxmh_sub()
  })
  
  #Download Action
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('mxmh_data', Sys.Date(),'.csv', sep='')
    },
    content = function(con){
      write.csv(mxmh_sub(), con)
    }
  )

  
  ### EDA: Categorical Tab ###
  
  observe({
    print(input$cat1)
  })
  
  #Contingency Table (1-way):
  output$table_cat <- DT::renderDataTable({
    
    if(input$cat2=="--") {  #One-Way:
      
      tab <- mxmh_sub() |>
        drop_na(.data[[input$cat1]]) |>
        group_by(.data[[input$cat1]]) |>
        summarize(Count=n(), .groups = "drop") |>
        pivot_wider(names_from = .data[[input$cat1]], 
                    values_from=Count,
                    values_fill=0)    
      
    } else {  #Two-Way:
      
      tab <- mxmh_sub() |>
        drop_na(.data[[input$cat1]], .data[[input$cat2]]) |>
        group_by(.data[[input$cat1]], .data[[input$cat2]]) |>
        summarize(Count=n(), .groups = "drop")|>
        pivot_wider(names_from = .data[[input$cat1]], 
                    values_from=Count,
                    values_fill=0) 
    }
    
    tab
      
  })
  
  
  #Bar Chart:
  output$barchart <- renderPlot({
    
    if(input$cat2=="--") {  #One-Way:
      
      ggplot(mxmh_sub(), aes(x=.data[[input$cat1]])) +
        geom_bar() +
        labs(x = names(vars_all[which(vars_all==input$cat1)]))
      
    } else{  #Two-Way:
      
      ggplot(mxmh_sub(), aes(x=.data[[input$cat1]])) +
        geom_bar(aes(fill = .data[[input$cat2]]), position="dodge") +
        labs(x= names(vars_all[which(vars_all==input$cat1)]))+
        scale_fill_discrete(names(vars_all[which(vars_all==input$cat2)]))

    
    }
  })
  
}


#-------------------------------------------------------------------------
# Run app
#-------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

