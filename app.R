library(shiny)
library(bslib)
library(shinyalert)
library(tidyverse)
library(ggplot2)

source("helpers.R")

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
    
    ##---- Sidebar + Widgets ----
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
    
    ##---- Main Panel ----
    mainPanel(
      
      ## Tab Structure
      tabsetPanel(
        type="tabs",
        
        # ++ About Tab ++
        tabPanel(
          "About", br(),
          layout_columns(
            
            card("This app supports exploratory data analysis of the “Music and Mental Health” (MxMH) survey, collected by Catherine Rasgaitis in 2022.",
                br(), br(), "The survey was designed to explore potential relations between an person’s mental health and their music tastes/habits. The survey results aimed to inform more rigorous research in the field of music therapy.",
                 tags$p("The original data set is available on ", tags$a("Kaggle.com", href="https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results")),
                 "Source data was downloaded on 2026-10-20 by Kayla Nowak."),
          
            card(tags$img(src="mxmh_logo.png")),
            
            card(h4("Using this App"), 
                 "The sidebar (on the left) subsets the MxMH dataset based on the provided inputs. This subset is then used for the Data Download and Data Exploration tabs. 
Please click “Apply Subset” to generate the desired data subset. To change the subset, please make the desired adjustments and then re-click the “Apply Subset” button to apply the updates.", 
                 
                 tags$p(tags$b("Data Download Tab"), " includes an interactive data table and the option to download the data as a CSV file. All available MxMH variables are included. Observations reflect the data subset criteria (from the sidebar)."),
                 
                 tags$p(tags$b("Data Exploration Tab"), " includes sub-tabs for Numerical or Categorical data exploration. Both tabs provide graphical and numerical summaries for the variable(s) selected at the top of the page. Note that the data subset (from the sidebar) remains in effect. "),
                 
                 tags$p(tags$em("Numeric Data"), " offers 1 numeric variable and 1 categorical (grouping) variable for exploration. The user may choose either a density plot or boxplot graphical summary. The numeric summary is a table of mean, standard deviation, minimum, median, and maximum for the numeric variable at each level of the categorical variable."),
                 
                 tags$p(tags$em("Categorical Data"), ' accepts up to 2 categorical variables for exploration. The user may choose either a bar chart or heatmap graphical summary; the numeric summary is a contingency table. For single-variable exploration, leave the secondary variable input set to "--". ')),
            col_widths = c(8,4,12))
          ),
        
        # ++ Data Download Tab ++
        tabPanel(
          "Data Download",
          card(downloadLink("downloadData", "Download as CSV")),
          DT::dataTableOutput(outputId="table_download")
          ),
        
        # ++ Data Exploration Tab ++
        tabPanel(
          "Data Exploration",
          navset_bar(
            nav_panel(
              "Numeric Data",
              fluidRow(column(5, selectizeInput("num1", 
                                                label="Primary Variable:", 
                                                choices=c(vars_mh,vars_oth_n))),
                       column(5, selectizeInput("cross", 
                                                label="Grouping Variable:", 
                                                choices=c("--",vars_lfbg,vars_oth_c))),
                       column(2, radioButtons("pick_plot_n",
                                            label="Plot Type:",
                                            choices=c("Density", "Boxplot")))),
              plotOutput(outputId = "plot_num"),
              DT::dataTableOutput(outputId="table_num")
            ),
            nav_panel(
              "Categorical Data",
              fluidRow(column(4, selectizeInput("cat1", 
                                                label="Primary Variable:", 
                                                choices=vars_lfbg)),
                       column(4, selectizeInput("cat2", 
                                                label="Secondary Variable:", 
                                                choices=c("--",vars_oth_c))),
                       column(4, radioButtons("pick_plot_c",
                                              label="Plot Type:",
                                              choices=c("Bar Chart", "Heatmap")))),
              plotOutput(outputId = "plot_cat"),
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
    
    # Loading message
    validate( need(input$pick_apply, #boolean?
                   message="Please click 'Apply Subset' on the sidebar to generate data.")),
    
    filename = function(){
      paste0('mxmh_data', Sys.Date(),'.csv')
    },
    content = function(con){
      write.csv(mxmh_sub(), con)
    }
  )

  
  ### EDA: Numeric Tab ###
  
  #Summary Table:
  output$table_num<- DT::renderDataTable({
    
    if(input$cross=="--") {
      
      #Prep data for overall summary:
      for_numtab <- mxmh_sub() |> 
        select(input$num1) |>
        drop_na(input$num1)
      
    } else {
      
      #Prep data for by-category summary:
      for_numtab <- mxmh_sub() |> 
        select(input$num1, input$cross) |>
        drop_na(input$num1, input$cross) |>
        group_by(!!sym(input$cross))
      
    }
    
    for_numtab |> 
      summarize(across(where(is.numeric),
                       list("Mean"   = ~ round(mean(.x, na.rm = TRUE), digits=1),
                            "SD"     = ~ round(sd(.x, na.rm = TRUE), digits=2),
                            "Min"    = ~ min(.x, na.rm = TRUE),
                            "Median" = ~ median(.x, na.rm = TRUE),
                            "Max"    = ~ max(.x, na.rm = TRUE)),
                       .names = "{.fn}")
      )
      
  })
  
  #Numeric Plots:
  output$plot_num <- renderPlot({
    
    # Loading message
    validate( need(input$pick_apply,
                   message="Please click 'Apply Subset' on the sidebar to generate data."))
    
  
    #Prep Local Objects
    if(input$cross=="--") {
      lab_num1   <- names(vars_all[which(vars_all==input$num1)])
      title_prep <- paste0(input$pick_plot_n, " of ", lab_num1, " (Overall)")
      data_prep  <- mxmh_sub() |> select(input$num1) |> drop_na(input$num1)
      
    } else {
      lab_num1  <- names(vars_all[which(vars_all==input$num1)])
      lab_cross <- names(vars_all[which(vars_all==input$cross)])
      title_prep <- paste0(input$pick_plot_n, " of ", lab_num1, " by ", lab_cross)
      data_prep <- mxmh_sub() |> select(input$num1, input$cross) |> drop_na(input$num1, input$cross)
    }
    
    # Density | Boxplot:
    if(input$pick_plot_n=="Density"){ 
      
      if(input$cross=="--"){  #Single Variable
        n_plot <- ggplot(data_prep, aes_string(x=input$num1)) +
          labs(x = lab_num1, y="Density", title = title_prep) +
          geom_density(fill="#00BFC4") +
          theme_minimal(base_size = 16)
        
      } else{   #By Category
        n_plot <- ggplot(data_prep, aes_string(x=input$num1, fill=input$cross)) +
          labs(x = lab_num1, y="Density", title = title_prep) +
          geom_density() +
          facet_wrap(~ get(input$cross)) +
          scale_fill_discrete(lab_cross) +
          theme_minimal(base_size = 16)
      }
      
    } else if (input$pick_plot_n=="Boxplot"){
      
      if(input$cross=="--"){  #Single Variable
        n_plot <- ggplot(data_prep, aes_string(y=input$num1 )) +
          labs(y = lab_num1, title = title_prep) +         
          geom_boxplot(width=0.2, fill="#00BFC4") + 
          scale_x_discrete() +  #for univariate only
          theme_minimal(base_size = 16) 
        
      } else {  #By Category
        n_plot <- ggplot(data_prep, aes_string(x=input$cross, y=input$num1, fill=input$cross)) +
          labs(x = lab_cross, y=lab_num1, title = title_prep) +
          geom_boxplot() +
          scale_fill_discrete(lab_cross) +
          theme_minimal(base_size = 16)
        
      }
    }
    
    #Return for Render:
    n_plot
    
  })

  
  ### EDA: Categorical Tab ###

  #Contingency Table:
  output$table_cat <- DT::renderDataTable({
    
    if(input$cat2=="--") {  #One-Way:
      
      tab <- mxmh_sub() |> 
        drop_na(input$cat1) |>
        select(input$cat1)  |> 
        table() 
    
    } else {  #Two-Way:
      
      tab <- mxmh_sub() |>
        drop_na(input$cat1,input$cat2) |>
        select(input$cat1,input$cat2)  |>
        table()
    }
    
    as.data.frame(tab) #needed for render, but forces to tall...
      
  })
  
  
  #Categorical Plots:
  output$plot_cat <- renderPlot({
    
    # Loading message
    validate( need(input$pick_apply,
                   message="Please click 'Apply Subset' on the sidebar to generate data."))
    
    #Prep Local Objects
    if(input$cat2=="--") {
      lab_cat1   <- names(vars_all[which(vars_all==input$cat1)])
      title_prep <- paste0(input$pick_plot_c, " of ", lab_cat1, " (Overall)")
      data_prep  <- mxmh_sub() |> select(input$cat1) |> drop_na(input$cat1)
      
    } else {
      lab_cat1  <- names(vars_all[which(vars_all==input$cat1)])
      lab_cat2 <- names(vars_all[which(vars_all==input$cat2)])
      title_prep <- paste0(input$pick_plot_c, " of ", lab_cat1, " by ", lab_cat2)
      data_prep <- mxmh_sub() |> select(input$cat1, input$cat2) |> drop_na(input$cat1, input$cat2)
    }
    
    # Bar Chart | Heatmap:
    if(input$pick_plot_c=="Bar Chart"){
    
      if(input$cat2=="--") {  #One-Way:
        c_plot <- ggplot(data_prep, aes_string(x=input$cat1)) +
          labs(x = lab_cat1, y = "Count", title = title_prep) +
          geom_bar(fill="#00BFC4") +
          theme_minimal(base_size = 16)
        
      } else{  #Two-Way:
        c_plot <- ggplot(data_prep, aes_string(x=input$cat1)) +
          labs(x = lab_cat1, y = "Count", title = title_prep) +
          geom_bar(aes_string(fill = input$cat2), position="dodge") +
          scale_fill_discrete(lab_cat2) +
          theme_minimal(base_size = 16)
        
      }
    } else if (input$pick_plot_c=="Heatmap"){
      
      #Additional prompt within Heatmap option:
      validate( need(input$cat2 != "--",
                message="Please select a second categorical variable for the heatmap display type."))
      
      tab <- table(data_prep[[input$cat1]],data_prep[[input$cat2]])
      
      c_plot <- ggplot(as.data.frame(tab), aes(x=Var1, y=Var2, fill=Freq)) +
        geom_tile(color="black") +
        scale_fill_gradient(low="white", high="#007478") +
        labs(x = lab_cat1, y = lab_cat2, title = title_prep)+
        theme_minimal(base_size = 16)

    }
    
    #Return for Render:
    c_plot
    
  })
  
}


#-------------------------------------------------------------------------
# Run app
#-------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
