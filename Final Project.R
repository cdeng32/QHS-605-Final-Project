setwd("C:/Users/chrde/OneDrive/Desktop/Grad/R and Class/QHS 605/Final Project")

#Xport read
library(foreign)

#Data
library(tidyverse)

#shiny
library(shiny)
library(shinyWidgets)
library(shinydashboard)

#Data Table
library(DT)

#2015-2016 Data Set ----
DR1IFF_I = read.xport("DR1IFF_I.XPT")
DR1IFF_I$Year = 2015

#2017-2018 Data Set ----
DR1IFF_J = read.xport("DR1IFF_J.XPT")
DR1IFF_J$Year <- 2017

#Variables ----
allowed_choices_x <- c("Intake_Day", "Eating_Occasion", "Source_of_Food")

allowed_choices_y <- c("Energy (kcal)", "Protein (gm)", "Carbohyrate (gm)", 
                       "Dietary fiber (gm)", "Total Fat (gm)")

#Data Manipulation ----

##English ----
###2015-2016 ----
DR1IFF_I_English = DR1IFF_I |> 
  filter(DR1LANG == "1", DR1_030Z %in% c(1,2,3,6), DR1FS %in% c(1,2,3), Year == 2015) |>
  select(DR1LANG, DR1DAY, DR1_030Z, DR1FS, DR1IKCAL, DR1IPROT, DR1ICARB,
         DR1IFIBE, DR1ITFAT, Year) |> 
  rename(Intake_Day = DR1DAY, Eating_Occasion = DR1_030Z, 
         Source_of_Food = DR1FS, "Energy (kcal)" = DR1IKCAL, 
         "Protein (gm)" = DR1IPROT, "Carbohyrate (gm)" = DR1ICARB, 
         "Dietary fiber (gm)" = DR1IFIBE, "Total Fat (gm)" = DR1ITFAT) |> 
  na.omit()

DR1IFF_I_English_re = DR1IFF_I_English |> 
  mutate(
    Intake_Day = factor(Intake_Day,
                        levels = c(1,2,3,4,5,6,7),
                        labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday"))
  ) |> 
  mutate(
    Eating_Occasion = factor(Eating_Occasion,
                             levels = c(1,2,3,6),
                             labels = c("Breaskfast", "Lunch", "Dinner", "Snack"))
  ) |> 
  mutate(
    Source_of_Food = factor(Source_of_Food,
                            levels = c(1,2,3),
                            labels = c("Store", "Restaurant", "Fast/Food"))
  )

###2017-2018 ----
DR1IFF_J_English = DR1IFF_J |> 
  filter(DR1LANG == "1", DR1_030Z %in% c(1,2,3,6), DR1FS %in% c(1,2,3), Year == 2017) |> 
  select(DR1LANG, DR1DAY, DR1_030Z, DR1FS, DR1IKCAL, DR1IPROT, DR1ICARB,
         DR1IFIBE, DR1ITFAT, Year) |> 
  rename(Intake_Day = DR1DAY, Eating_Occasion = DR1_030Z, 
         Source_of_Food = DR1FS, "Energy (kcal)" = DR1IKCAL, 
         "Protein (gm)" = DR1IPROT, "Carbohyrate (gm)" = DR1ICARB, 
         "Dietary fiber (gm)" = DR1IFIBE, "Total Fat (gm)" = DR1ITFAT) |> 
  na.omit()

DR1IFF_J_English_re = DR1IFF_J_English |> 
  mutate(
    Intake_Day = factor(Intake_Day,
                        levels = c(1,2,3,4,5,6,7),
                        labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday"))
  ) |> 
  mutate(
    Eating_Occasion = factor(Eating_Occasion,
                             levels = c(1,2,3,6),
                             labels = c("Breaskfast", "Lunch", "Dinner", "Snack"))
  ) |> 
  mutate(
    Source_of_Food = factor(Source_of_Food,
                            levels = c(1,2,3),
                            labels = c("Store", "Restaurant", "Fast/Food"))
  )

DR1IFF_IJ_English_Final = bind_rows(DR1IFF_J_English_re, DR1IFF_I_English_re)

#Spanish ----

###2015-2016 ----
DR1IFF_I_Spanish = DR1IFF_I |> 
  filter(DR1LANG == "2", DR1_030Z %in% c(1,2,3,6), DR1FS %in% c(1,2,3), Year == 2015) |>
  select(DR1LANG, DR1DAY, DR1_030Z, DR1FS, DR1IKCAL, DR1IPROT, DR1ICARB,
         DR1IFIBE, DR1ITFAT, Year) |> 
  rename(Intake_Day = DR1DAY, Eating_Occasion = DR1_030Z, 
         Source_of_Food = DR1FS, "Energy (kcal)" = DR1IKCAL, 
         "Protein (gm)" = DR1IPROT, "Carbohyrate (gm)" = DR1ICARB, 
         "Dietary fiber (gm)" = DR1IFIBE, "Total Fat (gm)" = DR1ITFAT) |> 
  na.omit()

DR1IFF_I_Spanish_re = DR1IFF_I_Spanish |> 
  mutate(
    Intake_Day = factor(Intake_Day,
                        levels = c(1,2,3,4,5,6,7),
                        labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday"))
  ) |> 
  mutate(
    Eating_Occasion = factor(Eating_Occasion,
                             levels = c(1,2,3,6),
                             labels = c("Breaskfast", "Lunch", "Dinner", "Snack"))
  ) |> 
  mutate(
    Source_of_Food = factor(Source_of_Food,
                            levels = c(1,2,3),
                            labels = c("Store", "Restaurant", "Fast/Food"))
  )

###2017-2018 ----
DR1IFF_J_Spanish = DR1IFF_J |> 
  filter(DR1LANG == "2", DR1_030Z %in% c(1,2,3,6), DR1FS %in% c(1,2,3), Year == 2017) |> 
  select(DR1LANG, DR1DAY, DR1_030Z, DR1FS, DR1IKCAL, DR1IPROT, DR1ICARB,
         DR1IFIBE, DR1ITFAT, Year) |> 
  rename(Intake_Day = DR1DAY, Eating_Occasion = DR1_030Z, 
         Source_of_Food = DR1FS, "Energy (kcal)" = DR1IKCAL, 
         "Protein (gm)" = DR1IPROT, "Carbohyrate (gm)" = DR1ICARB, 
         "Dietary fiber (gm)" = DR1IFIBE, "Total Fat (gm)" = DR1ITFAT) |> 
  na.omit()

DR1IFF_J_Spanish_re = DR1IFF_J_Spanish |> 
  mutate(
    Intake_Day = factor(Intake_Day,
                        levels = c(1,2,3,4,5,6,7),
                        labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday"))
  ) |> 
  mutate(
    Eating_Occasion = factor(Eating_Occasion,
                             levels = c(1,2,3,6),
                             labels = c("Breaskfast", "Lunch", "Dinner", "Snack"))
  ) |> 
  mutate(
    Source_of_Food = factor(Source_of_Food,
                            levels = c(1,2,3),
                            labels = c("Store", "Restaurant", "Fast/Food"))
  )

DR1IFF_IJ_Spanish_Final = bind_rows(DR1IFF_J_Spanish_re, DR1IFF_I_Spanish_re)



#Body ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Dietary Interview Data"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("English", tabName = "english", icon = icon("chart-simple")),
      menuItem("Spanish", tabName = "spanish", icon = icon("chart-simple"))
    ),
    title = span("Choose Variables", style = "padding-left: 15px;"),
    varSelectInput("variablex", label = "X Variable: ", data = DR1IFF_IJ_English_Final,
                   selected = "Intake_Day"),
    varSelectInput("variabley", label = "Y Variable: ", data = DR1IFF_IJ_English_Final,
                   selected = "Energy (kcal)")
  ),
  
  dashboardBody(
    tabItems(
      ##English ----
      tabItem(tabName = "english",
              fluidRow(
                box(
                  ###Data Table 2015 ----
                  title = "Data Table of 2015",
                  dataTableOutput("Data_table_2015_en"),
                ),
                
                box(
                  ###Data Table 2017 ----
                  title = "Data Table of 2017",
                  dataTableOutput("Data_table_2017_en"),
                ),
             ),
             fluidRow(    
               
               box(
                 ###Histogram Intake ----
                 title = "Histogram of Intake Day between Years",
                 plotOutput("hist_Intake_en")
               ),
               
               box(
                 ###Histogram Occasion ----
                 title = "Histogram of Eating Occasion between Years",
                 plotOutput("hist_Occasion_en")
               ),
               
               box(
                 ###Histogram Source ----
                 title = "Histogram of Food Source between Years",
                 plotOutput("hist_Source_en")
               ),
               
               box(
                 ###Boxplot 2015 ----
                 title = "2015 Data Comparison",
                 plotOutput("boxplot_2015_en")
               ),
               
               box(
                 ###Boxplot 2017 ----
                 title = "2017 Data Comparison",
                 plotOutput("boxplot_2017_en")
               ),
             )
          ),
      
      ##Spanish ----
      tabItem(tabName = "spanish",
              fluidRow(
                box(
                  ###Data Table 2015 ----
                  title = "Data Table of 2015",
                  dataTableOutput("Data_table_2015_sp"),
                ),
                
                box(
                  ###Data Table 2017 ----
                  title = "Data Table of 2017",
                  dataTableOutput("Data_table_2017_sp"),
                ),
              ),
              fluidRow(    
                
                box(
                  ###Histogram Intake ----
                  title = "Histogram of Intake Day between Years",
                  plotOutput("hist_Intake_sp")
                ),
                
                box(
                  ###Histogram Occasion ----
                  title = "Histogram of Eating Occasion between Years",
                  plotOutput("hist_Occasion_sp")
                ),
                
                box(
                  ###Histogram Source ----
                  title = "Histogram of Food Source between Years",
                  plotOutput("hist_Source_sp")
                ),
                
                box(
                  ###Boxplot 2015 ----
                  title = "2015 Data Comparison",
                  plotOutput("boxplot_2015_sp")
                ),
                
                box(
                  ###Boxplot 2017 ----
                  title = "2017 Data Comparison",
                  plotOutput("boxplot_2017_sp")
                ),
              )
          )
      ),
  )
)

server <- function(input, output, session) {
  
  #English ----
  
  ##Data Table 2015 ----
  output$Data_table_2015_en <- renderDataTable({
    datatable(DR1IFF_I_English, options = list(scrollX = TRUE))
  })
  
  ##Data Table 2017 ----
  output$Data_table_2017_en <- renderDataTable({
    datatable(DR1IFF_J_English, options = list(scrollX = TRUE))
  })
  
  ##Histogram Intake ----
  output$hist_Intake_en <- renderPlot({
    ggplot(DR1IFF_IJ_English_Final, aes(x = Intake_Day, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Histogram Occasion ----
  output$hist_Occasion_en <- renderPlot({
    ggplot(DR1IFF_IJ_English_Final, aes(x = Eating_Occasion, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Histogram Source ----
  output$hist_Source_en <- renderPlot({
    ggplot(DR1IFF_IJ_English_Final, aes(x = Source_of_Food, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Boxplot 2015 ----
  output$boxplot_2015_en <- renderPlot({
    ggplot(DR1IFF_I_English_re, aes(.data[[input$variablex]], .data[[input$variabley]],
                                    color = .data[[input$variablex]])) +
      geom_boxplot() 
  })
  
  ##Boxplot 2017 ----
  output$boxplot_2017_en <- renderPlot({
    ggplot(DR1IFF_J_English_re, aes(.data[[input$variablex]], .data[[input$variabley]],
                                    color = .data[[input$variablex]])) +
      geom_boxplot() 
  })
  
  ##Limit the choices for x ----
  observe({
    # Update the variable choices based on the allowed_choices vector
    updateSelectInput(session, "variablex", choices = allowed_choices_x)
  })
  
  ##Limit the choices for y ----
  observe({
    # Update the variable choices based on the allowed_choices vector
    updateSelectInput(session, "variabley", choices = allowed_choices_y)
  })
  
  #Spanish ----
  
  ##Data Table 2015 ----
  output$Data_table_2015_sp <- renderDataTable({
    datatable(DR1IFF_I_Spanish, options = list(scrollX = TRUE))
  })
  
  ##Data Table 2017 ----
  output$Data_table_2017_sp <- renderDataTable({
    datatable(DR1IFF_J_Spanish, options = list(scrollX = TRUE))
  })
  
  ##Histogram Intake ----
  output$hist_Intake_sp <- renderPlot({
    ggplot(DR1IFF_IJ_Spanish_Final, aes(x = Intake_Day, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Histogram Occasion ----
  output$hist_Occasion_sp <- renderPlot({
    ggplot(DR1IFF_IJ_Spanish_Final, aes(x = Eating_Occasion, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Histogram Source ----
  output$hist_Source_sp <- renderPlot({
    ggplot(DR1IFF_IJ_Spanish_Final, aes(x = Source_of_Food, fill = as.factor(Year))) +
      geom_histogram(color = "black", stat = "count", position = position_dodge()) + 
      labs(x = "Day of the Week Eaten",
           y = "Count",
           fill = "Year")
  })
  
  ##Boxplot 2015 ----
  output$boxplot_2015_sp <- renderPlot({
    ggplot(DR1IFF_I_Spanish_re, aes(.data[[input$variablex]], .data[[input$variabley]],
                                    color = .data[[input$variablex]])) +
      geom_boxplot() 
  })
  
  ##Boxplot 2017 ----
  output$boxplot_2017_sp <- renderPlot({
    ggplot(DR1IFF_J_Spanish_re, aes(.data[[input$variablex]], .data[[input$variabley]],
                                    color = .data[[input$variablex]])) +
      geom_boxplot() 
  })
  
  ##Limit the choices for x ----
  observe({
    # Update the variable choices based on the allowed_choices vector
    updateSelectInput(session, "variablex", choices = allowed_choices_x)
  })
  
  ##Limit the choices for y ----
  observe({
    # Update the variable choices based on the allowed_choices vector
    updateSelectInput(session, "variabley", choices = allowed_choices_y)
  })
}

shinyApp(ui, server)
