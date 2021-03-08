library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)

if (!require("tidyverse")) install.packages('tidyverse')


longitudinal <- readr::read_csv(here('data','oasis_longitudinal.csv'))
cross_sectional <- readr::read_csv(here('data','oasis_cross-sectional.csv'))


ui <- dashboardPage(skin = "red", 
                    dashboardHeader(title = "Predictors of Alzheimer's Disease", 
                                    titleWidth = 300), 
                    dashboardSidebar(disable=T), 
                    dashboardBody(
  fluidPage(
  selectInput("nWBVMin", "Lower bound of normalized brain volume on first visit", choices=c("0.7","0.72","0.74","0.76","0.78","0.8","0.82")),
  selectInput("nWBVMax", "Upper bound of normalized brain volume on first visit",    choices=c("0.7","0.72","0.74","0.76","0.78","0.8","0.82","0.84")),
  selectInput("AgeMin", "Select Age Min", choices = c("50", "60", "70", "80")),
  selectInput("AgeMax", "Select Age Max", choices = c("50", "60", "70", "80", "90")),
  
  textOutput("result")
  
)
)
)

server <- function(input, output, session) {
  
  
  #This gets the number of patients who fit the criteria
  numpatients <-  reactive({longitudinal %>%
      filter(Visit==1) %>%
      filter(nWBV>=input$nWBVMin & nWBV<= input$nWBVMax) %>%
      filter(Age>=input$AgeMin & Age<= input$AgeMax)%>%
      nrow()
  })
  
  #This gets the number of patients who fit the criteria and are demented or later got dementia (Converted)
  numdemented <-  reactive({longitudinal %>%
      filter(Visit==1) %>%
      filter(nWBV>=input$nWBVMin & nWBV<= input$nWBVMax) %>%
      filter(Age>=input$AgeMin & Age<= input$AgeMax) %>%
      filter(Group=='Demented' | Group == 'Converted') %>%
      nrow()
  })
  
  #Prints out the output
  output$result <- renderText({
    c(numpatients(),"patients meet listed search criteria, out of which", numdemented(), "have dementia or approximately", round((numdemented()/numpatients())*100), "% of the sample")
  
    
    
    })
  
  
  
  # stop the app when closed
  session$onSessionEnded(stopApp)
}


shinyApp(ui, server)