library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)
library(janitor)

if (!require("tidyverse")) install.packages('tidyverse')


longitudinal <- readr::read_csv(here('data','oasis_longitudinal.csv'))


longitudinal <- janitor::clean_names(longitudinal)


ui <- dashboardPage(skin = "red", 
                    dashboardHeader(title = "Predictors of Alzheimer's Disease From First Visit", 
                                    titleWidth = 600), 
                    dashboardSidebar(disable=T), 
                    dashboardBody(
                      fluidPage(
                        box(title = "Plot Options", width = 3,
                            selectInput("nWBVMin", "Lower bound of normalized brain volume on first visit", choices=c("0.6","0.72","0.74","0.76","0.78","0.8","0.82")),
                            selectInput("nWBVMax", "Upper bound of normalized brain volume on first visit",    choices=c("0.7","0.72","0.74","0.76","0.78","0.8","0.82","0.84")),
                            selectInput("AgeMin", "Lower bound of Age", choices = c("60", "70", "80")),
                            selectInput("AgeMax", "Upper bound of Age", choices = c("60", "70", "80", "90")),
                            hr(),
                            helpText("Reference:",
                                     
                                     "1. Marcus DS, Fotenos AF, Csernansky JG, Morris JC, Buckner RL. Open Access Series of Imaging Studies (OASIS): Longitudinal MRI Data in Nondemented and Demented Older Adults. Journal of cognitive neuroscience. 2010; 22(12):2677-2684. doi:10.1162/jocn.2009.21407.",
                                     
                                     "2. Marcus, DS, Wang, TH, Parker, J, Csernansky, JG, Morris, JC, Buckner, RL. Open Access Series of Imaging Studies (OASIS): Cross-Sectional MRI Data in Young, Middle Aged, Nondemented, and Demented Older Adults. Journal of Cognitive Neuroscience, 19, 1498-1507. doi:10.1162/jocn.2007.19.9.1498.")
                        ),
                        box(width = 5, textOutput("result")),
                        
                        box(width = 5, title = "Number of Converted/Demented Patients", plotOutput("plot", width = "450px", height = "400px"))
                      )
                    )
)

server <- function(input, output, session) {
  
  totalDemented <- longitudinal %>%
    filter(group=='Demented') %>%
    nrow()
  
  totalConverted <- longitudinal %>%
    filter(group=='Converted') %>%
    nrow()
  #This gets the number of patients who fit the criteria
  numpatients <-  reactive({longitudinal %>%
      filter(visit==1) %>%
      filter(n_wbv>=input$nWBVMin & n_wbv<= input$nWBVMax) %>%
      filter(age>=input$AgeMin & age<= input$AgeMax)%>%
      nrow()
  })
  
  #This gets the number of patients who fit the criteria and are converted
  numconverted <-  reactive({longitudinal %>%
      filter(visit==1) %>%
      filter(n_wbv>=input$nWBVMin & n_wbv<= input$nWBVMax) %>%
      filter(age>=input$AgeMin & age<= input$AgeMax) %>%
      filter(group == 'Converted') %>%
      nrow()
  })
  
  #This gets the number of patients who fit the criteria and are demented
  numdemented <-  reactive({longitudinal %>%
      filter(visit==1) %>%
      filter(n_wbv>=input$nWBVMin & n_wbv<= input$nWBVMax) %>%
      filter(age>=input$AgeMin & age<= input$AgeMax) %>%
      filter(group == 'Demented') %>%
      nrow()
  })
  
  #Prints out the output
  output$result <- renderText({
    c(numpatients(),"PATIENTS MEET LISTED SEARCH CRITERIA: ", numdemented(), "of the 64 patients with existing dementia had brain volumes and ages within these values. ", numconverted(), "of the 14 who did not have dementia but later converted to dementia had brain volumes and ages within these values.")
    
    
    
  })
  
  output$plot <- renderPlot({
    longitudinal%>%
      filter(visit==1) %>%
      filter(n_wbv>=input$nWBVMin & n_wbv<= input$nWBVMax) %>%
      filter(age>=input$AgeMin & age<= input$AgeMax) %>%
      filter(group != 'Nondemented') %>%
      ggplot(aes(x = group)) + geom_bar(fill = 'red', alpha = 0.65) + ylim(0, 75) + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x = 'Group', y = 'Count') 
    
  })
  
  # stop the app when closed
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)