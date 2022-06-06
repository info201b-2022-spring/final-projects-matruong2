
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
cdc_data <- read.csv("data/suicide_mortality.csv") %>%
  filter(YEAR != 2005) %>%
  select(STATE, YEAR, RATE, DEATHS)
cdc_data$DEATHS <- as.numeric(cdc_data$DEATHS)

WorldSuicide <- read_csv("data/who_suicide_statistics.csv")


intro_page <- 
  tabPanel(
    "Introduction",
    fluidPage(
      h1("This Is the Intro Page"),
      p("This is where we put the intro stuff blahhhhhhhh.")
    )
  )

first_tab <-                  
  tabPanel(                
    "Rate Comparison",              
    fluidPage(                
      h1("Average Rate Comparison by Location or Time"),
      sidebarLayout(
        sidebarPanel(
          h3("Choose to Compare Average Rates by Either State or Year."),
          h5("Select a tab and choose an option."),
          br(),
          h3("Question:"),
          h4("How do rates differ both between states and
          over time?"),
          br(),
          h3("Findings:"),
          h4("The overall average rate for all states from 2014-2020 was ______. The 
          state with the highest over all average rate in this time was ______ with _____,
          while the lowest of _____ belonged to ______. The year with the over all 
          highest average was ____, while the lowest was _____.
             ")
        ),
        mainPanel(
          tabsetPanel(
          tabPanel(
            "Compare States",
            selectInput(inputId = "year",
                      label = "Choose Year:",
                      choices = cdc_data$YEAR,
                      selected = 2020),
            plotlyOutput(outputId = "rate_plot")
          ),
          tabPanel(
            "Compare Over Time",
          selectInput(
            inputId = "state",
            label = "Choose State:",
            choices = cdc_data$STATE,
            selected = "AL"
          ),
            plotlyOutput(outputId = "time")
          ),
          tabPanel(
            "Data Table",
          dataTableOutput(outputId = "rate_table")
          )
        )
      )
    )
  ) 
)

second_tab <- 
  tabPanel( 
    "Suicides per Year by Age Groups", 
    fluidPage( 
      h1("Suicides per Year by Age Groups"),
      p(sidebarLayout(
        sidebarPanel(
          
          # Copy the chunk below to make a group of checkboxes
          radioButtons("checkGroup", label = h2("Age Groups"), 
                             choices = list("5 - 14 Years Old" = "5-14 years", "15 - 24 Years Old" = "15-24 years",
                                            "25 - 34 Years Old" = "25-34 years", "35 - 54 Years Old" = "35-54 years",
                                            "55 - 74 Years Old" = "55-74 years", "75+ Years Old" = "75+ years"),
                             selected = "5-14 years"),
          
        ),
        
        mainPanel("banana"
                  
        )
      )
      ))
    )
   


third_tab <- 
  tabPanel(
    "Tab Three",
    fluidPage(
      h1("Whatever is gonna go here"),
      p("more interactive stuff")
    )
  )


summary_page <-
  tabPanel(
    "Summary",
    fluidPage(
      h1("This is the Summary"),
      p("the friends we made along the way")
    )
  )



# combine all pages, create ui
ui <- (                         
  fluidPage(                    
    navbarPage (                
      
      "WE NEED A TITLE FOR THIS PAGE",
      intro_page,
      first_tab,
      second_tab,
      third_tab,
      summary_page
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

##FIRST TAB STUFF ---------------------------------------------
  
  output$rate_plot <- renderPlotly({
    filtered_cdc_data <- cdc_data %>%
      filter(YEAR == input$year) %>%
      group_by(STATE) %>%
      summarize(RATE = mean(RATE), YEAR, DEATHS)
    fig <- plot_ly(
      data = filtered_cdc_data,
      x = ~STATE,
      y = ~RATE,
      type = 'bar',
      marker = list(color = "rgb(158,202,225)",
                    line = list(color = "rgb(8,48,107)", width = 1.5))
      )
    fig <- fig %>% layout(title = "Rates by State in a Year",
                          xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group'
    )
    
  })
    
  output$time <- renderPlotly({
    filtered2_cdc_data <- cdc_data %>%
      filter(STATE == input$state) %>%
      group_by(YEAR) %>%
      summarize(RATE = mean(RATE), YEAR, DEATHS)
    fig <- plot_ly(
      data = filtered2_cdc_data,
      x = ~YEAR,
      y = ~RATE,
      type = "scatter",
      mode = "lines"
    )
      fig <- fig %>% layout(
        title = "Rates by State 2014 - 2020"
    )
  })
  
  output$rate_table <- renderDataTable({
   the_table <- cdc_data 
    })
 
##END FIRST TAB STUFF --------------------------------
  
}

# Run the application 
shinyApp(ui = ui, server = server)
