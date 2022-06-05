
library(shiny)
library(ggplot2)
cdc_data <- read.csv("data/suicide_mortality.csv")
cdc_data$DEATHS <- as.numeric(cdc_data$DEATHS)

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
    "Tab One",              
    fluidPage(                
      h1("Whatever is gonna go here"),
      p("more interactive stuff"),
      sidebarLayout(
        sidebarPanel(
          h5("Controls"),
          selectInput(inputId = "year",
                      label = "Choose Year:",
                      choices = cdc_data$YEAR)
        ),
        mainPanel(
          plotlyOutput(outputId = "rate_plot")
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
          checkboxGroupInput("checkGroup", label = h3("Age Groups"), 
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
ui <- (                         ## call this website my website
  fluidPage(                    ## render the ui
    navbarPage (                ## this ui has a navbar
      
      "TITLE",
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
    fig <- fig %>% layout(title = "Sucides Rates by State",
                          xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group'
    )
    
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
