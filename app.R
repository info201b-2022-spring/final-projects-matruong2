library(tidyverse)
library(shiny)
library(ggplot2)
library(usmap)

US_data <- read.csv("data/suicide_mortality.csv")
colnames(US_data)[2] <- "state"
US_data <- US_data %>% 
  filter( YEAR != 2005)

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
      p("more interactive stuff")
    )
  ) 

second_tab <- 
  tabPanel( 
    "Tab Two", 
    fluidPage( 
      h1("Whatever is gonna go here"),
      p("more interactive stuff")
    )
  ) 


third_tab <- 
  tabPanel(
    "Map Visualization",
    fluidPage(
      titlePanel("U.S. Map of Suicide Death Rates"),
      p("Maybe some kind of intro describing data shown"),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "mapYear",
            label = "Year",
            min = min(US_data$YEAR),
            max = max(US_data$YEAR),
            value = min(US_data$YEAR), #I might change to double slider
            step = 1
          )
        ),
        mainPanel(
          plotOutput(outputId = "map")
        )
      )
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

  
  # server code for the map visualization
  make_map_df <- function(year) { 
    map_df <- US_data %>% 
      filter(YEAR == year) #%>% 
      #doesn't really do much since each year has only one piece of data
      # could change so that avg_rate can be calculated from different year ranges
      #group_by(state) %>% 
      #summarize(avg_rate = mean(RATE, na.rm = TRUE)) 
    return(map_df)
  }
  
  output$map <- renderPlot({
    plot_title <- str_c("Death Rates by State in ", input$mapYear)
    plot_usmap(data = make_map_df(input$mapYear), values = "RATE") +
      labs(title = plot_title ) +
      theme(plot.title = element_text(size=22)) +
      scale_fill_continuous(low = "white", high = "purple3")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
