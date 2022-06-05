

library(shiny)
library(ggplot2)

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

    
}

# Run the application 
shinyApp(ui = ui, server = server)
