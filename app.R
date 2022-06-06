library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyverse)
library(usmap)
library(shinythemes)
cdc_data <- read.csv("data/suicide_mortality.csv") %>%
  filter(YEAR != 2005) %>%
  select(STATE, YEAR, RATE, DEATHS)
cdc_data$DEATHS <- as.numeric(cdc_data$DEATHS)


WorldSuicide <- read_csv("data/who_suicide_statistics.csv")
source("scatter and lines for final project2.0.R")

US_data <- read.csv("data/suicide_mortality.csv")
colnames(US_data)[2] <- "state"
US_data <- US_data %>% 
  filter( YEAR != 2005)


intro_page <- 
  tabPanel("Introduction", fluid = TRUE, sidebarLayout(
    sidebarPanel(
      h3("Context: Mental Health In The US"),
      p("Healthcare in the US as a whole has many glaring flaws but mental health has been a
          growing source of concern. With an imbalance of accessibility and a taboo regarding 
          seeking professional help, many Americans have turned to suicide in search of solace.
          In the year 2016 we found that there were (per 100k individuals): 11 psychiatrists, 
          30 psychologists, 60 social workers, and 4 nurses. The disparity of mental health workers
          and citizens is concerningly large and this research demonstrates the necessity for growth
          in this industry."),
      p("Though it may seem that awareness is growing, people are unaware of the challenges associated 
          with finding help, especially in certain parts of the country. Whether the particular state is 
          overpopulated or not populated enough, there are parts of the country that have an increased rate 
          of suicide that may have a correlation with a poorer infrastructure."),
      h3("Research Questions"),
      p("The primary quetions we are seeking to answer are:",
        "How much has suicide changed through the years?"),
      p("- Has suicide seen an increase overtime? This visualization can help understand which demographics
         has seen fluctuations."),
      p("What is the amount of suicide percent to actual deaths?"),
      p("- Recorded suicides may not reflect mortality 1:1, though this data works from reported suicides
            multiple sources may report varying figures."),
      p("What states have the highest rates of suicide?"),
      p("- The US has many different environments which may have varying degrees of suicide prevelance which
            could be attributed to a wide variety of factors that can severely impact one's quality of life."),
      h3("Goals Of This Research"),
      p("This research and visualization may bring awareness to certain parts of our country in need of greater 
          resources and support. The severity of the mental health crisis in the US is not captured by the statistics
          that describe the entirety of the US and seeing the data rather than simply numbers hopefully creates a 
          greater sense of urgency to enact change."),
      p("This is a highly sensitive topic and we hope to approach this sensibly. Outliers and erroneous datapoints 
          do a great injustice to lives lost but unfortunately, agencies responsible for reporting the data do
          not have a standardized form of reporting which causes discrepencies."),
      h3("Our Sources"),
      p(strong("Source 1: "), "https://www.cdc.gov/nchs/pressroom/sosmap/suicide-mortality/suicide.htm"),
      p("The data was collected from 3000+ local jurisdictions and each city, county, and state determines the data to 
          share with the CDC. However, the CDC plays a neutral role as the reporting is conducted by local jurisdictions.
          There are 400 rows within this dataset from the CDC. There are 5 columns, but one is a URL and does not contain 
          specific data about the state/year/suicide/death count This dataset could be merged with economic trends by year 
          to answer how the state of economy influences suicide rates. Other datasets merged with this one could show 
          causal factors for suicide propensity such as technological access, GDP, homelessness, and much more."),
      p(strong("Source 2: "), "https://www.kaggle.com/code/szamil/suicide-in-the-twenty-first-century/data"),
      p("This set of data was collected by WHO, and it talks about suicide rates throughout the world. The data talks about
          world suicide rates. There are 6 features and 43776 observations. We can answer the amount of suicides by year, sex, and age."),
      p(strong("Source 3: "),"https://www.kaggle.com/datasets/twinkle0705/mental-health-and-suicide-rates?select=Human+Resources.csv"),
      p("This dataset was collected by who, but was organized by Twinkle Khanna. For the human resources part, it shows the type of healthcare
          worker and the amount of them working per 100,000 population for each country in the data. There are 6 features and 107 observations. 
          This dataset can be merged with the previous datasets to show factors of how the amount of resources is related to suicide rates."),
      h3("Authors/Creators"),
      p(strong("Authors:"), "Em Tallman, Jirat Rymparsurat, Martin Truong, Tyler Takeuchi"),
      p("The Information School, University of Washington"),
      p("Spring 2022")
    ),
    
    mainPanel(
      img(src = "https://www.hopkinsmedicine.org/sebin/x/u/spring%20suicide.jpg",
          width = "95%", height = "95%"),
      p(""),
      img(src = "https://chicagotherapist.com/wp-content/uploads/2020/09/suicide-prevention-chicagotherapist.jpg",
          width = "95%", height = "95%"),
      p("")
    )
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
          h4("The overall average rate for all states from 2014-2020 was 16.01. The 
          state with the highest overall average rate in this time was Wyoming with 26.53,
          while the lowest of 7.93 belonged to New Jersey. The year with the overall 
          highest average was 2017, while the lowest was 2014.
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
          h3("Select an age group to view the number of suicides"),
          
          
          radioButtons("checkGroup", label = h3("Age Groups"), 
                       choices = list("5 - 14 Years Old" = "5-14 years", "15 - 24 Years Old" = "15-24 years",
                                      "25 - 34 Years Old" = "25-34 years", "35 - 54 Years Old" = "35-54 years",
                                      "55 - 74 Years Old" = "55-74 years", "75+ Years Old" = "75+ years"),
                       selected = "5-14 years"),
          h3("Question:"),
          h4("How does the number of suicides change depending on age group and through the years?"),
          
          h3("Findings:"),
          h4("There seems to be an increase of suicides as people get older, then the amount drops off around 55 years old. Overall though, as the years go by the number of suicides goes up for every age group."),
          
          h3("Considerations:"),
          h4("As the years go by, the capabilities of data collection can change. There is also the amount of information and how it can travel around the United States")
          
        ),
        
        mainPanel(
          plotlyOutput(outputId = "No_Of_Suicides")
          
        )
      )
      ))
  )



third_tab <- 
  tabPanel(
    "Map Visualization",
    fluidPage(
      titlePanel("U.S. Map of Suicide Death Rates"),
      sidebarLayout(
        sidebarPanel(
          p("This page shows a visual of how suicide death rates have been changing 
            over time. The dataset mainly has data from 2014-2020."),
          p("Use the slider to pick a year to display the map showing the death
            rates for each state"),
          sliderInput(
            inputId = "mapYear",
            label = "Year",
            min = min(US_data$YEAR),
            max = max(US_data$YEAR),
            value = min(US_data$YEAR), #I might change to double slider
            step = 1
          ),
          
          h3("Question:"),
          h4("What patterns are shown in how suicide death rates are changing over
             time and/or location in the United States?"),
          
          h3("Findings:"),
          h4("Based on the maximum of the rate bar to the right of the graph,
             we can see that the lowest max death rate is in 2014 since the bar
             doesn't reach 25, and the highest max rate is in 2020 since the bar
             is above 30. This doesn't mean that there's a linear relationship,
             but it does show that there's an overall slightly increasing trend
             of the suicide death rate over the years. Throughout the years, the 
             states on the West side usually have the highest rates, which is 
             surprising because it was the states with the mid to lower
             populations that had higher suicide death rates compared to the
             states with higher populations with lower suicide death rates.")
          
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
      h1("Conclusion/Findings"),
      p("Less densely populated states tend to have a higher suicide rate compared to
        the lesser populated states like Montana or Wyoming. This could be attributed to
        a lack of mental health resources as well as social or work-related scarcity."),
      p("Wyoming having the highest rate of suicide supports the findings of the graphs/map 
        because it is not as densely populated and likely has a poorer infrastructure for 
        mental healthcafre. In towns where the nearest grocery store is many miles away, the 
        nearest mental heathcare facility is likely even less accessible."),
      p("A rather interesting finding was the sharp increase in suicide among the 35-54 year-old
         age group. This may be an underserved or overlooked demographic within mental health and
         suicide prevention."),
      p("Within this data it becomes clear there are localities and age groups that are underserved
         and deserve more deliberate outreach and effort to increase the overall wellness of society.
         Numbers can tell a story but visualizing the numbers reveals a different perspective of 
        how to best serve society."),
      img(src = "https://www.sadag.org/images/2021-images/suicide-week.jpg",
          width = "70%", height = "70%")
      
    )
  )



# combine all pages, create ui
ui <- fluidPage( 
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Nuosu+SIL&display=swap');
      body, h1, h2, h3, h4, h5 {
        font-family: 'Nuosu SIL', serif;
      }
      "))
  ),
  navbarPage(
    "Suicide Mortality In The US: Between State Lines",
    intro_page,
    first_tab,
    second_tab,
    third_tab,
    summary_page
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
      marker = list(color = "rgb(98,121,184)",
                    line = list(color = "rgb(255,93,115)", width = 1.5))
    )
    fig <- fig %>% layout(title = "Rates by State in a Year",
                          plot_bgcolor = "FCEFEF",
                          xaxis = list(title = "State", tickangle = -45),
                          yaxis = list(title = "Average Suicide Rate"),
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
      mode = "lines",
      marker = list(color = "rgb(98,121,184)"),
      line = list(color = "rgb(255,93,115)")
    )
    fig <- fig %>% layout(
      title = "Rates by State 2014 - 2020",
      plot_bgcolor = "FCEFEF",
      xaxis = list(title = "Average Suicide Rate"),
      yaxis = list(title = "Year")
    )
  })
  
  output$rate_table <- renderDataTable({
    the_table <- cdc_data
  })
  
  ##END FIRST TAB STUFF --------------------------------
  
  
  ##SECOND TAB STUFF --------------------------
  output$No_Of_Suicides <- renderPlotly({
    suicidebyyearUSA <- filter(
      WorldSuicide,
      country == "United States of America") %>%
      filter(year >= 2000)%>% 
      group_by(year) %>% 
      na.omit(WorldSuicide)%>%
      filter(age == input$checkGroup)%>%
      summarize(suicides_no = sum(suicides_no))
    
    fig <- plot_ly(
      data = suicidebyyearUSA,
      x = ~year,
      y = ~suicides_no,
      type = "scatter",
      mode = "lines",
      line = list(color = "FF5D73"),
      marker = list(
        color = "6279B8",
        size = 10,
        shape = 18
        
      )
    )
    
    
    fig <- fig %>% layout(
      title = "Number of Suicides By Age Group",
      plot_bgcolor = "FCEFEF",
      xaxis = list(title = 'Years'),
      yaxis = list(title = 'Number of Suicides')
    )
    
  })
  ##END SECOND TAB STUFF -----------------------------   
  
  
  ## THIRD TAB CODE ------------------------------------
  # server code for the map visualization
  make_map_df <- function(year) { 
    map_df <- US_data %>% 
      filter(YEAR == year)
    return(map_df)
  }
  
  output$map <- renderPlot({
    plot_title <- str_c("Death Rates by State in ", input$mapYear)
    plot_usmap(data = make_map_df(input$mapYear), values = "RATE") +
      labs(title = plot_title ) +
      theme_void() +
      theme(plot.title = element_text(size=22),
            panel.background = element_rect(fill = 'gray', colour = '#6279B8')) +
      scale_fill_continuous(low = "white", high = "purple3")
  })
  
  ## END OF THIRD TAB CODE ------------------------------------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
