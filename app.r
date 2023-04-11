##I started using the shiny app instead of the separate ui.R and server.R so that way it was easier to plug the whole thing into Chapt GPT. 


library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(bslib)





# patent codes ------------------------------------------------------------


#use fake patent codes until they're uploaded
# example_data <- data.frame(
#   patent_codes = c("Code1", "Code2", "Code3", "Code3", "Code2", "Code4", "Code5"),
#   some_values = 1:6
# )
# 
# # Extract unique patent codes from the dataset
# unique_patent_codes <- unique(example_data$patent_codes)


# upload the real cpc codes -- probably need to change file path to be transferable
load("~/Desktop/strat/Module3/shiny/unique_cpc_group.Rdata")
unique_patent_codes <- unique(unique_cpc_group)

  # Assemble the dataframe 
  # Tip: Always be aware of what a "row" is when merging, watch the row count of the resulting data frame to make sure it does what you expect
    #cpc$patent_id <- as.character(cpc$patent_id)
    # Filter the cpc codes
    #dt <- cpc %>% filter(grepl(pattern = 'A61G5/',x = cpc$cpc_group,ignore.case = T))
    # merge with patents
    #dt <- merge(dt,patent,by = 'patent_id')
    # merge with assignee
    #dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?



# ui ----------------------------------------------------------------------

# Set up the bootstrap theme
mytheme <- bs_theme(version = 5, bootswatch = 'sandstone')

# Define the UI
ui <- navbarPage(
  title = 'Patent Analytics',   # Dashboard title
  theme = mytheme,              # Apply the custom theme
  # Define the landing page
  tabPanel(
    title = 'Home',             # Name the landing page
    fluidRow(
      column(
        width = 12,
        h3('Welcome to the Patent Analytics Dashboard!'),
        p('This dashboard provides an analysis of patent data. Please select one of the analysis pages from the menu above.')
      )
    )
  ),
  # Define the competition analysis page
  tabPanel(
    title = 'Competition Analysis',   # Name the analysis page
    fluidRow(
      column(
        width = 4,
        h4('Enter your search criteria:'),
        selectInput(inputId = 'patent_codes', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),
        #selectInput(inputId = 'patent_codes', label = 'Patent Codes:', value = ''),
        textInput(inputId = 'location', label = 'Location:', value = ''),
        textInput(inputId = 'industry', label = 'Industry:', value = ''),
        actionButton(inputId = 'run_analysis', label = 'Run Analysis', class = 'btn-primary')
      ),
      column(
        width = 8,
        h4('Results:'),
        textOutput(outputId = 'selected_patent_codes'),
        plotOutput(outputId = 'competition_plot', height = '500px')
      )
    )
  ),
  # Define the trends analysis page
  tabPanel(
    title = 'Trends Analysis',    # Name the analysis page
    fluidRow(
      column(
        width = 4,
        h4('Enter your search criteria:'),
        selectInput(inputId = 'patent_codes', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),         textInput(inputId = 'location', label = 'Location:', value = ''),
        textInput(inputId = 'industry', label = 'Industry:', value = ''),
        actionButton(inputId = 'run_analysis', label = 'Run Analysis', class = 'btn-primary')
      ),
      column(
        width = 8,
        h4('Results:'),
        textOutput(outputId = 'selected_trends_patent_codes'),
        plotOutput(outputId = 'trends_plot', height = '500px')
      )
    )
  )
)




# server ------------------------------------------------------------------



server <- function(input,output,session) {
  
  
  selected_codes <- reactiveValues(competition = "", trends = "")
  
  observeEvent(input$run_analysis, {
    selected_codes$competition <- paste("Selected patent codes:", paste(input$patent_codes, collapse = ", "))
    selected_codes$trends <- paste("Selected patent codes:", paste(input$patent_codes, collapse = ", "))
  })
  
  output$selected_patent_codes <- renderText({
    selected_codes$competition
  })
  
  output$selected_trends_patent_codes <- renderText({
    selected_codes$trends
  })
  

}



shinyApp(ui, server)

