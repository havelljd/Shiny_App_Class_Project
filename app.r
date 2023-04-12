
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(bslib)

library(httr)
library(tidyverse)
library(data.table)
library(feather)




# patent codes ------------------------------------------------------------

#change to patent codes once uploaded....
# example_data <- data.frame(
#   patent_codes = c("Code1", "Code2", "Code3", "Code3", "Code2", "Code4", "Code5"),
#   some_values = 1:6
# )
# 
# # Extract unique patent codes from the dataset
# unique_patent_codes <- unique(example_data$patent_codes)


# upload the real cpc codes -- probably need to change file path to be transferable
load("~/Desktop/strat/Module3/shiny/unique_cpc_group.Rdata")
# class(unique_cpc_group)
# structure(unique_cpc_group)
# is.list(unique_cpc_group)
unique_patent_codes <- unique(unique_cpc_group)

#load patent data ### set working directory
patent <- fread("~/Desktop/strat/Module3/Data/g_patent_2012_2021.csv")
application<- fread("~/Desktop/strat/Module3/Data/g_application_2012_2021.csv")
assignee <- fread("~/Desktop/strat/Module3/Data/g_assignee_disambiguated_2012_2021.csv")
term <-  fread("~/Desktop/strat/Module3/Data/g_us_term_of_grant_2012_2021.csv")
cpc <- fread("~/Desktop/strat/Module3/Data/g_cpc_current_2012_2021.csv")
location <- fread("~/Desktop/strat/Module3/Data/g_location_disambiguated_2012_2021.csv")


#david's data
# load('data/unique_cpc_group.Rdata')
# assignee <- read_feather('./g_assignee_disambiguated_2012_2021.feather')
# location <- read_feather('./g_location_disambiguated_2012_2021.feather')
# patent <- read_feather('./g_patent_2012_2021.feather')
# cpc <- fread('data/g_cpc_current_2012_2021.csv')
# cpc$patent_id <- as.character(cpc$patent_id)


 


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
        #textAreaInput(inputId = 'submarket_labels_input", label = "submarket labels") ##on David's code
        actionButton(inputId = 'run_analysis', label = 'Run Analysis', class = 'btn-primary')
      ),
      column(
        width = 8,
        h4('Results:'),
        textOutput(outputId = 'selected_patent_codes'),
        plotOutput(outputId = 'competition_plot', height = '500px'),
        h4('Outputs'), 
        textOutput(outputId = "text_labels_output"),
        DTOutput(outputId = "competition_dt")
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
        actionButton(inputId = 'generate_competitive_positioning', label = 'Run Analysis', class = 'btn-primary')
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
  

  
  ##David's notes
  competition <- reactiveValues(dt = data.frame(), plot = plotly_empty())
  
  
  observeEvent(input$generate_competitive_positioning, {
    #select codes
    selected_codes$competition <- paste("Selected patent codes:", paste(input$patent_codes, collapse = ", "))
    selected_codes$trends <- paste("Selected patent codes:", paste(input$patent_codes, collapse = ", "))
    
    #filter the cpc codes
    dt <- cpc %>%  filter(grepel(pattern = paste(input$market_cpcs_input, sep = "", collapse = "|", x = cpc$cpc_group,ignore.case = T)))
    #merge with patents
    dt <- merge(dt, patent, by = "patent_id")
    #merge with assignee
    dt <- merge(dt, assignee, by = "patent_id")
    

    ##paste market cpcs...

    #copy in other competitive positioning code
    

    competition$dt <- head(dt)
    competition$dt <- totals
    ouput$competition_dt <- renderDataTable({competition$dt})
    
  })
  
  
  output$selected_patent_codes <- renderText({
    selected_codes$competition
  })
  
  
  output$selected_trends_patent_codes <- renderText({
    selected_codes$trends
  })

  
  
  
  
  
  
}



shinyApp(ui, server)

