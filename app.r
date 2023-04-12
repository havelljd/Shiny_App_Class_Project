
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

library(DT)



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
unique_patent_codes <- unique(unique_cpc_group)
unique_patent_codes <- unique_patent_codes[grepl("B60L|Y02", unique_patent_codes, ignore.case = TRUE)]
## delete when we want full data, not filtered



#upload smaller data for testing
small_data <- fread("~/Desktop/strat/Module3/Data/small_data.csv")

#load patent data ### set working directory
# patent <- fread("~/Desktop/strat/Module3/Data/g_patent_2012_2021.csv")
# application<- fread("~/Desktop/strat/Module3/Data/g_application_2012_2021.csv")
# assignee <- fread("~/Desktop/strat/Module3/Data/g_assignee_disambiguated_2012_2021.csv")
# term <-  fread("~/Desktop/strat/Module3/Data/g_us_term_of_grant_2012_2021.csv")
# cpc <- fread("~/Desktop/strat/Module3/Data/g_cpc_current_2012_2021.csv")
# location <- fread("~/Desktop/strat/Module3/Data/g_location_disambiguated_2012_2021.csv")



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
  
#i think when we use "patent_codes" it is when david used "market_cpcs_input" -- so i chnaged to his for consistency
    
  # Define the competition analysis page
  tabPanel(
    title = 'Competition Analysis',   # Name the analysis page
    fluidRow(
      column(
        width = 4,
        h4('Enter your search criteria:'),
        selectInput(inputId = 'market_cpcs_input', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),
        textInput(inputId = 'location', label = 'Location:', value = ''),
        textInput(inputId = 'industry', label = 'Industry:', value = ''),
        #textAreaInput(inputId = 'submarket_labels_input', label = "submarket labels", choices = unique_patent_codes, multiple = TRUE), 
        actionButton(inputId = 'generate_competitive_positioning', label = 'Run Analysis', class = 'btn-primary')
      ),
      column(
        width = 8,
        h4('Results:'),
        textOutput(outputId = 'selected_patent_codes'),
        plotOutput(outputId = 'competition_plot', height = '500px'), #for when we add a plot
        h4('Outputs'), 
        textOutput(outputId = "text_labels_output"), ## from david; this isn't anywhere else in code rn
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
        selectInput(inputId = 'market_cpcs_input', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),         textInput(inputId = 'location', label = 'Location:', value = ''),
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
    selected_codes$competition <- paste("Selected patent codes:", paste(input$market_cpcs_input, collapse = ", "))
    selected_codes$trends <- paste("Selected patent codes:", paste(input$market_cpcs_input, collapse = ", "))
    
    #filter the cpc codes
    #dt <- cpc %>%  filter(grepl(pattern = paste(input$market_cpcs_input, sep = "", collapse = "|", x = cpc$cpc_group,ignore.case = T))) ## change market_cpcs_input (david) to patent_codes?
    #bring in smaller merged data 
    dt <- small_data
    ## to use when we have larger data
    # #merge with patents
    # dt <- merge(dt, patent, by = "patent_id")
    # #merge with assignee
    # dt <- merge(dt, assignee, by = "patent_id")
    

    ##paste market cpcs...

    #copy in other competitive positioning code
    

    competition$dt <- head(dt) #this gives an errors saying competition not found when run outside of the app
    #competition$dt <- totals ## for when we put in competition trends
    output$competition_dt <- renderDataTable({competition$dt})
    
  })
  
  
  output$selected_patent_codes <- renderText({
    selected_codes$competition
  })
  
  
  output$selected_trends_patent_codes <- renderText({
    selected_codes$trends
  })

  
  
  
}



shinyApp(ui, server)

