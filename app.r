
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
        #plotOutput(outputId = 'competition_plot', height = '500px'), #for when we add a plot
        # h4('Outputs'), 
        #textOutput(outputId = "text_labels_output"), ## from david; this isn't anywhere else in code rn
        DTOutput(outputId = "competition_dt")
      )
    )
  ),
  
  
  # Define the TECH trends analysis page
  tabPanel(
    title = 'Trends Analysis',    # Name the analysis page
    fluidRow(
      column(
        width = 4,
        h4('Enter your search criteria:'),
        # selectInput(inputId = 'market_cpcs_input', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),         textInput(inputId = 'location', label = 'Location:', value = ''),
        selectInput(inputId = 'trends_market_cpcs_input', label = 'Patent Codes:', choices = unique_patent_codes, multiple = TRUE),
        textInput(inputId = 'industry', label = 'Industry:', value = ''),
        actionButton(inputId = 'generate_tech_trends', label = 'Run Analysis', class = 'btn-primary')
      ),
      column(
        width = 8,
        h4('Results:'),
        textOutput(outputId = 'selected_trends_patent_codes'),
        #plotOutput(outputId = 'trends_plot', height = '500px')
        plotlyOutput(outputId = 'trends_plot', height = '500px')
      )
    )
  )
)




# server ------------------------------------------------------------------

server <- function(input,output,session) {
  
  
  selected_codes <- reactiveValues(competition = "", trends = "")
  
  competition <- reactiveValues(dt = data.frame(), plot = plotly_empty())
  trends <- reactiveValues(plot = plotly_empty())
  
  
  # OBSERVE EVENT -- competetive positioning
  observeEvent(input$generate_competitive_positioning, {
    #select codes
    selected_codes$competition <- paste("Selected patent codes:", paste(input$market_cpcs_input, collapse = ", "))

    #filter the cpc codes
    #bring in smaller merged data 
    dt <- small_data
    
    ## to use when we have larger data
    #dt <- cpc %>%  filter(grepl(pattern = paste(input$market_cpcs_input, sep = "", collapse = "|", x = cpc$cpc_group,ignore.case = T))) ## change market_cpcs_input (david) to patent_codes?
    # #merge with patents
    # dt <- merge(dt, patent, by = "patent_id")
    # #merge with assignee
    # dt <- merge(dt, assignee, by = "patent_id")
    #probably merge in location data here too, by "location_id"
    

 ##### copy in other COMPETETIVE POSITIONING code ######
    
    # Get top 10 companies   (their # of patents)
    totals <- dt %>% 
      filter(disambig_assignee_organization!='') %>% #drop those with no name
      group_by(disambig_assignee_organization) %>% 
      summarize(total=uniqueN(patent_id))  %>% #count unique, doesn't count duplicates
      arrange(desc(total)) %>% 
      slice(1:10)
    #totals <- totals[order(totals$total,decreasing = T),] %>% slice(1:10)  #same as last two rows above
    
    
    ### Calculate 5 year CAGR for top 10 companies
    #cagr = compound annual growth rate. basically (new rate/old rate)
    #create dataframe wiht all combinatinos of top 10 companies and years we want
    cagr <- data.frame(expand.grid(year=2017:2021,disambig_assignee_organization=totals$disambig_assignee_organization))
    #expand grid gives all combinations of two variables
    
    temp <- dt %>% 
      filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>% 
      group_by(year=year(patent_date),disambig_assignee_organization) %>% 
      summarise(n=uniqueN(patent_id))
    
    cagr <- merge(cagr,temp,by = c('year','disambig_assignee_organization'),all.x = T)
    rm(temp)
    cagr[is.na(cagr)] <- 0 #replace missing with 0?
    
    cagr <- cagr %>%
      group_by(disambig_assignee_organization) %>%
      mutate(cum_cnt = cumsum(n)) %>%  # make sure your date are sorted correctly before calculating the cumulative :)
      filter(year %in% c(2017,2021)) %>%
      pivot_wider(id_cols = disambig_assignee_organization,names_from = year,values_from = cum_cnt)
    cagr$cagr_2017_2021 <- round(((cagr$`2021`/cagr$`2017`)^(1/5))-1,3)
    
    
    # Calculate avg claim count for top 10 companies
    #more claims is better
    claims <- dt %>% 
      filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>%
      select(disambig_assignee_organization,patent_id,num_claims) %>%
      unique() %>%
      group_by(disambig_assignee_organization) %>%
      summarise
    
    # Combine and save file
    totals <- merge(totals,cagr,by = 'disambig_assignee_organization')
    totals <- merge(totals,claims,by = 'disambig_assignee_organization')
    totals <- totals %>% select(-`2017`,-`2021`)
    
    

    #competition$dt <- head(dt)
    competition$dt <- totals 
    output$competition_dt <- renderDataTable({competition$dt})
    
  })    
    
  
  
##### TECHNOLOGY TRENDS CODE #######
  #OBSERVE EVENT -- technology trends
  observeEvent(input$generate_tech_trends, {   
    
    #selected_codes$trends <- paste("Selected patent codes:", paste(input$market_cpcs_input, collapse = ", "))
    selected_codes$trends <- paste("Selected patent codes:", paste(input$trends_market_cpcs_input, collapse = ", "))
    
    # this isn't needed for the small data
    # keep <- small_data %>% 
    #   mutate(patent_id = as.character(patent_id)) %>% 
    #   #filter(grepl(pattern = selected_patent_codes, x = dt$cpc_group,ignore.case = T)) %>%
    #   filter(grepl(pattern = selected_codes$trends, x = small_data$cpc_group,ignore.case = T)) %>% 
    #   select(patent_id) %>%
    #   unique()
    
  # #MAP 2 currently doesn't change by patent code
    #tidy up the location data
    # dt$state_fips <- str_pad(string = dt$state_fips,width = 2,side = 'left', pad = '0')
    # dt$county_fips <- str_pad(string = dt$county_fips,width = 3,side = 'left', pad = '0')
    # dt$fips <- paste(dt$state_fips,dt$county_fips,sep = '')
    # 
    # #second map -- currently doesn't change depending on the codes selected
    # dt_state <- dt %>% group_by(disambig_state,state_fips) %>% summarise(n=uniqueN(patent_id))
    # 
    # l <- list(color = toRGB("white"), width = 2)
    # g <- list(
    #   scope = 'usa',
    #   projection = list(type = 'albers usa'),
    #   showlakes = TRUE,
    #   lakecolor = toRGB('white')
    # )
    # fig <- plot_geo(dt_state, locationmode = 'USA-states')
    # fig <- fig %>% add_trace(
    #   z = ~n,
    #   text = ~disambig_state,
    #   locations = ~disambig_state,
    #   color = ~n,
    #   colors = 'Blues'
    # )
    # fig <- fig %>% colorbar(title = "Count of patents")
    # fig <- fig %>% layout(
    #   title = 'Cyber security patents granted by State', #change title?
    #   geo = g
    # )
    # trends$plot <- fig # Added this line
    
    
    # #S curve

    segments_names <- c('segment 1','segment 2','segment 3','segment 4','segment 5')
    segments_codes <- c('G06F21', 'H04L63', 'H04L9', 'H04W12', 'H04K1')

    # First we need to identify patents that contain machine learning methods
    # This can be done by looking at the CPC codes and/or the text

    # cpc codes to flag
    ml_cpcs <- c('G06N3/02','G06N3/08','G06N3/12','G06N5','G06N20','G10L15/02', 'G10L15/075', 'G10L15/08', 'G10L15/014','G10L15/16',
                 'G10L15/18', 'G10L17/02', 'G10L17/04', 'G10L17/16', 'G10L17/18', 'G10L25/30', 'G10L25/33', 'G10L25/36', 'G10L25/39')

    # keywords to flag
    ml_keywords <- c('machine learning', 'deep learning', 'statistical learning','neural network')


    dt$ml <- ifelse(grepl(pattern = paste(ml_cpcs,collapse = '|',sep = ''),x = dt$cpc_group,ignore.case = T),1,0)
    table(dt$ml)
    head(dt$cpc_group)
    dt$ml <- ifelse(grepl(pattern = paste(ml_keywords,collapse = '|',sep = ''),x = dt$patent_title,ignore.case = T),1,dt$ml)
    dt$ml <- ifelse(grepl(pattern = paste(ml_keywords,collapse = '|',sep = ''),x = dt$patent_abstract,ignore.case = T),1,dt$ml)
    table(dt$ml)

    # add sub categories
    dt$segment <- NA
    for (i in 1:length(segments_names)) {
      dt$segment <- ifelse(grepl(pattern = segments_codes[i],x = dt$cpc_group,ignore.case = T),segments_names[i],dt$segment)
    }
    dt$segment[is.na(dt$segment)] <- 'Other'
    table(dt$segment,useNA = 'ifany')

    # Questions
    # what has adoption of machine learning looked like overall?
    temp <- dt %>% filter(ml==1) %>% group_by(year=year(patent_date)) %>% summarise(n=uniqueN(patent_id))
    temp <- temp[order(temp$year,decreasing = F),]
    fig <- plot_ly(temp, x = ~year, y = ~n, type = 'bar')


    trends$plot <- fig
    
    
    
    
    
    
    
  })

  
  
  
  
  output$selected_patent_codes <- renderText({
    selected_codes$competition
  })
  
  
  output$selected_trends_patent_codes <- renderText({
    selected_codes$trends
  })

  
  
  output$trends_plot <- renderPlotly({ 
    trends$plot
  })  
  
  
}



shinyApp(ui, server)

