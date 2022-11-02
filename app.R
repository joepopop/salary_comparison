# get data from reliable source or explain the nature of data really difficult to create accurate index (-> that's why broke down into more specific details: use RELIABLE SOURCES)
# add more colors (pure aesthetics and maybe for detailed indeces)

# load packages ----
library(rvest)
library(tidyverse)
library(shiny)
library(bslib)
library(thematic)

# apply theme over ggplot visualization ----
thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = "auto",
  sequential = sequential_gradient(),
  qualitative = okabe_ito(),
  inherit = FALSE,
  session = shiny::getDefaultReactiveDomain()
)

# load data from numbeo.com ---- 
index <- "https://www.numbeo.com/cost-of-living/rankings.jsp?title=2022&displayColumn=-1" %>% 
  read_html() %>% 
  html_table() %>% 
  tibble() %>% 
  unnest() %>% 
  select(-X1, -X2, -Rank) %>% 
  na.omit() %>% 
  janitor::clean_names()

# get lists of cities and countries (to be used in drop down options in UI) ----
city_list <- index %>% pull(city)
country_list <- index %>% 
  mutate(country = sub('.*\\,', '', city)) %>% distinct(country) %>% pull() 

# define UI ----
ui <- fluidPage(
  
    # apply theme
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    # add title
    titlePanel("Salary comparison across cities"),
    
    # specify inputs, conditional panels, and source on sidebar
    sidebarLayout(
        sidebarPanel(
            # specification of salary
            sliderInput(
              "salary",
              "Salary:",
              min = 10000,
              max = 500000,
              value = 70000
              ),
            # specification of main city 
            selectInput(
              "city",
              "City:",
              city_list,
              selected = "Chicago, IL, United States"
            ),
            # specification of how to choose comparison cities
            selectInput(
              inputId = "select_cities",
              label = "Select comparison cities",
              choices = c(" ", "By country" = 1, "Manually" = 2),
              selected = 2
            ),
            # conditional panel that shows up when user chooses to select comparison cities by country
            conditionalPanel(
              condition = "input.select_cities == 1",
              selectInput(
                inputId = "country",
                label = "Country:",
                choices = country_list,
                selected = c("United States", "Japan"),
                multiple = F
              )
            ),
            # conditional panel that shows up when user chooses to select comparison cities manually
            conditionalPanel(
              condition = "input.select_cities == 2",
              selectInput(
                inputId = "manual_cities",
                label = "Cities:",
                choices = city_list,
                selected = c("New York, NY, United States", "Tokyo, Japan", "London, United Kingdom", "Paris, France"),
                multiple = T
                
              )
            ),
            # specification to account for rent or not
            checkboxInput(
              "index",
              "Account for rent",
              value = T
            ),
            br(),
            br(),
            p(),
              # put source link
              a("Source:"),
              a("NUMBEO", 
                href = "https://www.numbeo.com/cost-of-living/rankings.jsp?title=2022&displayColumn=-1")
        ),
        
        # visualize bar chart with two variations: 1) when a country is chosen to select cities and 2) when specific cities are manually chosen
        mainPanel(
          conditionalPanel(
            condition = "input.select_cities == 1",
            plotOutput("distPlot")
          ),
          conditionalPanel(
            condition = "input.select_cities == 2",
            plotOutput("distPlot2")
          )
        )
    )
)

# define server ----
server <- function(input, output) {
    
    # output graph for when a country is chosen
    output$distPlot <- renderPlot({
    
      # require a country input before loading
      req(input$country)
      
      # store index choice as either cost of living accounting for rent vs not accounting for rent
      index_choice <- if_else(input$index == T,
                              "cost_of_living_plus_rent_index",
                              "cost_of_living_index"
                              )
      
      # extract relevant index column from data as vector
      your_index <- index %>% 
        filter(grepl(input$city, city)) %>%
        select(index_choice) %>% 
        pull()
      
      # generate plot
      index %>% 
        separate(city, into = c("city", "country"), sep = ",(?=[^,]+$)") %>% 
        filter(country %in% input$country) %>%
        arrange(desc(index_choice)) %>%
        group_by(country) %>% 
        mutate(id = row_number()) %>% 
        ungroup() %>% 
        filter(id < 6) %>% 
        mutate(salary = (get(index_choice) + (100 - your_index)) / 100 * input$salary) %>% 
        ggplot(aes(city, salary)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(breaks = waiver(), n.breaks = 6) +
        labs(
          x = "Cities",
          y = "Salary"
        ) +
        theme(
          panel.background = element_blank(),
          panel.grid = element_line(color = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank()
        )
      
    })
    
    # output graph for when specific cities are manually chosen
    output$distPlot2 <- renderPlot({
      
      # store index choice as either cost of living accounting for rent vs not accounting for rent
      index_choice <- if_else(input$index == T,
                              "cost_of_living_plus_rent_index",
                              "cost_of_living_index"
                              )
      
      # extract relevant index column from data as vector
      your_index <- index %>% 
        filter(grepl(input$city, city)) %>%
        select(index_choice) %>% 
        pull()
      
      # generate plot
      index %>% 
        filter(city %in% input$manual_cities) %>%
        mutate(
          city = str_remove(city, ",\\s[A-Za-z\\s]+$"),
          salary = (get(index_choice) + (100 - your_index)) / 100 * input$salary
          ) %>% 
        ggplot(aes(city, salary)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(breaks = waiver(), n.breaks = 6) +
        labs(
          x = "Cities",
          y = "Salary"
        ) +
        theme(
          
          panel.background = element_blank(),
          panel.grid = element_line(color = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank()
        )
      
    })
}

# run app
shinyApp(ui = ui, server = server)
