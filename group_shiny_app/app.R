library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

"%!in%" <- Negate("%in%")

coral <- read_csv(here("data", "coral_data", "perc_cover_long.csv")) %>% 
  clean_names() %>% 
  mutate(tax = taxonomy_substrate_functional_group) %>% 
  select( - taxonomy_substrate_functional_group) %>% 
  filter(tax %!in% c("Sand", "Turf", "Macroalgae", "Crustose Coralline Algae / Bare Space")) %>%
  mutate(date = ym(date)) %>%
    separate(col = date,
             into = c("year", "month"),
             sep = "-",
             extra = "merge",
             remove = TRUE) %>%
  mutate(year = as.character(year))

coral_cov_mean <- coral %>% 
  select(year, site:tax) %>% 
  group_by(year, site) %>% 
  summarize(percent_cover_mean = sum(percent_cover)/120)
    

fish <- read_csv(here("data", "fish_data", "annual_fish_survey.csv")) %>% 
  clean_names() %>% 
  select("year", "location", "taxonomy", "family", "count") %>% 
  mutate(count = as.numeric(count))

counts <- fish %>% 
  count(taxonomy, wt = count)

ui <- fluidPage(
    navbarPage(theme = bs_theme(bootswatch = "darkly"),
               "Mo'orea Coral Reef LTER",
               tabPanel("About",
                        sidebarLayout(
                            sidebarPanel("Developers: Deanna Elliott, Mia Guarnieri, Mari Herbst De Cortina",
                                         br(),
                                         " ",
                                         br(),
                                         "We are current graduate students at the Bren School of Environmental Science
                                         & Management, working towards Masters of Environmental Science and Management."
                            ),
                            mainPanel("This app visualizes data on coral cover and reef fish species abundance 
                                during four 'normal' years and two years when bleaching events occured in the 
                                Mo'orea Coral Reef LTER site.",
                                br(),
                                " ",
                                br(),
                               "The Mo'orea Coral Reef Long-Term Ecological Research site in French Polynesia
                                was established by the National Science Foundation in 2004. It is a research 
                                partner to the University of California, Santa Barbara and the University of
                                California, Northridge. Its purpose is as a model for exploring factors that 
                                mediate coral community structure and function, particularly external, 
                                anthropogenic drivers of disturbance.")
                        )),
               tabPanel("Coral Cover By Year",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select", 
                                            inputId = "cor_year",
                                            label = h3("Select Year"), 
                                            choices = list("2005" = 1, "2006" = 2, "2007" = 3, "2008" = 4,
                                                           "2009" = 5, "2010" = 6, "2011" = 7, "2012" = 8,
                                                           "2013" = 9, "2014" = 10, "2015" = 11, "2016" = 12,
                                                           "2017" = 13, "2018" = 14, "2019" = 15, "2020" = 16), 
                                            selected = 1)
                            )), # end of sidebarLayout
                            mainPanel(
                                "Use this tool to visualize differences in average coral cover at research sites between years.",
                                mainPanel(
                                  plotOutput(outputId = "cor_cov") #widget 2 output
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Coral Species Differences Between Years",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select",
                                            inputId = "coral_site_select",
                                            label = h3("Select Site"),
                                            choices = list("LTER 1" = "LTER 1", "LTER 2" = "LTER 2", "LTER 3" = "LTER 3",
                                                           "LTER 4" = "LTER 4", "LTER 5" = "LTER 5", "LTER 6" = "LTER 6")),
                                selectInput("select",
                                            inputId = "coral_transect_select",
                                            label = h3("Select Transect"),
                                            choices = list("1" = 1, "2" = 2, "3" = 3,
                                                           "4" = 4, "5" = 5)),
                                selectInput("select",
                                            inputId = "coral_quadrat_select",
                                            label = h3("Select Quadrat"),
                                            choices = list("1" = 1, "2" = 2, "3" = 3,
                                                           "4" = 4, "5" = 5, "6" = 6,
                                                           "7" = 7, "8" = 8)),
                                checkboxGroupInput("checkGroup",
                                                   inputId = "coral_year",
                                                   label = h3("Select years"), 
                                                   choices = list("2005" = 1, "2006" = 2, "2007" = 3, "2008" = 4,
                                                                  "2009" = 5, "2010" = 6, "2011" = 7, "2012" = 8,
                                                                  "2013" = 9, "2014" = 10, "2015" = 11, "2016" = 12,
                                                                  "2017" = 13, "2018" = 14, "2019" = 15),
                                                   selected = 1)
                                                  # selected = "2005") # WHY IS SELECTED BROKEN
                            ),  # end of sidebarPanel
                            mainPanel("Use this tool to visualize differences in coral species abundance at research sites between years. Non-coral species or substrates are not included in the data,
                                      so percent cover may not combine to 100%.",
                                      br(), " ",
                                      br(), " ",
                                      "Select a year to remove error message and view graph!",
                                      br(), " ",
                                      br(), " ",
                                plotOutput(outputId = "coral_abun"),
                                br(), " ", br(), " ",
                                "Data Citation: Moorea Coral Reef LTER and P. Edmunds. 2020. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Corals, ongoing since 2005 ver 38. Environmental Data Initiative. https://doi.org/10.6073/pasta/10ee808a046cb63c0b8e3bc3c9799806 (Accessed 2022-02-22)."
                            ) #end of mainPanel 3
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                              sliderInput("yr_slider", label = h3("Select Time Scale"), min = 2005, 
                                          max = 2020, value = c(2010, 2011), sep = NULL)
                            ), #end of sidebarPanel
                            mainPanel(
                                plotOutput(outputId = "fish_ab")
                            ) #end of mainPanel
                        )), #end of sidePanel, W3
               tabPanel("Timescale - Bleaching & Recovery",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("slider1", label = h3("Select Time Scale"), min = 0, 
                                            max = 100, value = c(40, 60))
                            ),  #end of sidebarPanel
                            mainPanel(
                                "OUTPUT",
                                verbatimTextOutput("range") #widget 4 output
                            ) #end of mainPanel 4
                        )) #end of sidePanel, W4
    ))  # end of navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # output widget 1
     coral_cov <- reactive ({
       coral_cov_mean %>%
         filter(year == input$cor_year)
    
  })
  
  output$coral_ <- renderPlot({
    ggplot(data = coral_abun, 
           aes(x="", y=percent_cover, fill=site)) +
      geom_bar(width = 1, stat = "identify")
    })

    
    #output widget 2
    coral_abun <- reactive ({
        coral %>%
            filter(site == input$coral_site_select) %>%
            filter(year == as.numeric(input$coral_year)) %>%
        filter(transect == input$coral_transect_select) %>%
        filter(quadrat == input$coral_quadrat_select)
      return(coral_abun)
    })
    
    output$coral_abun <- renderPlot({
        ggplot(data = coral_abun(), aes(x = year, y = percent_cover)) +
                   geom_col(aes(fill = tax)) +
        facet_wrap(~ year) +
           # theme(axis.title.x=element_blank(),
            #      axis.text.x=element_blank(),
            #      axis.ticks.x=element_blank()) +
        labs(x = "Year", y = "Percent cover",
             fill = "Species") +
        theme_minimal()
    })
    
    # widget 3 output
    fish_select <- reactive ({
        top_fish <- fish %>%
        filter(year %in% input$yr_slider[1]:input$yr_slider[2]) %>% 
        count(taxonomy, wt = count) %>% 
        slice_max(order_by = n, n = 10)
    
    fish_count <- fish %>% 
      filter(taxonomy %in% top_fish$taxonomy) %>%
      filter(year %in% input$yr_slider[1]:input$yr_slider[2]) %>% 
      group_by(year) %>% 
      count(taxonomy, wt = count)
    
    return(fish_count)
    })
    
    
    output$fish_ab <- renderPlot({
        ggplot(data = fish_select(), aes(x = year, y = n)) + 
        geom_line(aes(color = taxonomy, group = taxonomy)) +
        geom_point(aes(color = taxonomy)) %>% 
        labs(x = "Year", y = "Abundance", color = "Species")
            }) # end output widget 3
    
    #output widget 4
    output$range <- renderPrint({ input$slider1 })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

