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
    

fish <- read_csv(here("data", "fish_data", "annual_fish_survey.csv")) %>% 
  clean_names() %>% 
  select("year", "location", "taxonomy", "family", "count")

ui <- fluidPage(
    navbarPage(theme = bs_theme(bootswatch = "darkly"),
               "Mo'orea Coral Reef LTER",
               tabPanel("About",
                        sidebarLayout(
                            sidebarPanel("Authors: Deanna Elliott, Mia Guarnieri, Mari Herbst De Cortina",
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
                                            label = h3("Select Year"), 
                                            choices = list("2005" = 1, "2006" = 2, "2007" = 3, "2008" = 4,
                                                           "2009" = 5, "2010" = 6, "2011" = 7, "2012" = 8,
                                                           "2013" = 9, "2014" = 10, "2015" = 11, "2016" = 12,
                                                           "2017" = 13, "2018" = 14, "2019" = 15, "2020" = 16), 
                                            selected = 1)
                            ), # end of sidebarLayout
                            mainPanel(
                                "OUTPUT GOES HERE",
                                verbatimTextOutput("value") #widget 2 output
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Coral Coverage Differences Between Years",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select",
                                            inputId = "coral_site_select",
                                            label = h3("Select site"),
                                            choices = list("LTER 1" = "LTER 1", "LTER 2" = "LTER 2", "LTER 3" = "LTER 3",
                                                           "LTER 4" = "LTER 4", "LTER 5" = "LTER 5", "LTER 6" = "LTER 6")),
                                selectInput("select",
                                            inputId = "coral_transect_select",
                                            label = h3("Select transect"),
                                            choices = list("1" = 1, "2" = 2, "3" = 3,
                                                           "4" = 4, "5" = 5)),
                                selectInput("select",
                                            inputId = "coral_quadrat_select",
                                            label = h3("Select quadrat"),
                                            choices = list("1" = 1, "2" = 2, "3" = 3,
                                                           "4" = 4, "5" = 5, "6" = 6,
                                                           "7" = 7, "8" = 8)),
                                checkboxGroupInput("checkGroup",
                                                   inputId = "coral_year",
                                                   label = h3("Select years"), 
                                                   choices = list("2005" = 2005, "2006" = 2006, "2007" = 2007, "2008" = 2008, "2009" = 2009,
                                                                  "2010" = 2010, "2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                  "2015" = 2015, "2016" = 2016, "2017" = 2017, "2018" = 2018, "2019" = 2019))
                                                  # selected = "2005") # WHY IS SELECTED BROKEN
                            ),  # end of sidebarPanel
                            mainPanel("Use this tool to visualize differences in coral species abundance at research sites between years.",
                                      br(), " ",
                                      "Select a year to remove error message!",
                                plotOutput(outputId = "coral_abun")
                            ) #end of mainPanel 3
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Comparative Yearly Species Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("checkGroup",
                                             inputId = "pg_sex",
                                             label = h3("Fish or Coral?"),
                                             choices = list("male" = "male", "female" = "female")),
                                selectInput("select",
                                            label = h3("Select year"),
                                            choices = list("2007" = 2007, "2008" = 2008, "2009" = 2009))
                            ), #end of sidebarPanel
                            mainPanel(
                                plotOutput(outputId = "pg_plot")
                            ) #end of mainPanel
                        )), #end of sidePanel, W3
               tabPanel("Timescale - Bleaching & Recovery",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("slider1", label = h3("Select time scale"), min = 0, 
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
    
    #output widget 2
    coral_abun <- reactive ({
        coral %>%
            filter(site == input$coral_site_select) %>%
            filter(year == as.numeric(input$coral_year)) %>%
        filter(transect == input$coral_transect_select) %>%
        filter(quadrat == input$coral_quadrat_select)
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
    penguin_select <- reactive ({
        penguins %>%
            filter(year == input$pg_year) %>%
            filter(sex == input$pg_sex)
    })
    
    output$pg_plot <- renderPlot({
        ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
            geom_jitter(aes(color = species))
    }) # end output widget 3
    
    #output widget 4
    output$range <- renderPrint({ input$slider1 })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

