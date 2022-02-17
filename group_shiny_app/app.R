library(shiny)
library(bslib)
library(palmerpenguins)
library(tidyverse)
library(here)
library(janitor)

"%!in%" <- Negate("%in%")

coral <- read_csv(here("data", "coral_data", "perc_cover_long.csv")) %>% 
  clean_names() %>% 
  mutate(tax = taxonomy_substrate_functional_group) %>% 
  select( - taxonomy_substrate_functional_group) %>% 
  filter(tax %!in% c("Sand", "Turf", "Macroalgae", "Crustose Coralline Algae / Bare Space")) 

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
                                verbatimTextOutput("value"), #widget 2 output
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Comparative Yearly Species Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select",
                                            inputId = "fish_year_select",
                                            label = h3("Select year"),
                                            choices = list("2007" = 2007, "2008" = 2008, "2009" = 2009, 
                                                           "2010" = 2010))
                            ),  # end of sidebarPanel
                            mainPanel(
                                plotOutput(outputId = "pg_abun")
                            ) #end of mainPanel 3
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                              sliderInput("yr_slider", label = h3("Select time scale"), min = 2006, 
                                          max = 2020, value = c(2010, 2011), sep = NULL)
                            ), #end of sidebarPanel
                            mainPanel(
                                plotOutput(outputId = "fish_ab")
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
    )  # end of navbarPage
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #output widget 2
    penguin_abun <- reactive ({
        penguins %>%
            filter(year == input$fish_year_select) %>%
            group_by(species) %>%
            summarize(count = n())
    })
    
    output$pg_abun <- renderPlot({
        ggplot(data = penguin_abun(), aes(x = species, y = count)) +
                   geom_col()
    })
    
    # widget 3 output
    year_select <- reactive ({
        fish %>%
        filter(year %in% input$yr_slider[1]:input$yr_slider[2]) %>% 
        group_by(year) %>% 
        count(taxonomy, wt = count) %>% 
        slice_max(order_by = n, n = 10)
    })
    
    output$fish_ab <- renderPlot({
        ggplot(data = year_select(), aes(x = year, y = n)) +
            geom_point(aes(color = taxonomy))
            }) # end output widget 3
    
    #output widget 4
    output$range <- renderPrint({ input$slider1 })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

