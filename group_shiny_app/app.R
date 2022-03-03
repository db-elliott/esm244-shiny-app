library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(shinyWidgets)

#reading in and wrangling data


"%!in%" <- Negate("%in%")

#coral data
coral <- read_csv(here("data", "coral_data", "perc_cover_long.csv")) %>% 
  clean_names() %>% 
  mutate(tax = taxonomy_substrate_functional_group) %>% 
  select( - taxonomy_substrate_functional_group) %>% 
  filter(tax %!in% c("Sand", "Turf", "Macroalgae", "Crustose Coralline Algae / Bare Space")) %>%
  mutate(case_when(
    tax == "Fungiidae unidentified" ~ "Unknown Fungiidae")) %>% 
  mutate(date = ym(date)) %>%
    separate(col = date,
             into = c("year", "month"),
             sep = "-",
             extra = "merge",
             remove = TRUE) %>%
  mutate(year = as.character(year))

#coral percent cover
coral_cov_mean <- coral %>% 
 select(year, site:tax) %>% 
  group_by(year, site) %>% 
  summarize(percent_cover_mean = sum(percent_cover)/120) %>% 
  filter(year != "2854")

#bleaching data
bleach_2016 <- read_csv(here("data", "bleaching_data", "bleaching_2016.csv")) %>% 
  clean_names()
bleach_adult_2019 <- read_csv(here("data", "bleaching_data", "adult_corals_exp_aug2019.csv"))
bleach_adult_2019_NS <- read_csv(here("data", "bleaching_data", "adult_corals_NS_oct2019.csv"))

#spatial bleaching data
bleach_2016_sf <- bleach_2016 %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) 

st_crs(bleach_2016_sf) <- 4326

#fish data
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
                                         br(), " ",
                                         br(),
                                         "We are current graduate students at the Bren School of Environmental Science
                                         & Management, working towards Masters of Environmental Science and Management.",
                                         br(), " ",
                                         br(), " ",
                                         br(), " ",
                                         br(),
                                         "Data Citations:",
                                         br(), " ",
                                         br(),
                                         "Moorea Coral Reef LTER and P. Edmunds. 2020. MCR LTER: Coral Reef: 
                                         Long-term Population and Community Dynamics: Corals, ongoing since 2005 ver 38.
                                         Environmental Data Initiative. https://doi.org/10.6073/pasta/10ee808a046cb63c0b8e3bc3c9799806 
                                         (Accessed 2022-02-22).",
                                         br(), " ",
                                         br(),
                                         "Moorea Coral Reef LTER and A. Brooks. 2022. MCR LTER: Coral Reef: Long-term Population 
                                         and Community Dynamics: Fishes, ongoing since 2005 ver 61. Environmental Data Initiative. 
                                         https://doi.org/10.6073/pasta/ad17fdfd89064fb57e1ac0cf26b32483 (Accessed 2022-02-22).",
                                         br(), " ",
                                         br(),
                                         "Moorea Coral Reef LTER, D. Burkepile, and T. Adam. 2019. MCR LTER: Coral Reef: Coral 
                                         bleaching with nitrogen and heat stress: 2016 data in support of Donovan et al. 
                                         submitted to PNAS ver 10. Environmental Data Initiative. https://doi.org/10.6073/pasta/
                                         57108aaeede00e77cac110bc5366a92b (Accessed 2022-02-22).",
                                         br(), " ",
                                         br(),
                                         "Moorea Coral Reef LTER, K.E. Speare, T.C. Adam, E.M. Winslow, H.S. Lenihan, and D.E. 
                                         Burkepile. 2021. MCR LTER: Coral Reefs: Coral bleaching and mortality in July 2019; 
                                         data for Speare et al. 2021 Global Change Biology ver 10. Environmental Data Initiative.
                                         https://doi.org/10.6073/pasta/f59968d039de006909c5c92c51c3919c (Accessed 2022-02-22)."
                            ), #end sidebarPanel
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
                                anthropogenic drivers of disturbance.",
                               br(), " ",
                               br(),
                              div(img(src = "mcr_coral.jpg", height = 400, width = 500), style="text-align: center;"),
                               br(), " ",
                              br(),
                               div(img(src = "mcr_lter_map.png", height = 600, width = 500), style="text-align: center;") 
                            )
                        )), 
               tabPanel("Coral Cover By Year",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select", 
                                            inputId = "cor_year",
                                            label = h3("Select Year"), 
                                            choices = unique(coral_cov_mean$year))
                            ), # end of sidebarLayout
                            mainPanel(
                                "Use this tool to visualize differences in average coral cover at research sites between years.",
                                plotOutput(outputId = "coral_cov")
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Coral Species By Year",
                        sidebarLayout(
                            sidebarPanel(
                              "Select your site of interest:",
                                selectInput("select",
                                            inputId = "coral_site_select",
                                            label = h3("Select Site"),
                                            choices = list("LTER 1" = "LTER 1", "LTER 2" = "LTER 2", "LTER 3" = "LTER 3",
                                                           "LTER 4" = "LTER 4", "LTER 5" = "LTER 5", "LTER 6" = "LTER 6")),
                              "Sites are further broken down by transect and quadrat. Please select which subsection you'd like to explore:",
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
                              "Select a year or years for comparison:",
                                checkboxGroupInput("checkGroup",
                                                   inputId = "coral_year",
                                                   label = h3("Select years"), 
                                                   choices = list("2005" = 2005, "2006" = 2006, "2007" = 2007, "2008" = 2008, "2009" = 2009,
                                                                  "2010" = 2010, "2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                  "2015" = 2015, "2016" = 2016, "2017" = 2017, "2018" = 2018, "2019" = 2019))
                                                   # selected = "2005")
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
                                br(), " ", br(), " "
                            ) #end of mainPanel 3
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                              sliderInput("yr_slider", label = h3("Select Time Scale"), min = 2005, 
                                          max = 2020, value = c(2010, 2011), sep = NULL)
                            ), #end of sidebarPanel
                            mainPanel("Use this tool to visualize fish abundances across years. Only shows the top 10 most abundant fish species.",
                                plotOutput(outputId = "fish_ab")
                            ) #end of mainPanel
                        )), #end of sidePanel, W3
               tabPanel("Visualizing Bleaching",
                        sidebarLayout(
                            sidebarPanel("Toggle on or off to view bleached or unbleached coral colonies:",
                              switchInput(
                                inputId = "bleach_switch",
                                label = "Bleaching", 
                                labelWidth = "80px",
                                onStatus = "success", 
                                offStatus = "danger"),
                              "Slide to view coral colonies by extent of bleaching:",
                              conditionalPanel(
                                condition = "input.bleach_switch = 'TRUE'",
                                sliderInput("bleach_slider", 
                                            label = h4("Percent Bleached"), 
                                            min = 0, 
                                            max = 100, 
                                            value = 0))
                            ),  #end of sidebarPanel
                            mainPanel("Use this tool to visualize location and extent of coral bleaching from the 2016 bleaching event.",
                              tmapOutput(outputId = "bleach_perc"), #widget 4 output
                            ) #end of mainPanel 4
                        )) #end of sidePanel, W4
    ))  # end of navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # output widget 1
  coral_cov <- reactive ({
    coral_cov_mean %>%
      filter(year == as.numeric(input$cor_year))
    
  })
  
  output$coral_cov <- renderPlot({
    ggplot(data = coral_cov(), 
           aes(x= site, y=percent_cover_mean, fill = site)) +
      geom_col(color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("chocolate1", "darkturquoise", 
                                    "indianred1", "goldenrod1", 
                                    "mediumspringgreen", "mediumorchid3")) +
      labs(x = "Site", y = "Average % Cover") +
      theme_classic()
  })
    
    #output widget 2
    coral_abun <- reactive ({
        coral %>%
            filter(site == input$coral_site_select) %>%
            filter(year == as.numeric(input$coral_year)) %>%
        filter(transect == input$coral_transect_select) %>%
        filter(quadrat == input$coral_quadrat_select)
    })
    
    output$coral_abun <- renderPlot({
        ggplot(data = coral_abun(), aes(x = year, y = percent_cover)) + # maybe can use group = site?
                   geom_col(aes(fill = tax)) +
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
        geom_line(aes(color = taxonomy, group = taxonomy), size = 2) +
        geom_point(aes(color = taxonomy)) %>% 
        labs(x = "Year", y = "Abundance", color = "Species")
            }) # end output widget 3
    
    #output widget 4
    bleach_percent <- reactive ({
      
      bleaching <- if(input$bleach_switch == FALSE) {
        filter(bleach_2016_sf, percent_bleached == 0)
      } else {
          filter(bleach_2016_sf, percent_bleached > 0)}
      
      per_bleach <- bleaching %>%
        filter(percent_bleached >= input$bleach_slider)

      return(per_bleach)
    })
    #make a map
    
    output$bleach_perc <- renderTmap({
      tmap_mode(mode = "view")
      
      tm_shape(bleach_percent()) +
        tm_dots(col = "taxa",
                size = "colony_size_class",
                alpha = 0.7)
    }) # end output widget 4
    
    output$switch <- renderPrint(input$bleach_switch)
}

# Run the application 
shinyApp(ui = ui, server = server)
