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

coral <- read_csv(here("data","coral_data", "coral.csv"))

coral_cov_mean <- read_csv(here("data", "coral_data", "coral_cover.csv"))

coral_spp <- read_csv(here("data", "coral_data", "coral_spp.csv"))

bleach_2016 <- read_csv(here("data", "bleaching_data", "bleach_2016.csv"))

fish <- read_csv(here("data", "fish_data", "fish.csv"))

counts <- read_csv(here("data", "fish_data", "fish_counts.csv"))

#spatial bleaching data
bleach_2016_sf <- bleach_2016 %>% 
  drop_na(latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) 

st_crs(bleach_2016_sf) <- 4326


ui <- fluidPage(
    navbarPage(theme = bs_theme(bootswatch = "darkly"),
               "Mo'orea Coral Reef LTER",
               tabPanel("About",
                        sidebarLayout(
                            sidebarPanel("Developers: Deanna Elliott, Mia Guarnieri, Mari Herbst de Cortina",
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
                                during 13 'normal' years and two years when bleaching events occured in the 
                                Mo'orea Coral Reef LTER site.",
                                br(),
                                " ",
                                br(),
                               "The Mo'orea Coral Reef Long-Term Ecological Research site in French Polynesia,
                                established by the National Science Foundation in 2004, is a research 
                                partner to the University of California, Santa Barbara and California State University at Northridge. It is used as a model to understand how external
                               and anthropogenic forces affect coral community structure and function around disturbances.",
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
                                            choices = unique(coral_cov_mean$year)),
                                "Data: Edmunds, P. 2020"
                            ), # end of sidebarLayout
                            mainPanel(
                                "Use this tool to visualize differences in average coral cover at research sites between years.",
                                br(), " ", br(), " ", br(),
                                plotOutput(outputId = "coral_cov")
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Coral Species by Year",
                        sidebarLayout(
                          sidebarPanel(
                           "Select your site of interest.",
                            selectInput("select",
                                        inputId = "coral_site",
                                        label = h3("Select Site"),
                                        choices = unique(coral_spp$site),
                                        selectize = FALSE),
                            "Select a year or years for comparison.",
                            checkboxGroupInput("checkGroup",
                                               inputId = "coral_year",
                                               label = h3("Select Year(s)"),
                                               choices = unique(coral_spp$year)
                                               ),
                           "Data: Edmunds, P. 2020"
                          ),
                          mainPanel("Use this tool to visualize differences in coral species abundance.
                                    Non-coral species and substrates are not included in this analysis, so 
                                    percentages will likely not sum to 100%.",
                                    br(), " ", br(), " ", "Select a year to view graph.",
                                    br(), " ", br(), " ", br(),
                                    plotOutput(outputId = "coral_species")
                          )
                        )),
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                              sliderInput("yr_slider", label = h3("Select Time Scale"), min = 2005, 
                                          max = 2019, value = c(2010, 2011), step = 1, sep = ""),
                              "Data: Brooks, A. 2022"
                            ), #end of sidebarPanel
                            mainPanel(
                              "Use this tool to visualize fish abundances across years. Only 
                              shows the top 10 most abundant fish species.",
                              br(), " ", br(), " ", br(),
                                plotOutput(outputId = "fish_ab"),
                                br(), " ", br(),
                                div(img(src = "damselfish.png", height = 400, width = 500), style="text-align: center;"),
                                br(), " ", br(),
                                div(img(src = "Surgeonfish.png", height = 400, width = 500), style="text-align: center;"),
                                br(), " ", br(),
                                div(img(src = "parrotfish_wrasse.png", height = 400, width = 500), style="text-align: center;"),
                                br(), " ", br()
                            ) #end of mainPanel
                        )), #end of sidePanel, W3
               tabPanel("Visualizing Bleaching",
                        sidebarLayout(
                            sidebarPanel("Toggle on or off to view bleached or unbleached coral 
                                         colonies:",
                              switchInput(
                                inputId = "bleach_switch",
                                label = "Bleaching", 
                                labelWidth = "80px",
                                onStatus = "success", 
                                offStatus = "danger"),
                              "If you have coral bleaching turned on, slide to view coral 
                              colonies by minimum extent of bleaching:",
                              sliderInput("bleach_slider",
                                label = h4("Percent Bleached"),
                                min = 0,
                                max = 100,
                                value = 0),
                              "Data: Burkepile, D. and Adam, T. 2019"
                            ),  #end of sidebarPanel
                            mainPanel("Use this tool to visualize location and extent of coral 
                                      bleaching from the 2016 bleaching event. Click on a colony 
                                      to view its class size and percent bleached.",
                                      br(), " ", br(), " ", br(),
                              tmapOutput(outputId = "bleach_perc"), #widget 4 output
                              br(), " ", br(),
                              "Between February and May of 2016, water temperature exceeded the 
                              threshold for heat stress in corals for 70 straight days. This resulted 
                              in the bleaching of several coral colonies within the lagoon around the 
                              island.", br(), " ", br(),
                              "These data show the percent extent of bleaching of Pocillopora and 
                              Acropora coral colonies categorized into five size classes, indicated 
                              on this map according to circle size:", br(), " ", br(),
                              "1: 0-10cm (smallest circles)", br(),
                              "2: 11-20cm", br(),
                              "3: 21-30cm", br(),
                              "4: 31-40cm", br(),
                              "5: above 40cm (largest circles)"
                            ) #end of mainPanel 4
                        )) #end of sidePanel, W4
    )) # end of navbarPage




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
    coral_species <- reactive({
      coral_spp %>% 
        filter(site == input$coral_site) %>% 
        filter(year %in% c(as.numeric(input$coral_year)))
    })
    
    output$coral_species <- renderPlot({
      ggplot(data = coral_species(), aes(x = year, y = percent_cover_mean)) + # maybe can use group = site?
        geom_col(aes(fill = tax)) +
        labs(x = "Year", y = "% Cover",
             fill = "Species") +
        theme_classic()
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
        labs(x = "Year", y = "Abundance", color = "Species") +
        theme_classic()
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
                alpha = 0.7,
                popup.vars = c("size class" = "colony_size_class", "percent bleached" = "percent_bleached"),
                id = "taxa")
    }) # end output widget 4
    
    output$switch <- renderPrint(input$bleach_switch)
}

# Run the application 
shinyApp(ui = ui, server = server)