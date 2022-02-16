library(shiny)
library(bslib)
library(palmerpenguins)
library(tidyverse)

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
                                         & Management, working towards Master's of Environmental Science and Management."
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
                                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                            selected = 1)
                            ), # end of sidebarLayout
                            mainPanel(
                                "OUTPUT GOES HERE",
                                verbatimTextOutput("value"), #widget 2 output
                            ) # end of mainPanel2
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select",
                                            inputId = "fish_year_select",
                                            label = h3("Select year"),
                                            choices = list("2007" = 2007, "2008" = 2008, "2009" = 2009))
                            ),  # end of sidebarPanel
                            mainPanel(
                                plotOutput(outputId = "pg_abun")
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
                                            inputId = "pg_year",
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

