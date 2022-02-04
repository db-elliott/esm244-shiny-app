library(shiny)
library(palmerpenguins)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Morea Coral Reef LTER",
               tabPanel("About",
                       sidebarLayout(
                            sidebarPanel("This is where words about us will go."
                                         ),
                            mainPanel(
                            "This is where words about our app will go.")
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
                                "OUTPUT GOES HERE"
                            ) # end of mainPanel
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Yearly Fish Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("select",
                                            inputId = "year",
                                            label = h3("Select year"),
                                            choices = list("x" = "x", "y" = "y", "z" = "z"))
                            ),  # end of sidebarPanel
                            mainPanel(
                                "OUTPUT GOES HERE"
                            ) #end of mainPanel
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Comparative Yearly Species Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("radio",
                                             inputId = "pg_sex",
                                             label = h3("Fish or Coral?"),
                                             choices = list("male", "female")),
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
                                "OUTPUT"
                            ) #end of mainPanel
                        )) #end of sidePanel, W4
    )  # end of navbarPage
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    penguin_select <- reactive ({
        penguins %>%
            filter(year == input$pg_year) %>%
            filter(sex == input$pg_sex)
    })
    
    # widget 3 output
    output$pg_plot <- renderPlot({
        ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
            geom_jitter(aes(color = species))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
