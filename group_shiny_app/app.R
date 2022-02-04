library(shiny)

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
                                "OUTPUT GOES HERE",
                                verbatimTextOutput("value"), #widget 2 output
                            ) # end of mainPanel2
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
                            ) #end of mainPanel 3
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Comparative Yearly Species Abundance",
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("radio",
                                             label = h3("Fish or Coral?"),
                                             choices = list("Fish", "Coral")),
                                dateRangeInput("dates",
                                               label = h3("Select date range"))
                            ), #end of sidebarPanel
                            mainPanel(
                                "OUTPUT"
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
    output$value <- renderPrint({ input$select })
    
    #output widget 4
    output$range <- renderPrint({ input$slider1 })

    #output widget 2
    output$value <- renderPrint({ input$select })
    
    #output widget 4
    output$range <- renderPrint({ input$slider1 })
}

# Run the application 
shinyApp(ui = ui, server = server)
