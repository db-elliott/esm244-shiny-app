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
                                "OUTPUT GOES HERE"
                            ) # end of mainPanel
                        )),  # end of sidebarLayout, tabPanel W1
               tabPanel("Widget 2",
                        sidebarLayout(
                            sidebarPanel(
                                "Dropdown for fish abundance/year"
                            ),  # end of sidebarPanel
                            mainPanel(
                                "OUTPUT GOES HERE"
                            ) #end of mainPanel
                        )), #end of sidebarLayout, tabPanel W2
               tabPanel("Widget 3",
                        sidebarLayout(
                            sidebarPanel(
                                "Select buttons for pie chart"
                            ), #end of sidebarPanel
                            mainPanel(
                                "OUTPUT"
                            ) #end of mainPanel
                        )), #end of sidePanel, W3
               tabPanel("Widget 4",
                        sidebarLayout(
                            sidebarPanel(
                                "Timescale slider for bleaching/recovery"
                            ),  #end of sidebarPanel
                            mainPanel(
                                "OUTPUT"
                            ) #end of mainPanel
                        )) #end of sidePanel, W4
    )  # end of navbarPage
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
