library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("About",
               tabPanel("Widget 1",
                        sidebarLayout(
                            sidebarPanel(
                                "Dropdown for coral cover/year"
                            ), # end of sidebarPanel
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
               tabPanel("Thing 3")
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
