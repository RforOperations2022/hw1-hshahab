#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
arrdata <- read.csv(file = '/Users/hajrashahab/Documents/GitHub/hw1-hshahab/hshahab_NYCData/NYPD_Arrest_Data__Year_to_Date_.csv')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Arrest Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select variable for y-axis ----------------------------------
            selectInput(inputId = "y", label = "Y-axis:", 
                        choices = c("jurisdiction_code"), selected = "arrest_key"),
           
            # Select variable for x-axis ----------------------------------
            selectInput(inputId = "x", label = "X-axis:", 
                         choices = c("jurisdiction_code"), selected = "arrest_key"),
            
            # Select variable for color -----------------------------------
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("jurisdiction_code"),
                        selected = "arrest_key"),
            # Set alpha level ---------------------------------------------
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE)
        ),
        
        # Output --------------------------------------------------------
        mainPanel(
            
            # Show scatterplot --------------------------------------------
            plotOutput(outputId = "scatterplot"),
            
            # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "arrtable")
        )
    )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
    
    # Create scatterplot object the plotOutput function is expecting --
    output$scatterplot <- renderPlot({
        ggplot(data = arrdata, aes_string(x = input$x, y = input$y,
                                         color = input$z)) +
            geom_point(alpha = input$alpha) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")))
    })
    
    # Print data table if checked -------------------------------------
    output$arrtable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = arrdata[, 1:7], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
