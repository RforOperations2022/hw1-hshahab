#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(DT)
library(stringr)
library(tools)
library(dplyr)

arrdata <- read.csv(file = 'NYPD_Arrest_Data__Year_to_Date_.csv')
b64 <- base64enc::dataURI(file="Crime-2.jpeg", mime="Crime-2.jpeg")

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinythemes::shinytheme("united"),
    # Application title
    titlePanel("New York City Arrest Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select variable for y-axis ----------------------------------
            img(src = b64, height = 140, width = 270),
            
            # Select variable for x-axis ----------------------------------
            selectInput(inputId = "x", label = "X-axis", 
                         choices = c("Arrests_by_borough" = "ARREST_BORO", "Arrests_by_category" = "LAW_CAT_CD", "Arrests_by_sex" = "PERP_SEX", 
                                     "Arrest_Date" = "ARREST_DATE", "Age_Group" = "AGE_GROUP", "Arrests_by_race" = "PERP_RACE")),
           
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            # Horizontal line for visual separation -----------------------
            hr(), 
            
             # Set alpha level ---------------------------------------------
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Horizontal line for visual separation -----------------------
            hr(), 
            
              # Set point size ----------------------------------------------
            sliderInput(inputId = "size", 
                        label = "Size:", 
                        min = 0, max = 5, 
                        value = 2),
            
            # Horizontal line for visual separation -------------------------------
            hr(),   
            
            # Enter text for plot title ---------------------------------------------
            textInput(inputId = "plot_title", 
                      label = "Plot Title", 
                      placeholder = "Add Plot Title"),
            
            # Horizontal line for visual separation -----------------------
            hr(), 
            
            
            # Select which types of borough to plot ------------------------
            selectizeInput(inputId = "selected_type",
                           label = "Select borough(s):",
                           multiple = TRUE,
                           choices = c("Bronx(B)" = "B", "Queens(Q)" = "Q", "Staten Island(S)"= "S",
                                       "Manhattan(M)" = "M", "Brooklyn(K)" = "K"),
                           selected = c("B")),
       
            
            # Select sample size ----------------------------------------------------
            numericInput(inputId = "n_sample", 
                         label = "Sample size:", 
                         min = 1, max = nrow(arrdata), 
                         value = 100)
             ),
        
        # Output --------------------------------------------------------
        mainPanel(
            
            # Show barplot --------------------------------------------
            plotOutput(outputId = "barplot"),
            br(),
            
            # Output: Line graph --------------------------------------------
            #plotOutput(outputId = "linegraph"),
            #br(),
            
            # Output: Histogram -------------------------------------------
           plotOutput(outputId = "histogramplot"),
            #br(),
            
            # Print number of obs plotted ---------------------------------
            uiOutput(outputId = "n"),
            br(), br(), 
            
             # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "arrtable"),
            
            #Adding a Download Button
            downloadButton("downloadData", "Download Arrest Data")
        )
    )
)



# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
    
    # Create a subset of data filtering for selected title types ------
    borough_subset <- reactive({
        req(input$selected_type) 
        filter(arrdata, ARREST_BORO %in% input$selected_type)
    })
    
    # Update the maximum allowed n_samp for selected type movies ------
    observe({
        updateNumericInput(session, 
                           inputId = "n_sample",
                           value = min(100, nrow(nborough_subset())),
                           max = nrow(neighborhood_subset())
        )
    })
    
    
    # Create new df that is n_sample obs from selected type neighborhood ------
    borough_sample <- reactive({ 
        req(input$n_sample)
        sample_n(neighborhood_subset(), input$n_sample)
    })
    
    
    # Convert plot_title toTitleCase ----------------------------------
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    # Create barplot object the plotOutput function is expecting --
    output$barplot <- renderPlot({
        ggplot(data = arrdata, aes_string(x = input$x)) +
            geom_bar(stat = 'count', show.legend = TRUE) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")))
        })
        
    # Create histogram object
    output$histplot <- renderPlot({
        ggplot(data = borough_sample(), aes(x = ARREST_DATE, color = PERP_RACE)) +
            geom_histogram(stat='bin', position = 'identity', fill = "lightblue", linetype="dashed")  +
            labs(x = "Borough",
                 y = "Frequency",
                 color = toTitleCase(str_replace_all(input$z, "_", " ")),
                 title = pretty_plot_title()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    })
    
    # Print number of cases plotted ----------------------------------
    output$n <- renderUI({
        types <- borough_subset()$ARREST_BORO %>% 
            factor(levels = input$selected_type) 
        counts <- table(types)
    })
    
     # Print data table if checked -------------------------------------
    output$arrtable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = borough_sample[, 1:7], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )

# Reactive value for selected dataset ----
datasetInput <- reactive({
    switch(input$dataset,
           "ARREST DATA" = borough_sample())
})
output$downloadData <- downloadHandler(
    filename = function() {
        paste(input$dataset,".csv", sep = "")
    },
    content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
    }
)
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
