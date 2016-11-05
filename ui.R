library(shiny)

# Define UI for application that plots random distributions 
fluidPage(
        
        # Application title
        headerPanel("Daily Units Sold Predictor"),
        
        # Sidebar with a slider input for number of observations
        sidebarPanel(
                dateRangeInput(
                        "dates",
                        label = "Select Date Range to be used for analysis.",
                        start = as.POSIXct("2012-10-01 CEST"), 
                        end = as.POSIXct("2016-10-31 CEST"),
                        separator = "to", format = "yyyy/mm/dd"
                ),
                selectizeInput(
                        'month', 'Select Predicted Month', 
                        choices = c("October",
                                    "November",
                                    "December"),
                        options = list(
                                placeholder = 'Select Reporting Period',
                                onInitialize = I('function() { this.setValue(""); }')
                        )
                        ),
                h2("Usage Instructions"),
                p("Using Unit Sold information from Amazon we are building a 
                prediction on what the expected sales are in the Oct/Nov/Dec period."),
                p("The data available is from 2012 until end Oct 2016.")
                
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
                plotOutput("distPlot", width = "600px", height = "300px"),
                plotOutput("predPlot", width = "600px", height = "300px"),
                #plotOutput("3dPlot"),
                dataTableOutput("summary")
        )
)