library(shiny)
library(readr)
library(lubridate)
library(dplyr)
source("daily_analyzer.R")


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
        
        pr_oct <- data.frame(date = 20161001:20161031)
        pr_nov <- data.frame(date = 20161101:20161130)
        pr_dec <- data.frame(date = 20161201:20161231)
        
        daily_final <- read_csv('daily_final.csv')
        datasetInput <- reactive({
                daily_final <- daily_final[
                        daily_final$date >= format(input$dates[1], "%Y%m%d") &
                        daily_final$date <= format(input$dates[2], "%Y%m%d"), ]
        })
        monthInput <- reactive({
                validate(
                        need(input$month != "", "Select a prediction period!")
                )
                switch(input$month,
                        "October" = pr_oct,
                        "November" = pr_nov,
                        "December" = pr_dec
                )
        })
        
        output$distPlot <- renderPlot({
               with(analyze(datasetInput(), monthInput())$analyzed,
                    boxplot(percentage ~ day,
                            xlim = c(1, 31), ylim = c(0, 0.1), col = 'gold',
                            main = "Daily Units Sold (2012-2016)",
                            xlab = "Day", ylab = "Percentage of Month"))
        })
        
        output$summary <- renderDataTable({
                m <- analyze(datasetInput(), monthInput())
                data.frame(date = m$rebuild$cdate,
                           weekday = m$rebuild$weekday,
                           diwali_adjustment = m$rebuild$deewali,
                           prediction = (m$prediction/sum(m$prediction)) )
        }, options = list(lengthMenu = c(5, 10, 31), pageLength = 5))
        
        output$predPlot <- renderPlot({
                out <- analyze(datasetInput(), monthInput())
                plot(out$prediction,
                     xlim = c(1, 31), ylim = c(0, 0.1), col = 4,
                     main = "Daily Units Sold (projection)",
                     xlab = "Day", ylab = "Percentage of Month")
                if (input$month == "October") {
                        with(out$analyzed[out$analyzed$date > "20160931", ], 
                             points(percentage ~ day,
                               xlim = c(1, 31), ylim = c(0, 0.1),
                               pch = 19, col = 1))
                        legend("topleft", pch = c(1, 19), col = c(4, 1),
                               legend = c("Predicted Sold Units", "Actual Sold Units"))
                }
        })

})