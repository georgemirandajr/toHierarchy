library(data.table)
library(ggplot2)
library(knitr)
indexing <- fread("./data/indexing.csv", sep = "auto", header = "auto", stringsAsFactors = TRUE)
indexing$date <- as.Date(indexing$date, format = "%m/%d/%Y")
indexing$mo <- strftime(indexing$date, "%m")
indexing$yr <- strftime(indexing$date, "%Y")
monthlyavg <- aggregate(cbind(ratePH, namesPD, docs, minutes) ~ mo + yr + emp, data = indexing, FUN = mean)
monthlyavg$date <- as.POSIXct(paste(monthlyavg$yr, monthlyavg$mo, "01", sep = "-"))

shinyServer(function(input, output) {
    
    names <- reactive(subset(indexing, emp == input$emp))
    
    empavg <- reactive(subset(monthlyavg, emp == input$emp))
    
    output$trendtable <- renderTable({ 
                            names()})
    
    output$emp <- renderText(input$emp)
    
    output$trend <- renderPlot({
        
        validate(
            need(length(names()$emp) > 3, "Enter an employee number using this format: 'E01'"
            )
        )
        
        g <- ggplot(empavg(), aes(x = date, y = ratePH))
        
        g + geom_point(aes(color = namesPD, size = docs)) + 
            
            scale_color_gradient(low = "blue", high = "red") +
            
            labs(title = "Monthly Average Rate Per Hour", x = "", y = "", color = "Names Per Doc",
                 size = "Docs") + theme_bw()

    })
    
    output$total <- renderText({
        sum(names()$docs)
        
    })
    
    output$peak <- renderText({
        names()$docs[which.max(names()$docs)]
    })
    
    output$avg <- renderText({
        
        if (length(names()$emp) < 3) {
            
            NULL
            
        } else {
            
            round(mean(names()$ratePH), digits = 2)
                
    }
    
    })

    output$downloadCSV <-
        downloadHandler(filename = "report.csv",
                        content = function(con) {
                            assign(input$emp, empavg())
                            save(list = input$emp, file = con)
                            },
                        
        )

})