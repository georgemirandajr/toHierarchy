shinyUI(navbarPage("PACE App",
    navbarMenu("Recorder",
        tabPanel("Indexing"),
    
    fluidPage(
    
    sidebarLayout(
        
        sidebarPanel(
            
            textInput("emp", "Employee Number:",
                      value = "E01"),
            
            h1(textOutput("total")),
            "indexed this many documents in a year",
            h1(textOutput("peak")),
            "peak documents in a day",
            h1(textOutput("avg")),
            "total average documents per hour"
            
        ),
        
        mainPanel(
            
            tabsetPanel(
                tabPanel("Plot", plotOutput("trend")),
                tabPanel("Table", tableOutput("trendtable")),
            
            downloadButton("downloadCSV", "Download CSV")
            )
        )
    )
)
),
tabPanel("Elections")
)
)