#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

load("data_Project.RData")

# Define UI for application that draws a histogram


ui <- fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("County",
                           "County:",
                           c("All",
                             unique(as.character(final_data$County))))
        ),
        column(4,
               selectInput("State",
                           "State:",
                           c("All",
                             unique(as.character(final_data$State))))
        ),
        column(4,
               selectInput("cluster_3",
                           "cluster_3:",
                           c("All",
                             unique(as.character(final_data$cluster_3))))
        ),
        column(4,
               selectInput("cluster_5",
                           "cluster_5:",
                           c("All",
                             unique(as.character(final_data$cluster_5))))
        )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Table", DT::dataTableOutput("table")),
                    tabPanel("Plot_Number", plotOutput("plot1", height = 400,click = "plot1_click",brush = brushOpts(id = "plot1_brush")),
                             column(width = 12,
                                    h4("Point Info."),
                                    verbatimTextOutput("click_info")
                             ),
                    ),
                    tabPanel("Plot_Rate", plotOutput("plot2", height = 400,click = "plot2_click",brush = brushOpts(id = "plot2_brush")),
                             column(width = 12,
                                    h4("Point Info."),
                                    verbatimTextOutput("click_info_2")
                             ),
                    )
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- final_data
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$cluster_3 != "All") {
            data <- data[data$cluster_3 == input$cluster_3,]
        }
        if (input$cluster_5 != "All") {
            data <- data[data$cluster_5 == input$cluster_5,]
        }

        data
        
    }))
    
    output$plot1 <- renderPlot({
        data <- final_data
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$cluster_3 != "All") {
            data <- data[data$cluster_3 == input$cluster_3,]
        }
        if (input$cluster_5 != "All") {
            data <- data[data$cluster_5 == input$cluster_5,]
        }
            # ggplot(data=data, aes(Deaths, Positive_Cases)) + geom_point() + xlab("Deaths") + ylab("Positive Cases") + theme(axis.title = element_text(size=20), axis.text = element_text(size=15)) + xlim(0,26000)
        plot(Positive_Cases~Deaths, data = data)
    })
    
    output$plot2 <- renderPlot({
        data <- final_data
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$cluster_3 != "All") {
            data <- data[data$cluster_3 == input$cluster_3,]
        }
        if (input$cluster_5 != "All") {
            data <- data[data$cluster_5 == input$cluster_5,]
        }
        # ggplot(data=data, aes(Deaths, Positive_Cases)) + geom_point() + xlab("Deaths") + ylab("Positive Cases") + theme(axis.title = element_text(size=20), axis.text = element_text(size=15)) + xlim(0,26000)
        plot(Positive_Case_Rate~Death_Rate, data = data)
    })
    
    output$click_info <- renderPrint({
        data <- final_data
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$cluster_3 != "All") {
            data <- data[data$cluster_3 == input$cluster_3,]
        }
        if (input$cluster_5 != "All") {
            data <- data[data$cluster_5 == input$cluster_5,]
        }
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(data, input$plot1_click, xvar = "Deaths", yvar = "Positive_Cases", addDist = TRUE)
    })
    
    output$click_info_2 <- renderPrint({
        data <- final_data
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$cluster_3 != "All") {
            data <- data[data$cluster_3 == input$cluster_3,]
        }
        if (input$cluster_5 != "All") {
            data <- data[data$cluster_5 == input$cluster_5,]
        }
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(data, input$plot2_click, xvar = "Death_Rate", yvar = "Positive_Case_Rate", addDist = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
