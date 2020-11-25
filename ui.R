library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Data Analytics project : an Airbnb Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("cities", "Choose one or more cities :",
            ),
            
            h3("Display data"),
            
            selectInput("feature", "Metric :",
                        c("-" = "no_feature",
                          "Availability for the next 30 days" = "availability_30",
                          "Revenue for the next 30 days" = "revenue_30",
                          "Price for the next 30 days" = "price_30")),
            
            selectInput("aggregType", "Statistic value:",
                        c("-" = "no_aggreg",
                          "Total" = "total",
                          "Average" = "average",
                          "Median" = "median")),
            
            selectInput("aggregOn", "Metric by:",
                        c("-" = "no_aggreg",
                          "Room Type" = "room_type",
                          "Number of bedrooms" = "bedrooms")),
            
            dateRangeInput("daterange", "Period:",
                           start = "2019-07-01",
                           end   = "2019-08-31",
                           min    = "2019-07-01",
                           max    = "2019-08-31"),
            
            checkboxInput("isGrouped", "Group data in one graph", TRUE),
            
            h3("Plot options"),
            selectInput("plotType", "Plot Type :",
                        c("Boxplot" = "boxplot",
                          "Histogram" = "histogram",
                          "Barplot" = "barplot")),
            
            sliderInput("binwidth", "Bin width :",
                        min = 1, max = 200, value = 50
            ),
            
            sliderInput("top", "Top data points (for map) :",
                        min = 1, max = 500, value = 50
            ),
            
        ),

        # Two different tabs : 1-comparing two cities plots, 2-Deep dive city plots
        mainPanel(
            navbarPage(title="Graphics",
                tabPanel("Comparing cities", plotOutput("plot1")),
                tabPanel("Deep dive", 
                         plotOutput("plot2"),
                         leafletOutput("map"))
            )
        )
    )
))
