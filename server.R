library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ## After preprocessing data, concatenate them into one dataframe
    
    cityCountry <- read.csv("country_city.csv")

    # getting countries and cities available
    countries <- cityCountry$country
    countries <- unique(countries)
    cities <- cityCountry
    
    
    pasteCountryToCity <- function(city, country) {
        paste0(toupper(str_sub(country, 1, 1)), str_sub(country, 2, -1), ': ',toupper(str_sub(city, 1, 1)), str_sub(city, 2, -1))
    }
    cities <- mutate(cities, cityText = pasteCountryToCity(city, country))

    # Display cities available
    observe({
        citiesInput <- input$cities
        if(is.null(citiesInput)){
            # Can also set the label and select items
            updateCheckboxGroupInput(session, "cities",
                                     choices = setNames(cities$city, cities$cityText)
            )
        }
    })

    # We focus on data between min_date and max_date
    min_date <- '2019-07-01'
    max_date <- '2019-08-31'
    
    files_paths <- c()
    for(country in countries){
        
        cityCountryWanted <- cityCountry %>%
            filter(country == country)
        
        cityCountryWanted <- cityCountryWanted$city
        cityCountryWanted <- unique(cityCountryWanted)
        
        for(city in cityCountryWanted){
            file_dir <- file.path(".", "data_cleansed", country, city)
            file_subdirs <- list.dirs(file_dir)
            file_subdirs <- file_subdirs[-1]
            
            for(file_subdir in file_subdirs){
                if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
                    file_subdirs = file_subdirs[file_subdirs != file_subdir]
            }
            files_paths <- c(files_paths, file_subdirs)
        }
    }
    files_paths <- file.path(files_paths, "listings.csv")
    listings <- 
        do.call(rbind,
                lapply(files_paths, read.csv, row.names=1))
    
    ## Preprocess
    listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
    
    cities_selected <- reactive({
        if(length(input$cities) > 1){
            return(TRUE)
        } else {
            return(FALSE)
        }
    })
    
    hasAggregation <- reactive({
        if(input$statValue == "nostat" || input$metricBy == "nostat"){
            return(FALSE)
        } else {
            return(TRUE)
        }
    })
    
    data <- reactive({
        cities <- input$cities
        daterange <- input$daterange
        startdate <- ymd(daterange[1])
        enddate <- ymd(daterange[2])
        
        daterange <- interval(startdate, enddate)
        listings <- listings %>%
            filter(city %in% cities) %>%
            filter(ymd(data_date) %within% daterange)
        listings
    })
    
    plotLogic <- reactive({
        
        metric <- input$metric
        statValue <- input$statValue
        metricBy <- input$metricBy
        plotType <- input$plotType
        isGrouped <- input$isGrouped
        binwidth <- input$binwidth
        
        # basic plot without aggregation
        if(!hasAggregation()){
            # place the data inside the plot
            p <- ggplot(data(), aes_string(metric))
        }
        # If it has an aggregation
        else {
            aggreg_data <- data() %>%
                group_by(!!metricBy := get(metricBy), city)
            
            p <- switch (statValue,
                 "nostat" = return(NULL),
                 
                 "total" = {
                     total_data <- aggreg_data %>%
                         summarise(total = sum(get(metric), na.rm = TRUE), .groups = 'drop')
                     
                     plot <- ggplot(total_data, aes(y = total, x=get(metricBy)))
                 },
                 "average" = {
                     avg_data = aggreg_data %>%
                         summarise(average = mean(get(metric), na.rm = TRUE), .groups = 'drop')
                     
                     plot <- ggplot(avg_data, aes(y = average, x=get(metricBy)))
                 },
                 "median" = {
                     median_data = aggreg_data %>%
                         summarise(median = median(get(metric), na.rm = TRUE), .groups = 'drop')
                     
                     plot <- ggplot(median_data, aes(y = median, x=get(metricBy)))
                 })
        }
        
        # determining if we plot with a facet for each city or not
        if(!isGrouped && cities_selected()){
            p <- p + facet_wrap(.~city, ncol=1)
        }
        
        # adding the plot layer, depending if we want to plot an aggregation or not
        if(!hasAggregation()){
            
            #scaling if necessary
            if(metric == "revenue_30"){
                p <- p + scale_x_continuous(limits = quantile(data()$revenue_30, c(0.1, 0.9), na.rm = TRUE))
            }
            #scaling if necessary
            if(metric == "price_30"){
                p <- p + scale_x_continuous(limits = quantile(data()$price_30, c(0.1, 0.9), na.rm = TRUE))
            }
            
            p <- p + switch(plotType,
                "boxplot" = {
                    geom_boxplot(aes(fill = city), outlier.shape = NA)
                },
                
                "histogram" = {
                    geom_histogram(binwidth = binwidth, aes(fill = city))
                })
        } else {
            p <- p + switch(plotType,
                "boxplot" = {
                    return(NULL)
                },
                
                "histogram" = {
                    geom_histogram(stat='identity', aes_string(fill=metricBy))
                })
        }
        # add the labels
        p <- p + switch(metric,
            "no_metric" = return(NULL),
            "availability_30" = xlab("Availability over the last 30 days"),
            "revenue_30" = xlab("Revenue over the last 30 days"),
            "price_30" = xlab("Price"))
        
        # Adding the final title
        metricTitle <- switch (metric,
            "availability_30" = "Availability over the last 30 days",
            "revenue_30" = "Revenue over the last 30 days",
            "price_30" = "Price")
        
        p <- p + ggtitle(label = paste(metricTitle))

        p
    })
    
    plot1 <- reactive({
        if(cities_selected()){
            plotLogic()
        } else {
            return(NULL)
        }
    })
    plot2 <- reactive({
        if(cities_selected() || is.null(input$cities)){
            return(NULL)
        } else {
            plotLogic()
        }
    })
    
    dataToPlot <- reactive({
        metric <- input$metric
        top <- input$top

        dt <- data() %>%
                arrange(desc((!!as.name(metric)))) %>%
                head(top)
    }) 
    
    map <- reactive({
        if(cities_selected() || is.null(input$cities)){
            return(NULL)
        } else {
            metric = input$metric
            if(metric == "no_metric"){
                return(NULL)
            }
            else{
                dt = dataToPlot()
                leaflet() %>%
                    addProviderTiles(providers$Stamen.TonerLite,
                                     options = providerTileOptions(noWrap = TRUE)
                    ) %>%
                    addMarkers(lng = dt$longitude, lat = dt$latitude,
                               label = paste0("long : ", dt$longitude, " - ",
                                             "lat : ", dt$latitude, " - ",
                                             metric, " : ", dt[[metric]])) 
            }
        }
    })

    output$plot1 <-renderPlot(plot1())
    output$plot2 <-renderPlot(plot2())
    output$map <- renderLeaflet(map())

})
