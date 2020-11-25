 # library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
  
  dataURLs <- read.csv(file="data_urls.csv", stringsAsFactors=FALSE)
  
  selectedCountries <- c("italy", "germany", "spain")
  
  min_date <- as.Date('2019-07-01')
  max_date <- as.Date('2019-08-31')
  
  dataURLs <- dataURLs %>%
    filter(country %in% selectedCountries) %>%
    filter(as.Date(data_date) > min_date & as.Date(data_date) < max_date) %>%
    group_by(country) %>%
    top_n(4)
#Creating a dataframe to associate cities to their country
cityCountry <- read.csv(text="country,city")

# Iterate throught datasets to download all datasets
for(i in 1:nrow(dataURLs)) {
  url <- dataURLs[i,]
  print(paste0("Preprocessing : ", i, " on ", nrow(dataURLs)))
  listingsURL <- as.character(url[1, "listings_url"])
  calendarURL <- as.character(url[1, "calendar_url"])
  country <- as.character(url[1, "country"])
  city <- as.character(url[1, "city"])
  data_date <- as.character(url[1, "data_date"])
  
  newrow <- data.frame(country, city)
  names(newrow) <- c("country", "city")
  cityCountry <- rbind(cityCountry, newrow)
  
  if(file.exists(file.path("data_cleansed", country, city, data_date, "listings.csv"))) {
    print(paste0(file.path("data_cleansed", country, city, data_date, "listings.csv"), " already exists"))
  }
  else{
    # Download data sets
    listings <- fread(listingsURL)
    calendar <- fread(calendarURL)
  
    ## Add Keys: columns country, city and day date
    listings$country <- country
    listings$city <- city
    listings$data_date <- data_date
    
    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("country","city", "data_date", "id", "neighbourhood_cleansed", 
                          "latitude", "longitude", 
                          "property_type", "room_type", "accommodates", "bedrooms", 
                          "beds", "price", "minimum_nights",  "maximum_nights")
    
    listings <- listings %>% 
      select(all_of(columns_listings)) %>% 
      arrange(id)
    
    
    # Cleaning calendar dataframe
    
    ## arrange by id and date
    calendar <- calendar %>% 
      arrange(listing_id, date)
    
    ## add day number (starting first day)
    calendar <- calendar %>%
      group_by(listing_id) %>%
      mutate(day_nb = row_number()) %>%
      ungroup()
    
    ## change available column to binary
    calendar <- calendar %>%
      mutate(available = ifelse(available=="t", 1, 0))
    
    ## clean price column and transform to numeric
    calendar <- calendar %>%
      mutate(price = str_replace(price, "\\$", ""))
    calendar <- calendar %>%
      mutate(price = str_replace(price, ",", ""))
    calendar <- calendar %>%
      mutate(price = as.numeric(price))
    
    ## calculate estimated revenue for upcoming day
    calendar <- calendar %>%
      mutate(revenue = price*(1-available))
    
    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
    calendar <- calendar %>%
      group_by(listing_id) %>%
      summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                .groups = 'drop'
      )
    if (class(listings$id) != "integer") {
      listings <- listings %>%
        mutate(
          id = as.numeric(id)
        )
    }
    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
    
    dir.create(file.path("data_cleansed", country, city, data_date), recursive = TRUE)
    
    print(paste0("saving data into ", file.path("data_cleansed", country, city, data_date, "listings.csv")))
    write.csv(listings_cleansed, file.path("data_cleansed", country, city, data_date, "listings.csv"))
  }
}

cityCountry <- cityCountry %>%
  select(city, country) %>%
  group_by(country) %>%
  distinct(city) %>%
  arrange(country)

write.csv(cityCountry, "country_city.csv")
print("--- Preprocess finished ---")