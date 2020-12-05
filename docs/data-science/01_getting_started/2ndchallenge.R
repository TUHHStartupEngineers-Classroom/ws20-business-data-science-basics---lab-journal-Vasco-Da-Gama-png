library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(furrr)     # Parallel Processing using purrr (iteration)
library(lubridate)
library(RSQLite)
library(httr)
library(purrr)

link1 <- "https://api.covid19api.com/summary"
resp <- link1 %>%
  GET()

covid_global <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>%
  purrr::pluck("Countries") %>%
  as_tibble()

cases_by_country_tbl <- covid_global %>%
  transmute(Country, CountryCode, NewConfirmed,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered, Date = date(Date)) %>%
  arrange(desc(TotalConfirmed))

link2 <- "https://api.covid19api.com/dayone/country/germany/status/confirmed"
resp <- link2 %>%
  GET()

covid_germany_conf_tbl <- resp %>% 
  .$content %>% 
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  transmute(Date = date(Date), Cases)

cases_by_country_tbl
covid_germany_conf_tbl

plot(covid_germany_conf_tbl$Date,covid_germany_conf_tbl$Cases,
     main="Development of Corona in Germany",
     xlab = "Time",
     ylab="Sum of Confirmed Infected",
     type="l",
     col="blue")

# second exercise ----
url_home <- "https://www.rosebikes.de/"
url_mtb <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"

# Read in the HTML for the entire webpage
html_mtb <- read_html(url_mtb)

mtb_category_urls <- html_mtb %>%
  html_nodes(css = ".catalog-category-bikes__button") %>%
  html_attr("href") %>%
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(url = glue("https://www.rosebikes.de{subdirectory}")) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)


getBikeData <- function(model_category_url) {
  # Read in the html
  html_model_category <- read_html(model_category_url)
  
  # Get the model names
  model_names <- html_model_category %>%
    html_nodes(css = ".catalog-category-model__title") %>%
    html_text() %>%
    # Remove the query parameters of the URL
    str_replace_all(pattern = "\\n","") %>%
    # Convert vector to tibble
    enframe(name ="position", value ="model")
  
  #Get the model prices
  model_prices <- html_model_category %>%
    html_nodes(css = ".catalog-category-model__price-current-value") %>%
    html_text() %>%
    # Remove the query parameters of the URL
    str_replace_all(pattern = "\\n","") %>%
    str_replace_all(pattern = "€","") %>%
    str_replace_all(pattern = ",00","") %>%
    str_replace_all(pattern = "\\.","") %>%
    str_replace_all(pattern = "\\s","") %>%
    # Convert to numeric
    as.numeric() %>%
    # Convert vector to tibble
    enframe(name ="position", value = "price")
  
  bikeData <- left_join(model_names, model_prices, by= "position") %>%
    select(model, price)
}  

# Extract the urls as a character vector
mtb_category_urls_vec <- mtb_category_urls %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(mtb_category_urls_vec, getBikeData)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)

bike_data_tbl
