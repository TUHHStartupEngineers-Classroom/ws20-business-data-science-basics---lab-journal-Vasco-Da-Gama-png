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
library(vroom)
library(data.table)
library(mapdata)
library(scales)

# Load Data
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# ---- First challenge ----

# Manipulate so we get just the data we want ----
covid_data_req <- covid_data_tbl %>%
  filter(countriesAndTerritories == "Germany" | countriesAndTerritories == "United_Kingdom" | countriesAndTerritories == "France" |
           countriesAndTerritories == "Spain"|  countriesAndTerritories == "United_States_of_America") %>%
  transmute(Country = countriesAndTerritories, date = dmy(dateRep), cases) %>%
  arrange(Country, date) %>%
  pivot_wider(names_from = "Country",
              values_from = "cases") %>%
  transmute(date, France = cumsum(France), Germany = cumsum(Germany), Spain = cumsum(Spain), United_Kingdom = cumsum(United_Kingdom),
            United_States_of_America = cumsum(United_States_of_America)) %>%
  pivot_longer(cols = c(France, Germany, Spain, United_Kingdom, United_States_of_America), names_to = "Country", values_to = "CumCases") %>%
  arrange(Country, date)

# Plotting the data ----
covid_data_req %>%
  ggplot(aes(x = date, y = CumCases, fill = Country, color = Country)) +
  scale_color_manual(values=c("#69b3a2", "purple", "black", "blue", "red", "orange"))+
  theme_grey() +
  geom_line() + 
  labs(
    title = "Confirmed cumulative Covid-19 cases",
    x = "Year 2020",
    y = "Cumulative cases",
    color = "Country")+
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Second challenge ----

world <- map_data("world")

covid_data_world_tbl <- covid_data_tbl %>% 
  transmute(Country = countriesAndTerritories, date = dmy(dateRep), deaths, Population = popData2019) %>%
  mutate(across(Country, str_replace_all, "_", " ")) %>%
  mutate(Country = case_when(
    Country == "United Kingdom" ~ "UK",
    Country == "United States of America" ~ "USA",
    Country == "Czechia" ~ "Czech Republic",
    TRUE ~ Country)) %>%
  group_by(Country) %>%
  arrange(date) %>%
  transmute(Country, date, totalDeaths = cumsum(deaths), Population) %>%
  ungroup() %>%
  transmute(Country, date, mortality_rate = totalDeaths/Population)%>%
  filter(date == dmy("05/12/2020")) %>%
  transmute(Country, mortality_rate) 

#Joining the data
covidd_data_world_joined_tbl <- world %>%
  left_join(covid_data_world_tbl, by = c("region" = "Country")) %>%
  transmute(Country = region, long, lat, mortality_rate)

# Plot the map
covidd_data_world_joined_tbl %>%
  ggplot() +
  geom_map(aes(map_id = Country, x = long, y =lat, fill = mortality_rate), map = world, color = "black" ) +
  scale_fill_continuous(breaks=breaks_width(0.00025),low ="#ea4440", high="#2f142c", labels = scales::label_percent()) +
  labs(
    title = "COVID-19 mortality rate",
    x = "",
    y = "",
    fill = "Mortality rate",
    caption ="Date: 12/05/2020") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()) 
  
