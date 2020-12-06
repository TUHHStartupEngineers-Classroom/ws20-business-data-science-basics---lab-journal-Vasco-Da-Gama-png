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

col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
  )

# Load the data ----
assignee_tbl <- vroom(
  file       = "data-science/00_data/patentsview/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "data-science/00_data/patentsview/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "data-science/00_data/patentsview/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "data-science/00_data/patentsview/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# Converting to data table

setDT(assignee_tbl)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)

# Data joining ----

patent_joined_tbl <- patent_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "patent_id")) %>%
  left_join(assignee_tbl, by = c("assignee_id" = "id")) %>%
  left_join(uspc_tbl, by = c("id" = "patent_id")) #%>%
  # arrange(assignee_id)

# Data wrangling ----

# ---- First Question ----

mostUSPatents <- patent_joined_tbl[type.y == 2, .N, by = organization][order(-N)][1:10]
# Save RDS
saveRDS(mostUSPatents, "data-science/02_data_wrangling/mostUSPatents.rds")

# ---- Second Question ----

mostUSPatent2019 <- patent_joined_tbl[lubridate::year(date) == 2019 & type.y == 2, .N, by = organization][order(-N)][1:10]
# Save RDS
saveRDS(mostUSPatent2019, "data-science/02_data_wrangling/mostUSPatent2019.rds")

# ---- Third Question ----
# Top ten companies worldwide most patents
mostPatents <- patent_joined_tbl[, .N, by = organization][order(-N)][2:11] %>%
  left_join(patent_joined_tbl, by = c("organization" = "organization"))

mostPatents_mainclasses <- mostPatents[, .N, by = mainclass_id][order(-N)][2:6]
# Save RDS
saveRDS(mostPatents_mainclasses, "data-science/02_data_wrangling/mostPatents_mainclasses.rds")