#############################################################################################################################
#############################################################################################################################
##                                                                                                                         ##
##                              This file loads the IPPS data from the API provided by                                     ##
##                                                     Data.CMS.gov                                                        ##
##                                                                                                                         ##
##                                                                                                                         ##
#############################################################################################################################


##### Load the necessary packages
#####

library(tidyverse)
library(jsonlite)
library(janitor)
library(httr2)

##### --
#####

##### GET the data from the API for the years 2019, 2018 and 2017 and limited to California records
#####

### concatenate the api urls for each year                                             ↓ add a filter for CA
urls <- c(
  "https://data.cms.gov/data-api/v1/dataset/6f6d93e1-ecf8-4b93-9845-091faf20f274/data?filter[Rndrng_Prvdr_State_FIPS]=06",
  "https://data.cms.gov/data-api/v1/dataset/09c12f06-e3fe-4cb0-81e9-945f2078c1df/data?filter[Rndrng_Prvdr_State_FIPS]=06",
  "https://data.cms.gov/data-api/v1/dataset/b61ba5eb-021b-4510-947e-0f198982b0e8/data?filter[Rndrng_Prvdr_State_FIPS]=06"
)

### concatenate the years

years <- c("2019", "2018", "2017")

### Create a function that GETs the data from the url and parses it into a dataframe
get_content <- function(url, year) {
  request(url) %>%
    ### the requests seem to only pull 1000 records, this adds an iterative request to *repeatedly* pull
    ### 1000 records at a time
    req_perform_iterative(next_req = iterate_with_offset("offset", start = 1, offset = 1000)) %>%
    map_dfr(resp_body_json, simplifyVector = TRUE) %>%
    as_tibble() %>%
    clean_names() %>%
    ### add a column indicating what year the data is from
    mutate(year = year)
}

### map over the concatenated urls to pull all the data and combine it into one single data frame
ipps_data <- map2_dfr(urls, years, get_content)

##### --
#####

##### Write the data to the raw data folder and clear the work space
#####

### write the dataframe to the raw data file as a csv
write_csv(ipps_data, file = "data/raw/ipps_data.csv")

### Include a text file with the date current date to know when the data was last pulled from the API
write_file(paste0("Data last pulled from API on ", Sys.Date()), 
           file = paste0("data/raw/last_pull_data_", Sys.Date(), ".txt"))

### clear the workspace
remove(ipps_data, urls, years, get_content)

##### --
#####
