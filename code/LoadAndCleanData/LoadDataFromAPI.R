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

### concatenate the api urls for each year
urls <- c(
  "https://data.cms.gov/data-api/v1/dataset/337617e5-c18b-4cb9-8152-34851747584b/data?filter[Rndrng_Prvdr_Geo_Cd]=06",
  "https://data.cms.gov/data-api/v1/dataset/25b5b6c9-977d-4f7a-aaa7-7dde72f3d5cc/data?filter[Rndrng_Prvdr_Geo_Cd]=06",
  "https://data.cms.gov/data-api/v1/dataset/7c638c5f-7e8c-4346-8a25-9b37a0ed36dd/data?filter[Rndrng_Prvdr_Geo_Cd]=06"
)

### concatenate the years

years <- c("2019", "2018", "2017")

### Create a function that GETs the data from the url and parses it into a dataframe
get_content <- function(url, year) {
  request(url) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as_tibble() %>%
    clean_names() %>%
    ### add a column indicating what year the data is from
    mutate(year = year)
}

### map over our concatenated urls to pull all the data and combine it into one single data frame
ipps_data <- map2_dfr(urls, years, get_content)

##### --
#####

##### Finally we write our data to our raw data folder and clear our work space
#####

### write the dataframe to our raw data file as a csv
write_csv(ipps_data, file = "data/raw/ipps_data.csv")

### clear our workspace
remove(ipps_data, urls, years, get_content)

##### --
#####
