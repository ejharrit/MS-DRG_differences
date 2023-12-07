#############################################################################################################################
#############################################################################################################################
##                                                                                                                         ##
##                              This file loads the data we pulled from the Data.CMS.gov                                   ##
##                                  API, cleans it and saves the new data format                                           ##
##                                                                                                                         ##
##                                                                                                                         ##
#############################################################################################################################


##### Load the necessary packages
#####

library(tidyverse)
library(stringr)
library(skimr)

##### --
#####


##### Load the data saved from the LoadDataFromApi.R file
#####

ipps_data <- read_csv("data/raw/ipps_data.csv")

##### --
#####


##### Check the general structure of the data to see if there are any structural issues,
##### such as missing values or anything like that, that need to be fixed
#####

skim(ipps_data)

str(ipps_data)

### Data was meant to be pulled only for California. This checks that only California data was pulled
nrow(ipps_data) == ipps_data %>%
  filter(rndrng_prvdr_state_abrvtn == "CA") %>%
  nrow()

##### --
#####


##### Rename and select variables and create a new variable indicating which cities are included in the Inland Empire
##### as well as indicate whether there were complications
#####

### This uses a list of cities from the Municipal Management Association of Southern California to determine which cities
### are part of the Inland Empire, this list can be accessed at https://mmasc.org/163/Inland-Empire-Region
inland_empire <- c(
  "Adelanto",       "Apple Valley", "Banning",        "Barstow",          "Beaumont",
  "Big Bear Lake",  "Calimesa",     "Canyon Lake",    "Chino",            "Chino Hills",
  "Colton",         "Corona",       "Eastvale",       "Fontana",          "Grand Terrace",
  "Hemet",          "Hesperia",     "Highland",       "Jurupa Valley",    "Lake Elsinore",
  "Loma Linda",     "Menifee",      "Montclair",      "Moreno Valley",    "Murrieta",
  "Norco",          "Ontario",      "Perris",         "Rancho Cucamonga", "Redlands",
  "Rialto",         "Riverside",    "San Bernardino", "San Jacinto",      "Temecula",
  "Upland",         "Victorville",  "Wildomar",       "Yucaipa"
)

ipps_data <- ipps_data %>%
  rename(
    ### New Name  ### Original Name
    prvdr_cd    = rndrng_prvdr_ccn,
    prvdr_name  = rndrng_prvdr_org_name,
    city        = rndrng_prvdr_city,
    zip         = rndrng_prvdr_zip5
  ) %>%
  select(c(prvdr_cd:city, zip, drg_cd:year)) %>%
  mutate(
    inland_empire = if_else(
      city %in% inland_empire,
      1,
      0
    ),
    complication = case_when(
      str_detect(drg_desc, "WITH CC")   ~ "complication",
      str_detect(drg_desc, "WITH MCC")  ~ "major complication",
      TRUE                              ~ "none detected"
    )
  ) %>%
  ### Reorder the columns so that year is first and the inland_empire column is near the other
  ### geography fields and the complication column is near the drg_desc column
  select(year, prvdr_cd:zip, inland_empire, drg_cd:drg_desc, complication, everything())

##### --
#####

##### Write the data to the clean data folder and clear the work space
#####

### write the dataframe to the clean data file as a csv
write_csv(ipps_data, file = "data/cleaned/ipps_data.csv")

### clear the workspace
remove(ipps_data, inland_empire)

##### --
#####
