#############################################################################################################################
#############################################################################################################################
##                                                                                                                         ##
##                              This file this file performs an exploratory analysis on the                                ##
##                                  data we cleaned in the CleanRawDatAndSave.R file                                       ##
##                                                                                                                         ##
##                                                                                                                         ##
#############################################################################################################################


##### Load the necessary packages
#####

library(matrixStats)
library(tidyverse)

##### --
#####

#############################################################################################################################
##                                                                                                                         ##
##                                         custom functions                                                                ##
##                                                                                                                         ##
#############################################################################################################################

summarize_payments <- function(.data, ...){
  .data %>%
  summarize(
    across(
      c(...),
      \(x) weighted.mean(x, tot_dschrgs, na.rm = TRUE)
    ),
    tot_dschrgs = sum(tot_dschrgs, na.rm = TRUE)
  ) 
}
##### --
#####


##### Load the data saved from the CleanRawDatAndSave.R file
#####

ipps_data <- read_csv("data/cleaned/ipps_data.csv")

##### --
#####


##### Average's in the Inland Empire and California as a whole
#####

### Averages in Inland Empire
inland_empire_averages <- ipps_data %>%
  group_by(year) %>%
  filter(inland_empire == 1) %>% 
  summarize_payments(avg_submtd_cvrd_chrg:avg_mdcr_pymt_amt) %>%
  mutate(geography = "Inland Empire", .before = avg_submtd_cvrd_chrg)

### Averages in California
california_averages <- ipps_data %>%
  group_by(year) %>%
  summarize_payments(avg_submtd_cvrd_chrg:avg_mdcr_pymt_amt) %>%
  mutate(geography = "California", .before = avg_submtd_cvrd_chrg)

### Compare
bind_rows(
  inland_empire_averages,
  california_averages
)

##### --
#####


##### differences among payments made by non-Medicare entities
#####

### Create a new variable to track payments made by non-Medicare entities
ipps_data <- ipps_data %>%
  mutate(avg_non_mdcr_pymt_amt = round(avg_tot_pymt_amt - avg_mdcr_pymt_amt, 2))

### dot plot of non-Medicare payments by MS-DRG
ipps_data %>%
  group_by(drg_cd, year, inland_empire) %>%
  summarize_payments(avg_submtd_cvrd_chrg:avg_non_mdcr_pymt_amt) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(drg_cd, avg_non_mdcr_pymt_amt), y = avg_non_mdcr_pymt_amt)) +
  geom_point() +
  facet_grid(rows = vars(year), cols = vars(inland_empire))

### Table of this
ipps_data %>%
  group_by(drg_cd, drg_desc, year, inland_empire) %>%
  summarize_payments(avg_submtd_cvrd_chrg:avg_non_mdcr_pymt_amt) %>%
  ungroup() %>%
  select(drg_cd, drg_desc, year, inland_empire, avg_non_mdcr_pymt_amt) %>%
  group_by(year, inland_empire) %>%
  arrange(inland_empire, year, desc(avg_non_mdcr_pymt_amt)) %>%
  slice_head(n = 10) %>%
  print(n = 60)

### Checking the outlier in the Inland Empire
ipps_data %>%
  group_by(year, inland_empire) %>%
  filter(drg_cd == "253") %>%
  select(avg_submtd_cvrd_chrg:avg_non_mdcr_pymt_amt) %>%
  ungroup() %>%
  ggplot(aes(x = factor(inland_empire), y = avg_non_mdcr_pymt_amt)) +
  geom_violin() +
  geom_point() +
  facet_wrap(~year)

### Checking most common codes
top_codes <- ipps_data %>%
  group_by(drg_cd) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  distinct(drg_cd, total) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(drg_cd)

ipps_data %>%
  filter(drg_cd %in% top_codes) %>%
  ggplot(aes(x = factor(inland_empire), y = avg_non_mdcr_pymt_amt)) +
  geom_boxplot() +
  geom_point()  +
  facet_grid(rows = vars(year), cols = vars(drg_cd))
