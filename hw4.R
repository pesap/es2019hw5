library(tidyverse)
library(readxl)


# Read dataset
loc <- read_excel("ca_ozone/location.xls")

# Rename column to lower case and remove spacing
names(loc) %<>%  # Interesting
  str_replace_all("\\s","_") %>% 
  tolower


# Question 9
# Count the number of sites that start with San or Santa
sites_with_san <- loc %>%
  filter(stringr::str_detect(site_name, '^(San|Santa)') )
sites_with_san

# Print number of rows for places that start with San or Santa
nrow(sites_with_san)



# Question 10
# Check for correct address.
mask1 <-str_detect(loc$address, "\\d{1,5}\\s\\w.\\s(\\b\\w*\\b\\s){1,2}\\w*\\.")
mask2 <-str_detect(loc$zip_code, "\\d{5}")
good_address <- loc %>%
  filter(mask1 & mask2)
good_address

# Question 14
my_summary <- function(data, filter){
  data %>% 
  filter(stringr::str_detect(site_name, filter) ) %>%
  group_by(year=year(date)) %>%
  summarize(mean = mean(o3, na.rm = TRUE), median = median(o3, na.rm=TRUE), max=max(o3, na.rm=TRUE), min =min(o3, na.rm=TRUE))
}
my_summary(daily.site, '^(San|Santa)')


# Question 15
daily_mean_f1 <- function(data, county) {
  data %>%
    filter(stringr::str_detect(county_name, "Merced")) %>%
    group_by(date) %>%
    summarize(daily_mean=mean(o3, na.rm=TRUE)) %>%
    group_by(year=year(date)) %>%
    summarize(annual_mean=mean(daily_mean, na.rm=TRUE))
}
annual_daily_mean <- daily_mean_f1(daily.site)
annual_daily_mean

