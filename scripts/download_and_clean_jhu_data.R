#########################################################################################################
# load the necessary packages
if (!require(pacman)) {
    install.packages("pacman")
    library(pacman)
}

p_load(ggrepel, tidyverse, devtools, lubridate, geofacet)

# Options for log
options("tidylog.display" = NULL)

#########################################################################################################
# Set up list of dates
list_of_dates <- format(
    seq(as.Date("01-22-2020", format="%m-%d-%Y"), 
        as.Date(format(Sys.Date(), "%m-%d-%Y"), format="%m-%d-%Y"), 
        by="days"), 
    '%m-%d-%Y')

#########################################################################################################
# Function to download any old data if not already downloaded. 

cached_download <- function(url, cache_dir = NULL, update = FALSE) {
    
    if (is.null(cache_dir)) {
        print("Error: Set directory.")
    } else if (dir.exists(cache_dir) && update == FALSE) {
        fpath <- file.path(cache_dir, basename(url))

        if (!file.exists(fpath)) {
            utils::download.file(url, fpath)
        }
        
        fpath
    } else if (dir.exists(cache_dir) && update == TRUE) {
        fpath <- file.path(cache_dir, basename(url))
      
        utils::download.file(url, fpath)
    
        fpath
    } else if (url == sprintf(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
        format(Sys.Date(), "%m-%d-%Y"), format="%m-%d-%Y")) {
        fpath <- file.path(cache_dir, basename(url))
        
        utils::download.file(url, fpath)
        
        fpath
    }
    
}

#########################################################################################################
# Download all data not previously downloaded
for (i in seq(1, length(list_of_dates), by = 1)) {
    
    temp_url <- test_url <- sprintf(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
        list_of_dates[i])
    
    cached_download(url = temp_url, cache_dir= "temp/", update = FALSE)
}

#########################################################################################################
# Update today
utils::download.file(sprintf(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
    list_of_dates[length(list_of_dates)]), file.path("temp/", basename(sprintf(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
        list_of_dates[length(list_of_dates)]))))

#########################################################################################################
# Read in data and append
temp_url <- test_url <- sprintf(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
  list_of_dates[1])
fpath <- file.path("temp/", basename(temp_url))

covid19_full_before_format_change <- read_csv(fpath) 

##############################################################
# Combine these columns
# https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- as.numeric(NA)
  
  y[, c(as.character(x.diff))] <- as.numeric(NA)
  
  return(rbind(x, y))
}


for (i in seq(2, 60, by = 1)) {
    temp_url <- test_url <- sprintf(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", 
        list_of_dates[i])
    fpath <- file.path("temp/", basename(temp_url))

    temp_data <- read_csv(fpath) 
    
    covid19_full_before_format_change <- rbind.all.columns(covid19_full_before_format_change, temp_data)
}
    

us_states <- covid19_full_before_format_change %>% 
  filter(`Country/Region` == "US") 

%>%
  pivot_wider(names_from = type, values_from = cases) 

us_states <- us_states %>%
  group_by(Province.State) %>%
  mutate(total_to_date_confirmed = cumsum(confirmed)) %>%
  mutate(total_to_date_deaths = cumsum(death)) %>%
  mutate(total_to_date_recoveries= cumsum(recovered)) %>%
  select(-c(confirmed, death, recovered)) %>%
  filter(total_to_date_confirmed >= 10) 

us_states <- us_states %>%
  group_by(Province.State) %>%
  mutate(threshold_date = min(date)) %>%
  mutate(days_since = date - threshold_date) %>%
  mutate(max_days = max(days_since)) %>%
  arrange(desc(Province.State))
