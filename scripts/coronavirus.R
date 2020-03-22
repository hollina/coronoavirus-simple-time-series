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
# Load the data
# Note: thix code is directly copied from the "RamiKrispin/coronavirus" package
#----------------------------------------------------
# Pulling the coronvirus data from John Hopkins repo
# https://github.com/CSSEGISandData/COVID-19
#----------------------------------------------------
# Setting functions
`%>%` <- magrittr::`%>%`
#----------------------------------------------------
# Pulling confirmed cases

raw_conf <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                     stringsAsFactors = FALSE)

lapply(1:ncol(raw_conf), function(i){
  if(all(is.na(raw_conf[, i]))){
    raw_conf <<- raw_conf[, -i]
    return(print(paste("Column", names(raw_conf)[i], "is missing", sep = " ")))
  } else {
    return(NULL)
  }
})
# Fixing US data
# Aggregating county level to state level

raw_us_conf <- raw_conf %>%
  dplyr::filter(Country.Region == "US") %>%
  dplyr::mutate(state = ifelse(!grepl(",", Province.State),
                               Province.State,
                               trimws(substr(Province.State,
                                             regexpr(",", Province.State) + 1,
                                             regexpr(",", Province.State) + 3)))) %>%
  dplyr::left_join(data.frame(state = state.abb,
                              state_name = state.name,
                              stringsAsFactors = FALSE),
                   by = "state") %>%
  dplyr::mutate(state_name = ifelse(is.na(state_name), state, state_name)) %>%
  dplyr::mutate(state_name = ifelse(state_name == "D.", "Washington, D.C.", state_name)) %>%
  dplyr::mutate(Province.State = state_name) %>%
  dplyr::select(-state, -state_name)

raw_us_map <- raw_us_conf %>%
  dplyr::select("Province.State","Country.Region", "Lat", "Long") %>%
  dplyr::distinct() %>%
  dplyr::mutate(dup = duplicated(Province.State)) %>%
  dplyr::filter(dup == FALSE) %>%
  dplyr::select(-dup)

us_agg_conf <- aggregate(x = raw_us_conf[, 5:(ncol(raw_us_conf))], by = list(raw_us_conf$Province.State), FUN = sum) %>%
  dplyr::select(Province.State = Group.1, dplyr::everything())

us_fix_conf <- raw_us_map %>% dplyr::left_join(us_agg_conf, by = "Province.State")


raw_conf1 <- raw_conf %>%
  dplyr::filter(Country.Region != "US") %>%
  dplyr::bind_rows(us_fix_conf)



# Transforming the data from wide to long
# Creating new data frame
df_conf <- raw_conf1[, 1:4]

for(i in 5:ncol(raw_conf1)){
  
  raw_conf1[,i] <- as.integer(raw_conf1[,i])
  # raw_conf[,i] <- ifelse(is.na(raw_conf[, i]), 0 , raw_conf[, i])
  print(names(raw_conf1)[i])
  
  if(i == 5){
    df_conf[[names(raw_conf1)[i]]] <- raw_conf1[, i]
  } else {
    df_conf[[names(raw_conf1)[i]]] <- raw_conf1[, i] - raw_conf1[, i - 1]
  }
  
  
}


df_conf1 <-  df_conf %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                             names_to = "date_temp",
                                             values_to = "cases_temp")

# Parsing the date
df_conf1$month <- sub("X", "",
                      strsplit(df_conf1$date_temp, split = "\\.") %>%
                        purrr::map_chr(~.x[1]) )

df_conf1$day <- strsplit(df_conf1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_conf1$date <- as.Date(paste("2020", df_conf1$month, df_conf1$day, sep = "-"))

# Aggregate the data to daily
df_conf2 <- df_conf1 %>%
  dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "confirmed",
                Country.Region = trimws(Country.Region),
                Province.State = trimws(Province.State))

head(df_conf2)
tail(df_conf2)
#----------------------------------------------------
# Pulling death cases

raw_death <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                      stringsAsFactors = FALSE,
                      fill =FALSE)

lapply(1:ncol(raw_death), function(i){
  if(all(is.na(raw_death[, i]))){
    raw_death <<- raw_death[, -i]
    return(print(paste("Column", names(raw_death)[i], "is missing", sep = " ")))
  } else {
    return(NULL)
  }
})
# Fixing US data
# Aggregating county level to state level

raw_us_death <- raw_death %>%
  dplyr::filter(Country.Region == "US") %>%
  dplyr::mutate(state = ifelse(!grepl(",", Province.State),
                               Province.State,
                               trimws(substr(Province.State,
                                             regexpr(",", Province.State) + 1,
                                             regexpr(",", Province.State) + 3)))) %>%
  dplyr::left_join(data.frame(state = state.abb,
                              state_name = state.name,
                              stringsAsFactors = FALSE),
                   by = "state") %>%
  dplyr::mutate(state_name = ifelse(is.na(state_name), state, state_name)) %>%
  dplyr::mutate(state_name = ifelse(state_name == "D.", "Washington, D.C.", state_name)) %>%
  dplyr::mutate(Province.State = state_name) %>%
  dplyr::select(-state, -state_name)

# raw_us_map <- raw_us_death %>%
#   dplyr::select("Province.State","Country.Region", "Lat", "Long") %>%
#   dplyr::distinct() %>%
#   dplyr::mutate(dup = duplicated(Province.State)) %>%
#   dplyr::filter(dup == FALSE) %>%
#   dplyr::select(-dup)

us_agg_death <- aggregate(x = raw_us_death[, 5:(ncol(raw_us_death))], by = list(raw_us_death$Province.State), FUN = sum) %>%
  dplyr::select(Province.State = Group.1, dplyr::everything())

us_fix_death <- raw_us_map %>% dplyr::left_join(us_agg_death, by = "Province.State")


raw_death1 <- raw_death %>%
  dplyr::filter(Country.Region != "US") %>%
  dplyr::bind_rows(us_fix_death)





# Transforming the data from wide to long
# Creating new data frame
df_death <- raw_death1[, 1:4]

for(i in 5:ncol(raw_death1)){
  print(i)
  raw_death1[,i] <- as.integer(raw_death1[,i])
  raw_death1[,i] <- ifelse(is.na(raw_death1[, i]), 0 , raw_death1[, i])
  
  if(i == 5){
    df_death[[names(raw_death1)[i]]] <- raw_death1[, i]
  } else {
    df_death[[names(raw_death1)[i]]] <- raw_death1[, i] - raw_death1[, i - 1]
  }
}


df_death1 <-  df_death %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                               names_to = "date_temp",
                                               values_to = "cases_temp")

# Parsing the date
df_death1$month <- sub("X", "",
                       strsplit(df_death1$date_temp, split = "\\.") %>%
                         purrr::map_chr(~.x[1]) )

df_death1$day <- strsplit(df_death1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_death1$date <- as.Date(paste("2020", df_death1$month, df_death1$day, sep = "-"))

# Aggregate the data to daily
df_death2 <- df_death1 %>%
  dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "death",
                Country.Region = trimws(Country.Region),
                Province.State = trimws(Province.State))

head(df_death2)
tail(df_death2)
#----------------------------------------------------
# Pulling recovered cases

raw_rec <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
                    stringsAsFactors = FALSE,
                    fill =FALSE)

lapply(1:ncol(raw_rec), function(i){
  if(all(is.na(raw_rec[, i]))){
    raw_rec <<- raw_rec[, -i]
    return(print(paste("Column", names(raw_rec)[i], "is missing", sep = " ")))
  } else {
    return(NULL)
  }
})
# Fixing US data
# Aggregating county level to state level

raw_us_rec <- raw_rec %>%
  dplyr::filter(Country.Region == "US") %>%
  dplyr::mutate(state = ifelse(!grepl(",", Province.State),
                               Province.State,
                               trimws(substr(Province.State,
                                             regexpr(",", Province.State) + 1,
                                             regexpr(",", Province.State) + 3)))) %>%
  dplyr::left_join(data.frame(state = state.abb,
                              state_name = state.name,
                              stringsAsFactors = FALSE),
                   by = "state") %>%
  dplyr::mutate(state_name = ifelse(is.na(state_name), state, state_name)) %>%
  dplyr::mutate(state_name = ifelse(state_name == "D.", "Washington, D.C.", state_name)) %>%
  dplyr::mutate(Province.State = state_name) %>%
  dplyr::select(-state, -state_name)

raw_us_map <- raw_us_rec %>%
  dplyr::select("Province.State","Country.Region", "Lat", "Long") %>%
  dplyr::distinct() %>%
  dplyr::mutate(dup = duplicated(Province.State)) %>%
  dplyr::filter(dup == FALSE) %>%
  dplyr::select(-dup)

us_agg_rec <- aggregate(x = raw_us_rec[, 5:(ncol(raw_us_rec))], by = list(raw_us_rec$Province.State), FUN = sum) %>%
  dplyr::select(Province.State = Group.1, dplyr::everything())

us_fix_rec <- raw_us_map %>% dplyr::left_join(us_agg_rec, by = "Province.State")


raw_rec1 <- raw_rec %>%
  dplyr::filter(Country.Region != "US") %>%
  dplyr::bind_rows(us_fix_rec)




# Transforming the data from wide to long
# Creating new data frame
df_rec <- raw_rec1[, 1:4]

for(i in 5:ncol(raw_rec1)){
  print(i)
  raw_rec1[,i] <- as.integer(raw_rec1[,i])
  raw_rec1[,i] <- ifelse(is.na(raw_rec1[, i]), 0 , raw_rec1[, i])
  
  if(i == 5){
    df_rec[[names(raw_rec1)[i]]] <- raw_rec1[, i]
  } else {
    df_rec[[names(raw_rec1)[i]]] <- raw_rec1[, i] - raw_rec1[, i - 1]
  }
}


df_rec1 <-  df_rec %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                           names_to = "date_temp",
                                           values_to = "cases_temp")

# Parsing the date
df_rec1$month <- sub("X", "",
                     strsplit(df_rec1$date_temp, split = "\\.") %>%
                       purrr::map_chr(~.x[1]) )

df_rec1$day <- strsplit(df_rec1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_rec1$date <- as.Date(paste("2020", df_rec1$month, df_rec1$day, sep = "-"))

# Aggregate the data to daily
df_rec2 <- df_rec1 %>%
  dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "recovered",
                Country.Region = trimws(Country.Region),
                Province.State = trimws(Province.State))

head(df_rec2)
tail(df_rec2)
#---------------- Aggregate all cases ----------------

#
coronavirus <- dplyr::bind_rows(df_conf2, df_death2, df_rec2) %>%
  as.data.frame()



head(coronavirus)
tail(coronavirus)

#########################################################################################################
# Create a running count of confimred cases, deaths, and recoveries by country-day.

us_states <- coronavirus %>% 
  filter(Country.Region == "US") %>%
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

#########################################################################################################
analysis_us <- coronavirus %>% 
  group_by(Country.Region, date, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) 

analysis_us <- analysis_us %>%
  group_by(Country.Region) %>%
  mutate(total_to_date_confirmed = cumsum(confirmed)) %>%
  mutate(total_to_date_deaths = cumsum(death)) %>%
  mutate(total_to_date_recoveries= cumsum(recovered)) %>%
  select(-c(confirmed, death, recovered)) %>%
  filter(total_to_date_confirmed >= 10) 

# Drop China and Cruise ship. Add days since 100
analysis_us <- analysis_us %>%
  filter(Country.Region != "China") %>%
  filter(Country.Region != "Cruise Ship") %>%
  group_by(Country.Region) %>%
  mutate(threshold_date = min(date)) %>%
  mutate(days_since = date - threshold_date) %>%
  mutate(max_days = max(days_since)) %>%
  filter(max_days >= 10) %>% 
  arrange(desc(Country.Region))

# Make a separate italy dataset (it will be reference line)
italy_us <- analysis_us %>%
  filter(Country.Region == "Italy") 

south_korea_us <- analysis_us %>%
  filter(Country.Region == "Korea, South") 
#########################################################################################################
analysis <- coronavirus %>% 
  group_by(Country.Region, date, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) 

analysis <- analysis %>%
  group_by(Country.Region) %>%
  mutate(total_to_date_confirmed = cumsum(confirmed)) %>%
  mutate(total_to_date_deaths = cumsum(death)) %>%
  mutate(total_to_date_recoveries= cumsum(recovered)) %>%
  select(-c(confirmed, death, recovered)) %>%
  filter(total_to_date_confirmed >= 100) 

# Drop China and Cruise ship. Add days since 100
analysis <- analysis %>%
  filter(Country.Region != "China") %>%
  filter(Country.Region != "Cruise Ship") %>%
  group_by(Country.Region) %>%
  mutate(threshold_date = min(date)) %>%
  mutate(days_since = date - threshold_date) %>%
  mutate(max_days = max(days_since)) %>%
  filter(max_days >= 10) %>% 
  arrange(desc(Country.Region))

# Make a separate italy dataset (it will be reference line)
italy <- analysis %>%
  filter(Country.Region == "Italy") 

south_korea <- analysis %>%
  filter(Country.Region == "Korea, South") 

#########################################################################################################
us_state_case_plot <- ggplot(us_states, aes(x = days_since, y = log(total_to_date_confirmed + 1))) +
  annotate(geom='line', x=italy_us$days_since,y=log(italy_us$total_to_date_confirmed + 1), linetype = "dashed") +
  geom_line(alpha = 1, size = 2, color = "black") +
  geom_line(alpha = 1, size = 1.5, color = "orange") +
  coord_flip() +
  theme_bw() +
  facet_geo(~ Province.State, grid = "us_state_grid2") +
  ggtitle(paste0("Natural log of confirmed coronavirus cases in each US state compared to Italy (dashed-line)\n \nMost recent date: ", max(coronavirus$date))) +
  labs( 
    x = "Natural log of confirmed coronavirus cases", 
    y = "Days since 10 confirmed cases")


ggsave(file = "output/coronavirus_ln_cases_by_state.png", plot = us_state_case_plot, height = 6, width = 9, dpi = 600)

#########################################################################################################
# Cases: Make time trend of each country v italy on separate plot

cases_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy" & Country.Region != "Korea, South"), aes(x = days_since, y = total_to_date_confirmed)) +
  annotate(geom='line', x=italy$days_since,y=italy$total_to_date_confirmed, linetype = "dashed") +
  annotate(geom='line', x=south_korea$days_since,y=south_korea$total_to_date_confirmed, linetype = "solid") +
  geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "orange") +
    theme_classic() +
    facet_wrap(vars(Country.Region)) +
    ggtitle(paste0("Coronavirus case trajectories\n Italy (dashed-line); South-Korea (solid-line) \n
            Most recent date: ", max(coronavirus$date))) +
    ylab("Total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_cases_compared_to_italy_and_south_korea.png", plot = cases_v_italy, height = 6, width = 9, dpi = 600)

cases_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy" & Country.Region != "Korea, South"), aes(x = days_since, y = log(total_to_date_confirmed))) +
  annotate(geom='line', x = italy$days_since,y = log(italy$total_to_date_confirmed), linetype = "dashed") +
  annotate(geom='line', x = south_korea$days_since,y = log(south_korea$total_to_date_confirmed), linetype = "solid") +
  geom_line(alpha = 1, size = 2, color = "black") +
  geom_line(alpha = 1, size = 1.5, color = "orange") +
  theme_classic() +
  facet_wrap(vars(Country.Region)) +
  ggtitle(paste0("Natural log of coronavirus case trajectories\n Italy (dashed-line); South-Korea (solid-line) \n
            Most recent date: ", max(coronavirus$date))) +
  ylab("Total confirmed cases (certainly an undercount)") +
  xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_ln_cases_compared_to_italy_and_south_korea.png", plot = cases_v_italy, height = 6, width = 9, dpi = 600)



# Deaths: Make time trend of each country v italy on separate plot
deaths_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy" & Country.Region != "Korea, South"), aes(x = days_since, y = total_to_date_deaths)) +
  annotate(geom='line', x=italy$days_since,y=italy$total_to_date_deaths, linetype = "dashed") +
  annotate(geom='line', x=south_korea$days_since,y=south_korea$total_to_date_deaths, linetype = "solid") +
  geom_line(alpha = 1, size = 2, color = "black") +
  geom_line(alpha = 1, size = 1.5, color = "red") +
  theme_classic() +
  facet_wrap(vars(Country.Region)) +
  ggtitle(paste0("Corona virus death count. \n
  Italy (dashed-line); South-Korea (solid-line) \n
  Most recent date: ", max(coronavirus$date))) +
  ylab("Total confirmed deaths (certainly an undercount)") +
  xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_deaths_compared_to_italy_and_south_korea.png", plot = deaths_v_italy, height = 6, width = 9, dpi = 600)

deaths_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy" & Country.Region != "Korea, South"), aes(x = days_since, y = log(total_to_date_deaths))) +
  annotate(geom='line', x=italy$days_since,y = log(italy$total_to_date_deaths), linetype = "dashed") +
  annotate(geom='line', x=south_korea$days_since,y = log(south_korea$total_to_date_deaths), linetype = "solid") +
  geom_line(alpha = 1, size = 2, color = "black") +
  geom_line(alpha = 1, size = 1.5, color = "red") +
  theme_classic() +
  facet_wrap(vars(Country.Region)) +
  ggtitle(paste0("Natural log of coronavirus death count. \n
  Italy (dashed-line); South-Korea (solid-line) \n
  Most recent date: ", max(coronavirus$date))) +
  ylab("Total confirmed deaths (certainly an undercount)") +
  xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_ln_deaths_compared_to_italy_and_south_korea.png", plot = deaths_v_italy, height = 6, width = 9, dpi = 600)

#########################################################################################################
# Cases Make time trend of each country v USA on one plot
cases_all <- ggplot(data = analysis, aes(x = days_since, y = total_to_date_confirmed, group = Country.Region, label = Country.Region)) +
  geom_line(data = subset(analysis, Country.Region != "US"), alpha = .33, size = 3.5, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 4, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 3.5, color = "orange") +
  theme_classic() +
  ggtitle(paste0("The United States (in orange) looks more like Europe or Iran than Asia\n
    Most recent date: ", max(coronavirus$date))) +
  ylab("Total confirmed cases (certainly an undercount)") +
  xlab("Days since 100 confirmed cases") +
  geom_label_repel(data = subset(analysis, max_days == days_since))

ggsave(file = "output/coronavisrus_cases_all_on_one_plot.png", plot = cases_all, height = 6, width = 9, dpi = 600)


# Deaths: Make time trend of each country v USA on one plot
deaths_all <- ggplot(data = analysis, aes(x = days_since, y = total_to_date_deaths, group = Country.Region, label = Country.Region)) +
  geom_line(data = subset(analysis, Country.Region != "US"), alpha = .33, size = 3.5, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 4, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 3.5, color = "red") +
  theme_classic() +
  ggtitle(paste0("Death count: The United States (in red) v other countries\n
    Most recent date: ", max(coronavirus$date))) +
  ylab("Total deaths (certainly an undercount)") +
  xlab("Days since 100 confirmed cases") +
  geom_label_repel(data = subset(analysis, max_days == days_since))

ggsave(file = "output/coronavirus_deaths_all_on_one_plot.png", plot = deaths_all, height = 6, width = 9, dpi = 600)