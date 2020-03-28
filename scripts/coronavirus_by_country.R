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
# Create filepath in temp folder
fpath <- file.path('temp/', basename("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

# Download data
utils::download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", fpath)

# Read NYT data into R
covid19_cases_by_country <- read_csv(fpath) 

covid19_cases_by_country <- covid19_cases_by_country %>%    
    pivot_longer(names_to = "date", values_to = "cases", ends_with("20")) 


covid19_cases_by_country <- covid19_cases_by_country%>%
    group_by(`Country/Region`, date) %>%
    summarise(cases = sum(cases)) 

covid19_cases_by_country$date <- as.Date(covid19_cases_by_country$date, "%m/%d/%y")

covid19_cases_by_country <- covid19_cases_by_country %>%
    filter(cases >= 100) %>%
    mutate(threshold_date = min(date)) %>%
    mutate(days_since = date - threshold_date) %>%
    mutate(max_days = max(days_since)) %>%
    filter(max_days >= 10) 

# Make a separate italy dataset (it will be reference line)
italy <- covid19_cases_by_country %>%
    filter(`Country/Region` == "Italy") 

south_korea <- covid19_cases_by_country %>%
    filter(`Country/Region` == "Korea, South") 


#########################################################################################################
# Cases: Make time trend of each country v italy on separate plot

cases_v_italy <- ggplot(data = subset(covid19_cases_by_country, `Country/Region` != "Italy" & `Country/Region` != "Korea, South"), aes(x = days_since, y = cases)) +
    annotate(geom='line', x=italy$days_since,y=italy$cases, linetype = "dashed") +
    annotate(geom='line', x=south_korea$days_since,y=south_korea$cases, linetype = "solid") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "orange") +
    theme_classic() +
    facet_wrap(vars(`Country/Region`)) +
    ggtitle(paste0("Coronavirus case trajectories\n Italy (dashed-line); South-Korea (solid-line) \n
            Most recent date: ", max(covid19_cases_by_country$date))) +
    ylab("Total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_cases_compared_to_italy_and_south_korea.png", plot = cases_v_italy, height = 6, width = 9, dpi = 600)

ln_cases_v_italy <- ggplot(data = subset(covid19_cases_by_country, `Country/Region` != "Italy" & `Country/Region` != "Korea, South"), aes(x = days_since, y = log(cases))) +
    annotate(geom='line', x = italy$days_since,y = log(italy$cases), linetype = "dashed") +
    annotate(geom='line', x = south_korea$days_since,y = log(south_korea$cases), linetype = "solid") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "orange") +
    theme_classic() +
    facet_wrap(vars(`Country/Region`)) +
    ggtitle(paste0("Natural log of coronavirus case trajectories\n Italy (dashed-line); South-Korea (solid-line) \n
            Most recent date: ", max(covid19_cases_by_country$date))) +
    ylab("Total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_ln_cases_compared_to_italy_and_south_korea.png", plot = ln_cases_v_italy, height = 6, width = 9, dpi = 600)

cases_all <- ggplot(data = covid19_cases_by_country, aes(x = days_since, y = cases, group = `Country/Region`, label = `Country/Region`)) +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` != "US"), alpha = .33, size = 3.5, color = "black") +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` == "US"), alpha = 1, size = 4, color = "black") +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` == "US"), alpha = 1, size = 3.5, color = "orange") +
    theme_classic() +
    ggtitle(paste0("The United States (in orange) \n
    Most recent date: ", max(covid19_cases_by_country$date))) +
    ylab("Total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases") +
    geom_label_repel(data = subset(covid19_cases_by_country, max_days == days_since & cases > 25000 ))

ggsave(file = "output/coronavisrus_cases_all_on_one_plot.png", plot = cases_all, height = 6, width = 9, dpi = 600)

ln_cases_all <- ggplot(data = covid19_cases_by_country, aes(x = days_since, y = log(cases), group = `Country/Region`, label = `Country/Region`)) +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` != "US"), alpha = .33, size = 3.5, color = "black") +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` == "US"), alpha = 1, size = 4, color = "black") +
    geom_line(data = subset(covid19_cases_by_country, `Country/Region` == "US"), alpha = 1, size = 3.5, color = "orange") +
    theme_classic() +
    ggtitle(paste0("The United States (in orange) \n
    Most recent date: ", max(covid19_cases_by_country$date))) +
    ylab("Natural log of total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases") +
    geom_label_repel(data = subset(covid19_cases_by_country, max_days == days_since & log(cases) > 10 ))

ggsave(file = "output/coronavisrus_ln_cases_all_on_one_plot.png", plot = ln_cases_all, height = 6, width = 9, dpi = 600)

#########################################################################################################
# Create filepath in temp folder
fpath <- file.path('temp/', basename("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

# Download data
utils::download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", fpath)

# Read NYT data into R
covid19_deaths_by_country <- read_csv(fpath) 

covid19_deaths_by_country <- covid19_deaths_by_country %>%    
    pivot_longer(names_to = "date", values_to = "deaths", ends_with("20")) 


covid19_deaths_by_country <- covid19_deaths_by_country%>%
    group_by(`Country/Region`, date) %>%
    summarise(deaths = sum(deaths)) 

covid19_deaths_by_country$date <- as.Date(covid19_deaths_by_country$date, "%m/%d/%y")

covid19_deaths_by_country <- covid19_deaths_by_country %>%
    filter(deaths >= 100) %>%
    mutate(threshold_date = min(date)) %>%
    mutate(days_since = date - threshold_date) %>%
    mutate(max_days = max(days_since)) %>%
    filter(max_days >= 10) 

# Make a separate italy dataset (it will be reference line)
italy <- covid19_deaths_by_country %>%
    filter(`Country/Region` == "Italy") 

south_korea <- covid19_deaths_by_country %>%
    filter(`Country/Region` == "Korea, South") 


#########################################################################################################
# Deaths: Make time trend of each country v italy on separate plot

deaths_v_italy <- ggplot(data = subset(covid19_deaths_by_country, `Country/Region` != "Italy" & `Country/Region` != "Korea, South"), aes(x = days_since, y = deaths)) +
    annotate(geom='line', x=italy$days_since,y=italy$deaths, linetype = "dashed") +
    annotate(geom='line', x=south_korea$days_since,y=south_korea$deaths, linetype = "solid") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "red") +
    theme_classic() +
    facet_wrap(vars(`Country/Region`)) +
    ggtitle(paste0("Coronavirus death trajectories\n Italy (dashed-line) \n
            Most recent date: ", max(covid19_deaths_by_country$date))) +
    ylab("Total deaths (certainly an undercount)") +
    xlab("Days since 100 deaths")

ggsave(file = "output/coronavirus_deaths_compared_to_italy.png", plot = deaths_v_italy, height = 6, width = 9, dpi = 600)

ln_deaths_v_italy <- ggplot(data = subset(covid19_deaths_by_country, `Country/Region` != "Italy" & `Country/Region` != "Korea, South"), aes(x = days_since, y = log(deaths))) +
    annotate(geom='line', x = italy$days_since,y = log(italy$deaths), linetype = "dashed") +
    annotate(geom='line', x = south_korea$days_since,y = log(south_korea$deaths), linetype = "solid") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "red") +
    theme_classic() +
    facet_wrap(vars(`Country/Region`)) +
    ggtitle(paste0("Natural log of coronavirus case trajectories\n Italy (dashed-line)\n
            Most recent date: ", max(covid19_deaths_by_country$date))) +
    ylab("Total confirmed deaths (certainly an undercount)") +
    xlab("Days since 100 confirmed deaths")

ggsave(file = "output/coronavirus_ln_deaths_compared_to_italy.png", plot = ln_deaths_v_italy, height = 6, width = 9, dpi = 600)

deaths_all <- ggplot(data = covid19_deaths_by_country, aes(x = days_since, y = deaths, group = `Country/Region`, label = `Country/Region`)) +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` != "US"), alpha = .33, size = 3.5, color = "black") +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` == "US"), alpha = 1, size = 4, color = "black") +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` == "US"), alpha = 1, size = 3.5, color = "red") +
    theme_classic() +
    ggtitle(paste0("The United States (in red) \n
    Most recent date: ", max(covid19_deaths_by_country$date))) +
    ylab("Total confirmed deaths (certainly an undercount)") +
    xlab("Days since 100 confirmed deaths") +
    geom_label_repel(data = subset(covid19_deaths_by_country, max_days == days_since))

ggsave(file = "output/coronavisrus_deaths_all_on_one_plot.png", plot = deaths_all, height = 6, width = 9, dpi = 600)

ln_deaths_all <- ggplot(data = covid19_deaths_by_country, aes(x = days_since, y = log(deaths), group = `Country/Region`, label = `Country/Region`)) +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` != "US"), alpha = .33, size = 3.5, color = "black") +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` == "US"), alpha = 1, size = 4, color = "black") +
    geom_line(data = subset(covid19_deaths_by_country, `Country/Region` == "US"), alpha = 1, size = 3.5, color = "red") +
    theme_classic() +
    ggtitle(paste0("The United States (in red) \n
    Most recent date: ", max(covid19_deaths_by_country$date))) +
    ylab("Natural log of total confirmed deaths (certainly an undercount)") +
    xlab("Days since 100 confirmed deaths") +
    geom_label_repel(data = subset(covid19_deaths_by_country, max_days == days_since ))

ggsave(file = "output/coronavisrus_ln_deaths_all_on_one_plot.png", plot = ln_deaths_all, height = 6, width = 9, dpi = 600)





