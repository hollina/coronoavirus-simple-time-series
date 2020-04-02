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
fpath <- file.path('temp/', basename("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

# Download NYT data
utils::download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", fpath)

# Read NYT data into R
covid19_us_states <- read_csv(fpath) 


us_states <- covid19_us_states %>%
    filter(cases >= 10) 

us_states <- us_states %>%
    group_by(state) %>%
    mutate(threshold_date = min(date)) %>%
    mutate(days_since = date - threshold_date) %>%
    mutate(max_days = max(days_since)) %>%
    arrange(desc(state))

ny <- us_states %>%
    filter(state == "New York")

#########################################################################################################
us_state_case_plot <- ggplot(us_states, aes(x = days_since, y = log(cases))) +
    annotate(geom='line', x = ny$days_since,y = log(ny$cases), linetype = "dashed") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "orange") +
    coord_flip() +
    theme_bw() +
    facet_geo(~ state, grid = "us_state_grid2") +
    ggtitle(paste0("Natural log of confirmed coronavirus cases in each US state compared to New York (dashed-line) \n \nMost recent date: ", max(us_states$date))) +
    labs( 
        x = "Natural log of confirmed coronavirus cases", 
        y = "Days since 10 confirmed cases")


ggsave(file = "output/coronavirus_ln_cases_by_state.png", plot = us_state_case_plot, height = 6, width = 9, dpi = 600)


us_state_death_plot <- ggplot(us_states, aes(x = days_since, y = log(deaths))) +
    annotate(geom='line', x = ny$days_since,y = log(ny$deaths), linetype = "dashed") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "red") +
    coord_flip() +
    theme_bw() +
    facet_geo(~ state, grid = "us_state_grid2") +
    ggtitle(paste0("Natural log of coronavirus deaths in each US state compared to New York (dashed-line)\n \nMost recent date: ", max(us_states$date))) +
    labs( 
        x = "Natural log of coronavirus deaths", 
        y = "Days since 10 confirmed cases")


ggsave(file = "output/coronavirus_ln_deaths_by_state.png", plot = us_state_death_plot, height = 6, width = 9, dpi = 600)
