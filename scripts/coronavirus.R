#########################################################################################################
# load the necessary packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(ggrepel, tidyverse)
p_load_gh("RamiKrispin/coronavirus")
options("tidylog.display" = NULL)

#########################################################################################################
# Load the data
data("coronavirus")

# Export the data
write_csv(coronavirus, "data/coronavirus_data.csv")

#########################################################################################################
# Create a running count of confimred cases, deaths, and recoveries by country-day.
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

#########################################################################################################
# Cases: Make time trend of each country v italy on separate plot
cases_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy"), aes(x = days_since, y = total_to_date_confirmed)) +
    annotate(geom='line', x=italy$days_since,y=italy$total_to_date_confirmed, linetype = "dashed") +
    geom_line(alpha = 1, size = 2, color = "black") +
    geom_line(alpha = 1, size = 1.5, color = "orange") +
    theme_classic() +
    facet_wrap(vars(Country.Region)) +
    ggtitle("The US, Western Europe, and Iran are on similar coronavirus case trajectories as Italy (dashed-line)") +
    ylab("Total confirmed cases (certainly an undercount)") +
    xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_cases_compared_to_italy.png", plot = cases_v_italy, height = 6, width = 9, dpi = 600)


# Deaths: Make time trend of each country v italy on separate plot
deaths_v_italy <- ggplot(data = subset(analysis, Country.Region != "Italy"), aes(x = days_since, y = total_to_date_deaths)) +
  annotate(geom='line', x=italy$days_since,y=italy$total_to_date_deaths, linetype = "dashed") +
  geom_line(alpha = 1, size = 2, color = "black") +
  geom_line(alpha = 1, size = 1.5, color = "red") +
  theme_classic() +
  facet_wrap(vars(Country.Region)) +
  ggtitle("The trajectory for deaths is not as clear. Deaths counts are still low in US. Italy (dashed-line)") +
  ylab("Total confirmed deaths (certainly an undercount)") +
  xlab("Days since 100 confirmed cases")

ggsave(file = "output/coronavirus_deaths_compared_to_italy.png", plot = deaths_v_italy, height = 6, width = 9, dpi = 600)

#########################################################################################################
# Cases Make time trend of each country v USA on one plot
cases_all <- ggplot(data = analysis, aes(x = days_since, y = total_to_date_confirmed, group = Country.Region, label = Country.Region)) +
  geom_line(data = subset(analysis, Country.Region != "US"), alpha = .33, size = 3.5, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 4, color = "black") +
  geom_line(data = subset(analysis, Country.Region == "US"), alpha = 1, size = 3.5, color = "orange") +
  theme_classic() +
  ggtitle("The United States (in orange) looks more like Europe or Iran than Asia") +
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
  ggtitle("Death count: The United States (in red) v other countries") +
  ylab("Total deaths (certainly an undercount)") +
  xlab("Days since 100 confirmed cases") +
  geom_label_repel(data = subset(analysis, max_days == days_since))

ggsave(file = "output/coronavirus_deaths_all_on_one_plot.png", plot = deaths_all, height = 6, width = 9, dpi = 600)