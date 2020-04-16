rm(list = ls())
package.list <- list("ggplot2", "dplyr", "reshape2", "lubridate")
source('R/load_packages.R') # load packages


# read in the data
Italy_deaths = read.csv("~/Google Drive/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
                    dplyr::filter(Country.Region == 'Italy') %>%
                    dplyr::select(-Province.State, -Country.Region, -Lat, -Long) %>%
                    reshape2::melt(, variable.name = 'date', value.name = 'Italy_deaths') %>%
                    dplyr::mutate(date = sub(date, pattern = 'X', replacement = ''),
                                  date = as.Date(date, format = '%m.%d.%y'),
                                  Italy_death_new = Italy_deaths - lag(Italy_deaths),
                                  date_numeric = lubridate::yday(date))

# start date with 10 total deaths
start_date = Italy_deaths$date[min(which(Italy_deaths$Italy_deaths > 10))]

# let's only look after the date when 10 total deaths had occured
Italy_deaths = Italy_deaths %>% dplyr::filter(date > start_date)

# fit non-linear model
mod <- lm(log(Italy_death_new+1) ~ date_numeric, data = Italy_deaths)

# set span such that we fit on the nearest 14 points
span = 14/nrow(Italy_deaths)

# add fitted curve
Italy_deaths = Italy_deaths %>%dplyr::mutate(Italy_death_estimated = exp(predict(mod, list(date_numeric = date_numeric))))

pdf(file = 'figs/Italy_deaths_and_exponential_fit.pdf',
    height = 5,
    width = 8)
print(
ggplot(Italy_deaths %>% dplyr::filter(date > start_date),
       aes(date, Italy_death_new)) +
  geom_line(size = 1.0) +
  geom_point() +
  geom_smooth(span = span,
              col = 'red',
              linetype = 'dashed') +
  # geom_line(aes(date, Italy_death_estimated), 
  #           col = 'red', 
  #           linetype = 'dashed',
  #           size = 1.25) +
  labs(x = 'Date',
       y = "New reported deaths in the Italy from Covid-19") +
  theme_bw()
)
dev.off()

