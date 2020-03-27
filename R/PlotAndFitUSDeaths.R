rm(list = ls())
package.list <- list("ggplot2", "dplyr", "reshape2", "lubridate")
source('R/load_packages.R') # load packages


# read in the data
US_deaths = read.csv("~/Google Drive/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
                    dplyr::filter(Country.Region == 'US') %>%
                    dplyr::select(-Province.State, -Country.Region, -Lat, -Long) %>%
                    reshape2::melt(, variable.name = 'date', value.name = 'US_deaths') %>%
                    dplyr::mutate(date = sub(date, pattern = 'X', replacement = ''),
                                  date = as.Date(date, format = '%m.%d.%y'),
                                  US_death_new = US_deaths - lag(US_deaths),
                                  date_numeric = lubridate::yday(date))


# let's only look after Feb 28th
US_deaths = US_deaths %>% dplyr::filter(date > as.Date('2020-02-28'))

# fit non-linear model
mod <- nls(US_death_new ~ exp(a + b * date_numeric), data = US_deaths, start = list(a = 0, b = 0))



# add fitted curve
US_deaths = US_deaths %>%dplyr::mutate(US_death_estimated = predict(mod, list(date_numeric = date_numeric)))

ggplot(US_deaths %>% dplyr::filter(date > as.Date('2020-02-28'))) +
  geom_line(aes(date, US_death_new)) +
  geom_line(aes(date, US_death_estimated), col = 'red', linetype = 'dashed') +
  theme_bw()
