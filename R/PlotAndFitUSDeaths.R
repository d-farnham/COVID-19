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


# start date with 10 total deaths
start_date = US_deaths$date[min(which(US_deaths$US_deaths > 10))]

# let's only look after the date when 10 total deaths had occured
US_deaths = US_deaths %>% dplyr::filter(date > start_date)

# fit non-linear model
mod.lo <- loess(US_death_new ~ date_numeric, data = US_deaths, control = loess.control(surface = "direct"))
predict(mod.lo, newdata = data.frame(date_numeric = (max(US_deaths$date_numeric)+1):(max(US_deaths$date_numeric)+10), se = TRUE))


# set span such that we fit on the nearest 14 points
span = 14/nrow(US_deaths)

# add fitted curve
US_deaths = US_deaths %>% dplyr::mutate(US_death_estimated = exp(predict(mod, list(date_numeric = date_numeric))))

pdf(file = 'figs/US_deaths_and_exponential_fit.pdf',
    height = 5,
    width = 8)
print(
ggplot(US_deaths %>% dplyr::filter(date > start_date),
       aes(date, US_death_new)) +
  geom_line(size = 1.0) +
  geom_point() +
  geom_smooth(span = span,
              col = 'red',
              linetype = 'dashed') +
  # geom_line(aes(date, US_death_estimated), 
  #           col = 'red', 
  #           linetype = 'dashed',
  #           size = 1.25) +
  labs(x = 'Date',
       y = "New reported deaths in the US from Covid-19") +
  theme_bw()
)
dev.off()

