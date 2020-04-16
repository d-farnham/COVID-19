rm(list = ls())
package.list <- list("ggplot2", "dplyr", "reshape2", "lubridate")
source('R/load_packages.R') # load packages


# read in the data
China_deaths = read.csv("~/Google Drive/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  dplyr::filter(Country.Region == 'China') %>%
  dplyr::select(-Province.State, -Country.Region, -Lat, -Long) %>%
  reshape2::melt(, variable.name = 'date', value.name = 'China_deaths') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(China_deaths = sum(China_deaths)) %>%
  dplyr::mutate(date = sub(date, pattern = 'X', replacement = ''),
                date = as.Date(date, format = '%m.%d.%y'),
                China_death_new = China_deaths - lag(China_deaths),
                date_numeric = lubridate::yday(date))


# start date with 10 total deaths
start_date = China_deaths$date[min(which(China_deaths$China_deaths > 10))]

# let's only look after the date when 10 total deaths had occured
China_deaths = China_deaths %>% dplyr::filter(date > start_date)

# fit non-linear model
mod <- lm(log(China_death_new+1) ~ date_numeric, data = China_deaths)

# set span such that we fit on the nearest 14 points
span = 14/nrow(China_deaths)

# add fitted curve
China_deaths = China_deaths %>% dplyr::mutate(China_death_estimated = exp(predict(mod, list(date_numeric = date_numeric))))

pdf(file = 'figs/China_deaths_and_exponential_fit.pdf',
    height = 5,
    width = 8)
print(
  ggplot(China_deaths %>% dplyr::filter(date > start_date),
         aes(date, China_death_new)) +
    geom_line(size = 1.0) +
    geom_point() +
    geom_smooth(span = span,
                col = 'red',
                linetype = 'dashed') +
    # geom_line(aes(date, China_death_estimated), 
    #           col = 'red', 
    #           linetype = 'dashed',
    #           size = 1.25) +
    labs(x = 'Date',
         y = "New reported deaths in the China from Covid-19") +
    theme_bw()
)
dev.off()
