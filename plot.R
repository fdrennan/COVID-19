library(tidyverse)
library(fs)
library(lubridate)

csv_files <- dir_ls('csse_covid_19_data/csse_covid_19_daily_reports/')
csv_files <- csv_files[!str_detect(csv_files, 'README')]

data <- 
  map_df(csv_files, function(x) {
    csv_data <- 
      x %>% 
      read_csv %>% 
      mutate_all(as.character) 
    
    date_file <- str_remove(str_split(x, "/")[[1]][[3]], '.csv')
    date_file <-  as.Date(date_file, '%m-%d-%Y')
    csv_data$file_name = date_file
    csv_data
  })

data <- 
  data %>% 
  rename(
    province =`Province/State`,
    country = `Country/Region`,
    last_update = `Last Update`,
    confirmed = Confirmed,
    deaths = Deaths,
    recovered = Recovered
  ) %>% 
  mutate(
    last_update = as.Date(last_update, '%m/%d/%Y'),
    confirmed = as.numeric(confirmed),
    deaths = as.numeric(deaths),
    recovered = as.numeric(recovered)
  ) %>% 
  mutate_if(is.numeric, ~ if_else(is.na(.), as.numeric(0), .))


data %>% 
  group_by(file_name, country) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup %>% 
  ggplot() +
  aes(x = file_name, y = log(confirmed), colour = country) +
  geom_line() +
  theme(legend.position = 'none')


  
