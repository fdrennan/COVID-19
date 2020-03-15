library(tidyverse)
library(fs)
library(lubridate)
library(e1071)
library(glue)

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

# Clean up the data
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
    confirmed = as.numeric(confirmed),
    deaths = as.numeric(deaths),
    recovered = as.numeric(recovered)
  ) %>% 
  mutate_if(is.numeric, ~ if_else(is.na(.), as.numeric(0), .)) %>% 
  select(-last_update)


summaries <- 
  data %>% 
  filter(country == 'US') %>% 
  group_by(file_name, country) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup %>% 
  mutate(
    day = row_number()
  )

yesterday <- 
  summaries %>% 
  filter(file_name == Sys.Date() - 1) %>% 
  pull(day)


modeling_data <- 
  summaries %>% 
  mutate(confirmed = log(confirmed)) %>% 
  filter(
    file_name >= Sys.Date() - 7 * 2
  )

model <- lm(
  confirmed ~ day, data = modeling_data
)


print(yesterday)

future_days <- 
  tibble(
    day = 30:(yesterday + 15)
  )

future_days$confirmed <- predict(
  model, 
  future_days
) 

modeling_data$prediction = 'Actual'
future_days$prediction = 'Predicted'

combined_data <-
  bind_rows(modeling_data, future_days) %>% 
    mutate(
      day = day - yesterday - 1,
      file_name = Sys.Date() + day
    ) %>% 
  filter(
    file_name > Sys.Date() - 7 * 2
  ) %>% 
  mutate(
    confirmed = exp(confirmed)
  )


ggplot(combined_data) +
  aes(x = file_name, y  = log(confirmed), colour = prediction) +
  geom_line(size = 1)  +
  geom_point(size = 1) +
  xlab('Date') +
  ylab('Confirmed (log)') +
  ggtitle(
    'COVID-19 Predicted v. Actual on Log Scale'
  )


ggplot(combined_data) +
  aes(x = file_name, y  = confirmed, colour = prediction) +
  geom_line(size = 1)  +
  geom_point(size = 1) +
  xlab('Date') +
  ylab('Confirmed (Actual)')+
  ggtitle(
    'COVID-19 Predicted v. Actual'
  )

final_solution <- 
  combined_data %>% 
  arrange(
    file_name, prediction
  ) %>% 
  select(
    file_name, confirmed, prediction
  ) %>% 
  as.data.frame
  
write.csv(
  final_solution, glue("{Sys.Date()}.csv")
)
