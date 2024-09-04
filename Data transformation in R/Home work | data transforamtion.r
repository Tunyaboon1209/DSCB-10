install.packages("nycflights13")
install.packages("lubridate")
install.packages("stringr")
library(nycflights13)
library(dplyr)
library(lubridate)
library(stringr)

#1. Flag delayed flights
flights_delays = flights %>%
  select(year, dep_delay, arr_delay) %>%
  mutate(
    both_delayed = ifelse(dep_delay >= 0 & arr_delay >= 0, 0, 1)
  )

#2. calculate flight duration
time_formated_flights = flights %>%
  mutate(
    dep_time_formatted = paste0(
      str_pad(dep_time %/% 100, 2, pad = "0"), 
      ":", 
      str_pad(dep_time %% 100, 2, pad = "0")
      ),
    arr_time_formatted = paste0(
      str_pad(arr_time %/% 100, 2, pad = "0"),
      ":", 
      str_pad(arr_time %% 100, 2, pad = "0")
      ),
    dep_time_proper = strptime(dep_time_formatted, format = "%H:%M"),
    arr_time_proper = strptime(arr_time_formatted, format = "%H:%M")
  )

flights_duration = time_formated_flights %>%
  mutate(date = paste0(
                  year, "-", 
                  str_pad(month, 2, pad = "0"), "-", 
                  str_pad(day, 2, pad = "0"))
         , flight_duration = difftime(arr_time_proper, dep_time_proper, units = "mins")) %>%
  select(date, 
         dep_time = dep_time_formatted, 
         arr_time = arr_time_formatted, 
         flight, origin, 
         dest, 
         distance, 
         flight_duration)

#3. Create a categorical variable for flight distance
Categorize_flight_distance = flights %>%
  mutate(date = paste0(year, "-", 
                      str_pad(month, 2, pad = "0"), "-", 
                      str_pad(day, 2, pad = "0")),
          distance_category = cut(distance, 
                                  breaks = c(0, 1250, 2500, 5000), 
                                  labels = c("short haul", 
                                             "medium haul", 
                                             "long haul"))) %>%
  select(date, 
         flight, 
         origin, 
         dest, 
         distance, 
         distance_category)

#4. Calculate average delays by carrier
avg_delay_time = flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay + arr_delay, na.rm = TRUE))

#5. Determine busiest airports
busiest_airports <- flights %>% 
  count(origin, name = "flight_count") %>% 
  arrange(desc(flight_count))
