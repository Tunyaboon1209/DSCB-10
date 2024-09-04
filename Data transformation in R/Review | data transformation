library(dplyr)
library(tidyverse)
tracks = read_csv("Tracks.csv")
head(tracks)

#mutate
## transform milliseconds into seconds
tracks %>%
  select(TrackId, Name, AlbumId, Milliseconds) %>%
  mutate(Min = round(Milliseconds / 60000))

## summarize
tracks %>%
  group_by(AlbumId) %>%
  summarise(avg_langth = mean(Milliseconds),
            sum_length = sum(Milliseconds),
            min_length = min(Milliseconds),
            max_length = max(Milliseconds),
            count_length = n())
