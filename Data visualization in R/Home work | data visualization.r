library(tidyverse)
library(dplyr)

#chart 1 scatter plot + smooth + rug
ggplot(economics, mapping = aes(x=pce, y=psavert)) +
  geom_point() +
  geom_smooth() +
  geom_rug()

#chart 2 box plot
econ_by_decade <- economics %>% 
        mutate(decade = lubridate::year(date) %/% 10 * 10) %>% 
        group_by(decade)

ggplot(econ_by_decade, mapping = aes(x = decade, 
                         y = psavert, 
                         group = decade, 
                         fill = decade)) +
  geom_boxplot()

#chart 3 scatter plot
ggplot(economics, mapping = aes(x=uempmed, y=psavert)) +
  geom_point() +
  geom_smooth()

#chart 4 bar chart
ggplot(econ_by_decade, mapping = aes(x = decade, y = unemploy, fill = decade)) +
  geom_col()

#chart 5 box plot
ggplot(econ_by_decade, mapping = aes(x = decade, y = pce, group = decade, fill = decade)) +
  geom_boxplot()
