library(tidyverse)
## dplyr 
## 1. select
## 2. filter
## 3. arrange
## 4. mutate
## 5. summarise
## other functions: count, distinct
## join tables, bind_row, bind_col

df <- mtcars %>%
  rownames_to_column(var="model") %>%
  tibble()

## random sample/ sampling
set.seed(9)
df %>%
  sample_n(3)
  
df %>%
  sample_frac(0.2)
  
df %>%
  select(model, am) %>%
  mutate(am = ifelse(am==0, "Auto", "Manual")) %>%
  count(am) %>%
  mutate(pct = n/ sum(n))
  
## distinct
model_names <- df %>%
  select(model, am) %>%
  mutate(am = ifelse(am==0, "Auto", "Manual")) %>%
  distinct(model) %>%
  pull()

## join tables
data()

## SQL joins
## inner, left, right, full

## inner join
band_members %>%
  inner_join(band_instruments, by = "name")

## left join
band_members %>%
  left_join(band_instruments, by = "name")

## right join
band_members %>%
  right_join(band_instruments, by = "name")

## full outer join
band_members %>%
  full_join(band_instruments, by="name")

## replace na
band_members %>%
  full_join(band_instruments, by="name") %>%
  mutate(plays = replace_na(plays, "drum"),
         band = replace_na(band, "Aerosmith"))

## union data (like SQL)
df1 <- data.frame(id=1:3,
                  name=c("toy","john","mary"))

df2 <- data.frame(id=3:5,
                  name=c("mary", "anna", "david") )

df3 <- data.frame(id=6:8,
                  name=c("a","b","c"))

df4 <- data.frame(id=9:10,
                  name=c("d","e"))

## bind rows and then remove duplicates
df1 %>%
  bind_rows(df2) %>%
  bind_rows(df3) %>%
  bind_rows(df4) %>%
  distinct()

## shortcut when we have multiple dataframes
list_df <- list(df1, df2, df3, df4)

list_df %>% 
  bind_rows() %>%
  distinct()


## join in case key names are not the same
band_members %>%
  rename(friend = name) %>%
  left_join(band_instruments, by=c("friend"="name") )

## basic data visualization
## grammar of graphic
library(tidyverse) ## ggplot2

## histogram
qplot(mpg, data=mtcars, geom="histogram", bins=10)

## density
qplot(mpg, data=mtcars, geom="density")

## scatter plot
qplot(x=hp, y=mpg, data=mtcars, geom="point")

## ggplot ของแทร่
ggplot(data = mtcars,
       aes(x = hp, y = mpg)) +
  geom_point(col="red") +
  geom_smooth(se=F) +
  theme_minimal()
