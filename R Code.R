
### You know nothing, John Snow!

library(tidyverse)
library(HistData)
library(ggthemes)
library(ggimage)
library(gganimate)

### Let's first look at deaths over time

HistData::Snow.dates %>%
  gather(Metric, Count, -date) %>%
  ggplot(aes(x = date, y= Count, colour = Metric)) +
  geom_line(size = .8) +
  theme_classic() +
  scale_color_stata() +
  labs(x = "Date", title = "Cholera Deaths over Time") +
  scale_x_date(date_labels = "%A, %d %B %Y", date_breaks = "2 weeks")

### Let's understand Cholera more generally

HistData::Cholera %>%
  mutate(death_rate = cholera_deaths / popn) %>%
  ggplot(aes(y = death_rate, x = water, fill = water, colour = water)) +
  geom_violin(alpha = .6) +
  geom_jitter() +
  scale_fill_stata() +
  scale_colour_stata() +
  theme_classic()

### Let's have a more general look

pairs(death_rate ~ popn + elevation + pop_dens + house_valpp + poor_rate +area + houses + house_val,
      data = HistData::Cholera %>%
        mutate(death_rate = cholera_deaths / popn))

### Creating the plot

HistData::Snow.streets %>% 
  arrange(desc(street), desc(n)) %>%
  ggplot() +
  geom_path(aes(x = x, y = y, group = street)) +
  geom_image(data = HistData::Snow.deaths, aes(x = x, y = y),
             image = "https://pngimg.com/uploads/death/death_PNG55.png", size = .03) +
  geom_density2d(data = HistData::Snow.deaths, aes(x = x, y = y), colour = "red", size = .75, alpha = .5) +
  stat_density2d(data = HistData::Snow.deaths, aes(x = x, y = y,
                                                     fill = ..level.., alpha = ..level..), 
                 geom = "polygon", size = .01, bins = 10) +
  scale_fill_gradient(low = NA,
                        high = "red", guide = FALSE) +
  geom_image(data = HistData::Snow.pumps, aes(x = x, y = y, 
        image = "http://www.stickpng.com/assets/images/5a05852a9cf05203c4b603c3.png"), 
             size = .038) +
  guides(colour = FALSE, alpha = FALSE) + 
  theme_classic() +
  labs(x = "", y = "", title = "John Snow's Analysis: Cholera Deaths in London in 1854", 
       subtitle = "The water pump in the middle seems to be the infected",
       caption = "It seems John Snow knew something") +
  geom_image(x = 5, y = 5, 
             image = "https://stickeroid.com/uploads/pic/full/thumb/stickeroid_5bf56b718319e.png",
             size = .2)

### Animating the plot

### First create a dataframe that randomly introduces ten observations at a time
### Well, it selects a growing subset and then assigns that a value
### So, the final level of the factor has all cases, the first has one

df <- HistData::Snow.deaths[1,]
df$frame <- 1

df <- df[0,]

for(i in seq(from = 10, to = nrow(HistData::Snow.deaths), by = 5)){
  append_df <- HistData::Snow.deaths[1:i,]
  append_df$frame <- i
  df <- rbind(df, append_df)
}

### Let's make it levels of a factor so we get frames in succession

df <- df %>% 
  mutate(frame = frame %>% as.factor %>% as.numeric)

### Let's see how we went

plot <- HistData::Snow.streets %>% 
  arrange(desc(street), desc(n)) %>%
  ggplot() +
  geom_path(aes(x = x, y = y, group = street)) +
  geom_point(data = df, aes(x = x, y = y, frame = frame), size = 1.5, colour = "dark red") +
  geom_point(data = HistData::Snow.pumps, aes(x = x, y = y), colour = "dark blue", alpha = .7, size = 4) +
  guides(colour = FALSE, alpha = FALSE) + 
  theme_classic() +
  labs(x = "", y = "", title = "John Snow's Analysis: Cholera Deaths in London in 1854
       It seems John Snow knew something")

ggplotly(plot) %>%
  animation_opts(frame = 10, transition = 8, redraw = FALSE)

### Let's do that again with the density contours etc.

plot <- HistData::Snow.streets %>% 
  arrange(desc(street), desc(n)) %>%
  ggplot() +
  geom_path(aes(x = x, y = y, group = street)) +
  geom_point(data = df, aes(x = x, y = y, frame = frame), size = 1.5, colour = "dark red") +
  geom_density2d(data = df, aes(x = x, y = y, frame = frame, group = frame), colour = "dark red", 
                 size = 1, alpha = .2) +
  stat_density2d(data = df, aes(x = x, y = y, frame = frame, group = frame), 
                 geom = "polygon", size = .01, bins = 10, fill = "dark red", alpha = .2) +
  geom_point(data = HistData::Snow.pumps, aes(x = x, y = y), colour = "dark blue", alpha = .7, size = 4) +
  guides(colour = FALSE, alpha = FALSE) + 
  theme_classic() +
  labs(x = "", y = "", title = "John Snow's Analysis: Cholera Deaths in London in 1854
       It seems John Snow knew something")

ggplotly(plot) %>%
  animation_opts(frame = 10, transition = 5, redraw = FALSE)

### Let's test a bug. This was solved

test <- iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, group = Species, frame = Species)) +
  geom_density2d(size = 1, alpha = .5)

ggplotly(test)
