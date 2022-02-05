# Libraries 

library(geniusr)
library(tidyverse)
library(ggtext)
library(extrafont)
library(patchwork)
library(TTR)

# Pulling Data 

genius_token() # initialize api token 

df <- get_artist_songs_df(artist_id = 1421)

# Collecting song id's

df <- df %>%
  select(song_id) %>%
  purrr::flatten_int()

# Songs data 

song_data <- df %>%
  purrr::map_dfr(get_song_df)

# Data cleaning and wrangling

song_data <- song_data[complete.cases(song_data[ , "song_release_date"]), ]
song_data <- song_data[complete.cases(song_data[ , "song_pageviews"]), ]
song_data <- song_data[complete.cases(song_data[ , "album_name"]), ]

album_df <- song_data %>%
  group_by(album_name) %>%
  summarise(n_songs = sum(song_pageviews)) %>%
  arrange(n_songs) %>%
  slice(1:10) %>%
  mutate(album_name = str_trunc(album_name, width = 35))

song_data <- song_data %>%
  arrange(song_release_date) %>%
  mutate(index = 1:nrow(song_data)) %>%
  mutate(views = TTR::SMA(song_pageviews, n = 10))

# Custom theme 

theme_custom <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#0d1117", fill = "#0d1117"),
          panel.background = element_rect(colour = "#0d1117", fill = "#0d1117")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "white", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 12, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plotting 

alb <- album_df %>%
  ggplot() +
  geom_bar(aes(x = reorder(album_name, n_songs), y = n_songs), stat = "identity", fill = "#41ab5d") +
  labs(title = "Kendrick Lamar's most popular albums",
       subtitle = "Sorted based on Song Page Views",
       y = "Views",
       x = "Album Names") +
  theme_custom() +
  coord_flip()

sng <- song_data %>%
  ggplot() +
  geom_line(aes(x = index, y = views), colour = "#41ab5d", size = 2) +
  geom_point(aes(x = index, y = views), colour = "#41ab5d", size = 2.5) +
  labs(title = "Kendrick Lamar Song Views Timeline",
       subtitle = "2009-2010-2011-2012-2015-2017",
       y = "Views",
       x = "Songs") +
  geom_vline(xintercept = 11, colour = "white", linetype = "longdash", size = 0.5) +
  geom_vline(xintercept = 45, colour = "white", linetype = "longdash", size = 0.5) +
  geom_vline(xintercept = 62, colour = "white", linetype = "longdash", size = 0.5) +
  geom_vline(xintercept = 81, colour = "white", linetype = "longdash", size = 0.5) +
  geom_vline(xintercept = 103, colour = "white", linetype = "longdash", size = 0.5) +
  geom_vline(xintercept = 130, colour = "white", linetype = "longdash", size = 0.5) +
  theme_custom()

# Combine plots

alb + sng + patchwork::plot_layout(ncol = 2) +
  labs(caption = "Data from Genius via geniusr\nCreated by Harsh Krishna")

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("Artist Overview.png", bg = "#0d1117", width = 8000, height = 2700, units = "px")