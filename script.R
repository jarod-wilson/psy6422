# / Info / ========================================================================================
# Raw R script for project completed as part of PSY6422 at the University of Sheffield
# Full project repository available from https://github.com/jarod-wilson/psy6422.

# / Libraries / ===================================================================================

library(tidyverse)
library(here)
library(knitr)
library(lubridate)
library(ggdark)
library(ggtext)

# / Data Preparation / ============================================================================

df <- read.csv(here("raw_data", "steam_users.csv"))

df <- df %>%
  rename(date = DateTime,
         users = Users,
         in_game = In.Game)

df$date <- as.Date(df$date, format="%Y-%m-%d %H:%M:%S")

df <- df %>%
  mutate(users = replace(users,is.na(users),0))

df <- df %>%
  mutate(in_game = replace(in_game,is.na(in_game),0))

df <- df %>%
  mutate(users = users / 1000000)

df <- df %>%
  mutate(in_game = in_game / 1000000)

df2 <- df %>%
  select(date, users, in_game) %>%
  filter(between(date, as.Date("2020-01-01"), as.Date("2020-12-31")))

# / Data Visualisation / ==========================================================================

# Drawing the plot
g <- ggplot(df2, aes(x = date)) +
  geom_line(aes(y = users), colour = "#0072B2") + 
  geom_line(aes(y = in_game), color = "#D55E00") +
  
  # Customising and theming the plot
  geom_vline(xintercept = as.Date("2020-03-13"), colour = "red") + # This adds a vertical line on the plot,
  annotate(geom="text", x = as.Date("2020-07-01"), y = 15,         # and this line annotates it with text.
           label = "COVID-19 is declared a pandemic by the WHO", fontface = "bold") +
  scale_x_date(date_breaks = "1 month", 
               date_minor_breaks = "1 week", 
               date_labels = "%b %Y") +
  dark_theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    plot.title = element_markdown()) + 
  
  # Using the package ggtext we can get the line colours in the title using HTML syntax, removing the need for a legend.
  labs(title = "How the number of <span style='color:#0072B2;'>Steam users</span> 
       and <span style='color:#D55E00;'>users in game</span> changed throughout 2020.",
       caption = "Visualisation by Jarod Wilson | Data taken from www.SteamDB.com",
       y = " Users in millions")

# / Extras / ======================================================================================

# Interactive plot with plotly

library(plotly)
g_int <- ggplotly(g)

g_int

# Animated plot with gganimate
library(gganimate)
g_anim <- g +
  transition_reveal(date)

animate(g_anim, end_pause = 50, duration = 20, fps = 10)