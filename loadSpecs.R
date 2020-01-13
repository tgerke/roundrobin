library(tidyverse)
library(lubridate)
library(hms)

# input date parameters ---------------------------------------------------

practice_begin <- as_date("2020-02-01")
games_begin <- as_date("2020-02-15")
season_end <- as_date("2020-04-25")

# input team parameters ---------------------------------------------------

teams <- tribble(
  ~division, ~team, ~manager,
  "Shetland", "Marlins", "Macaluso",
  "Shetland", "Red Sox", "Gerke",
  "Shetland", "Pirates", "Williams",
  "Shetland", "Astros", "Perez",
  "Shetland", "Dodgers", "Suarez",
  "Shetland", "Rays", "Priede",
  "Shetland", "A's", "Thompson", 
  "Shetland", "Cardinals", "Patel",
  "Shetland", "DBacks", "Johnson", 
  "Shetland", "Rockies", "Gato",
  "Pinto", "Marlins", "Macaluso",
  "Pinto", "Red Sox", "Gerke",
  "Pinto", "Pirates", "Williams",
  "Pinto", "Astros", "Perez",
  "Pinto", "Dodgers", "Suarez",
  "Pinto", "Rays", "Priede",
  "Pinto", "A's", "Thompson",
  "Pinto", "Cardinals", "Patel"
)


# make table of games -----------------------------------------------------

# random sampling needed to deterimine order of games and home/away balance
set.seed(8675309)
left_join(teams, teams, 
  by = "division"
  ) %>%
  filter(team.x != team.y) %>%
  rowwise %>%
  mutate(matchup = toString(sort(c(team.x,team.y)))) %>%
  select(division, matchup) %>%
  distinct() %>%
  separate(matchup, into = c("Team1", "Team2"), sep = ", ") 

# input event series parameters -------------------------------------------

# these detail how many events (and of which type) occur in each division
# for each day of the week. Times of day will be added later, as well as
# exclusions for weeks on/off for game scheduling.
events <- tribble(
  ~division, ~day, ~event, ~how_many,
  "Shetland", "Mon", "Cages", 2,
  "Shetland", "Tue", "Cages", 2,
  "Shetland", "Wed", "Cages", 2,
  "Shetland", "Thu", "Cages", 2,
  "Shetland", "Fri", "Cages", 2,
  "Shetland", "Mon", "Field practice", 2,
  "Shetland", "Tue", "Field practice", 2,
  "Shetland", "Wed", "Field practice", 2,
  "Shetland", "Thu", "Field practice", 2,
  "Shetland", "Fri", "Field practice", 2,
  "Shetland", "Sat", "Field practice", 5,
  "Shetland", "Sun", "Field practice", 5,
  "Shetland", "Mon", "Game", 1,
  "Shetland", "Tue", "Game", 1,
  "Shetland", "Wed", "Game", 1,
  "Shetland", "Thu", "Game", 1,
  "Shetland", "Fri", "Game", 1,
  "Shetland", "Sat", "Game", 5,
)

