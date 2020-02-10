library(tidyverse)

source(here::here("R", "round_robin.R"))
# order_games(round_robin(n_teams = 10), n_rounds = 18)

# load an example set of teams
teams <- tribble(
  ~division, ~team, ~manager,
  "Shetland", "Orioles", "Marchese",
  "Shetland", "Pirates", "Rodriguez",
  "Shetland", "A's", "Gerke",
  "Shetland", "Diamondbacks", "Macaluso",
  "Shetland", "Rays", "Seaton",
  "Shetland", "Rockies", "Laporta", 
  "Shetland", "Yankees", "Alonso",
  "Shetland", "Nationals", "Goan", 
  "Shetland", "Marlins", "Sinn",
  "Shetland", "Dodgers", "Reace",
  "Pinto", "Orioles", "Suarez",
  "Pinto", "Pirates", "Huhn",
  "Pinto", "Diamondbacks", "Williams",
  "Pinto", "Rays", "Shiver",
  "Pinto", "Rockies", "Menendez",
  "Pinto", "Yankees", "Diaz",
  "Pinto", "Nationals", "Bennington",
  "Pinto", "Marlins", "Lopez",
  "Pinto", "Dodgers", "Perez",
  "Mustang", "Diamondbacks", "Tate",
  "Mustang", "Rays", "Fisher",
  "Mustang", "Rockies", "Alvarez",
  "Mustang", "Yankees", "Wakefield",
  "Mustang", "Nationals", "Garrido",
  "Mustang", "Marlins", "Primrose",
  "Mustang", "Dodgers", "Dobie",
  "Bronco", "A's", "Garcia",
  "Bronco", "Yankees", "Andux",
  "Bronco", "Rays", "Vazquez"
) 

shetland <- teams %>% filter(division == "Shetland")

# x is data.frame or tibble with team name (team_var)
# need 1 row per team, all unique team names
# if team_no_var is NULL, teams will be ordered sequentially into the
# round_robin algorithm. Optionally, the user can specify the numbers.
pretty_robin <- function(x, team_var = "team", team_no_var = NULL, n_rounds = NULL) {
  # do error checking for unique names
  # to-do
  
  # need to ensure x is a tibble, not a data.frame due to 
  # pull/match column referencing below
  
  # handle case where n_rounds == NULL
  
  # add a team numbering 
  if(is.null(team_no_var)) {
    x <- x %>% mutate(team_no = 1:n())
    team_no_var <- "team_no"
  }
  n_teams <- nrow(x)
  
  game_grid <- round_robin(n_teams = n_teams) %>% 
    order_games(n_rounds = n_rounds) %>%
    as_tibble()
  
  # to-do: need to add bye week to lookup when team == 0
  for (i in 1:n_rounds) {
    game_grid <- game_grid %>% 
      separate(paste('Round', i), into = c("away", "home")) %>%
      mutate(home = pull(x, team_var)[match(home, pull(x, team_no_var))],
             away = pull(x, team_var)[match(away, pull(x, team_no_var))]) %>%
      unite(!!paste('Round', i), home, away, sep = " vs ")
  }
  
  return(game_grid)
}

# single division example
teams %>% 
  filter(division == "Shetland") %>%
  pretty_robin(n_rounds = 18)
