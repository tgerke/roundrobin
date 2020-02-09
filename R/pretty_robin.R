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
  
  # add a team numbering 
  if(is.null(team_no_var)) {
    x <- x %>% mutate(team_no = 1:n())
  }
  n_teams <- nrow(x)
  
  game_grid <- round_robin(n_teams = n_teams) %>% 
    order_games(n_rounds = n_rounds)
  
  game_grid %>% 
    separate('Round 1', into = c("away", "home")) %>%
    mutate(home = x$team[match(home, x$team_no)],
           away = x$team[match(away, x$team_no)]) %>% 
    unite('Round 1', home, away, sep = " vs ")
  
  game_grid %>% select(-Game) %>% 
    purrr::map_df(~ separate(.x, into = c("away", "home")) %>%
        mutate(home = x$team[match(home, x$team_no)],
               away = x$team[match(away, x$team_no)]) #%>% 
        #unite("help", home, away, sep = " vs ")
  )  

}