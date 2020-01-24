library(tidyverse)
library(lubridate)
library(hms)

# input team parameters ---------------------------------------------------

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


# practice weeks ----------------------------------------------------------

week1_slots <- tribble(
  ~division, ~date, ~type, ~time,
  # Saturday
  "Shetland", as_date("2020-01-25"), "Field", "9:00 AM",
  "Shetland", as_date("2020-01-25"), "Field", "10:30 AM",
  "Shetland", as_date("2020-01-25"), "Field", "12:00 PM",
  "Shetland", as_date("2020-01-25"), "Field", "1:30 PM",
  "Shetland", as_date("2020-01-25"), "Field", "3:00 PM",
  "Pinto", as_date("2020-01-25"), "Field", "9:00 AM",
  "Pinto", as_date("2020-01-25"), "Field", "10:30 AM",
  "Pinto", as_date("2020-01-25"), "Field", "12:00 PM",
  "Pinto", as_date("2020-01-25"), "Field", "1:30 PM",
  "Pinto", as_date("2020-01-25"), "Field", "3:00 PM",
  "Mustang", as_date("2020-01-25"), "Field", "9:00 AM",
  "Mustang", as_date("2020-01-25"), "Field", "10:30 AM",
  "Mustang", as_date("2020-01-25"), "Field", "12:00 PM",
  "Mustang", as_date("2020-01-25"), "Field", "1:30 PM",
  "Mustang", as_date("2020-01-25"), "Field", "3:00 PM",
  "Bronco", as_date("2020-01-25"), "Field", "9:00 AM",
  "Bronco", as_date("2020-01-25"), "Field", "11:00 AM",
  "Bronco", as_date("2020-01-25"), "Field", "1:00 PM",
  # Sunday
  "Shetland", as_date("2020-01-26"), "Field", "9:00 AM",
  "Shetland", as_date("2020-01-26"), "Field", "10:30 AM",
  "Shetland", as_date("2020-01-26"), "Field", "12:00 PM",
  "Shetland", as_date("2020-01-26"), "Field", "1:30 PM",
  "Shetland", as_date("2020-01-26"), "Field", "3:00 PM",
  "Pinto", as_date("2020-01-26"), "Field", "9:00 AM",
  "Pinto", as_date("2020-01-26"), "Field", "10:30 AM",
  "Pinto", as_date("2020-01-26"), "Field", "12:00 PM",
  "Pinto", as_date("2020-01-26"), "Field", "1:30 PM",
  "Mustang", as_date("2020-01-26"), "Field", "9:00 AM",
  "Mustang", as_date("2020-01-26"), "Field", "10:30 AM",
  # Monday
  "Shetland", as_date("2020-01-27"), "Field", "5:30 PM",
  "Shetland", as_date("2020-01-27"), "Field", "7:00 PM",
  "Shetland", as_date("2020-01-27"), "Cages", "6:00 PM",
  "Shetland", as_date("2020-01-27"), "Cages", "7:00 PM",
  "Pinto", as_date("2020-01-27"), "Field", "5:30 PM",
  "Pinto", as_date("2020-01-27"), "Field", "7:00 PM",
  "Pinto", as_date("2020-01-27"), "Cages", "6:00 PM",
  "Pinto", as_date("2020-01-27"), "Cages", "7:00 PM",
  "Mustang", as_date("2020-01-27"), "Field", "5:30 PM",
  "Mustang", as_date("2020-01-27"), "Field", "7:00 PM",
  "Mustang", as_date("2020-01-27"), "Cages", "6:00 PM",
  "Mustang", as_date("2020-01-27"), "Cages", "7:00 PM",
  #"Bronco", as_date("2020-01-27"), "Field", "5:30 PM",
  #"Bronco", as_date("2020-01-27"), "Field", "7:00 PM",
  #"Bronco", as_date("2020-01-27"), "Cages", "6:00 PM",
  #"Bronco", as_date("2020-01-27"), "Cages", "7:00 PM",
  # Tuesday
  "Shetland", as_date("2020-01-28"), "Field", "5:30 PM",
  "Shetland", as_date("2020-01-28"), "Field", "7:00 PM",
  "Shetland", as_date("2020-01-28"), "Cages", "6:00 PM",
  "Shetland", as_date("2020-01-28"), "Cages", "7:00 PM",
  "Pinto", as_date("2020-01-28"), "Field", "5:30 PM",
  "Pinto", as_date("2020-01-28"), "Field", "7:00 PM",
  "Pinto", as_date("2020-01-28"), "Cages", "6:00 PM",
  "Pinto", as_date("2020-01-28"), "Cages", "7:00 PM",
  "Mustang", as_date("2020-01-28"), "Field", "5:30 PM",
  #"Mustang", as_date("2020-01-28"), "Field", "7:00 PM",
  "Mustang", as_date("2020-01-28"), "Cages", "6:00 PM",
  #"Mustang", as_date("2020-01-28"), "Cages", "7:00 PM",
  #"Bronco", as_date("2020-01-28"), "Field", "5:30 PM",
  "Bronco", as_date("2020-01-28"), "Field", "7:00 PM",
  "Bronco", as_date("2020-01-28"), "Cages", "6:00 PM",
  "Bronco", as_date("2020-01-28"), "Cages", "7:00 PM",
  # Wednesday
  "Shetland", as_date("2020-01-29"), "Field", "5:30 PM",
  "Shetland", as_date("2020-01-29"), "Field", "7:00 PM",
  "Shetland", as_date("2020-01-29"), "Cages", "6:00 PM",
  "Shetland", as_date("2020-01-29"), "Cages", "7:00 PM",
  "Pinto", as_date("2020-01-29"), "Field", "5:30 PM",
  "Pinto", as_date("2020-01-29"), "Field", "7:00 PM",
  "Pinto", as_date("2020-01-29"), "Cages", "6:00 PM",
  "Pinto", as_date("2020-01-29"), "Cages", "7:00 PM",
  "Mustang", as_date("2020-01-29"), "Field", "5:30 PM",
  "Mustang", as_date("2020-01-29"), "Field", "7:00 PM",
  "Mustang", as_date("2020-01-29"), "Cages", "6:00 PM",
  #"Mustang", as_date("2020-01-29"), "Cages", "7:00 PM",
  #"Bronco", as_date("2020-01-29"), "Field", "5:30 PM",
  "Bronco", as_date("2020-01-29"), "Field", "7:00 PM",
  "Bronco", as_date("2020-01-29"), "Cages", "6:00 PM",
  #"Bronco", as_date("2020-01-29"), "Cages", "7:00 PM",
  # Thursday
  "Shetland", as_date("2020-01-30"), "Field", "5:30 PM",
  "Shetland", as_date("2020-01-30"), "Field", "7:00 PM",
  "Shetland", as_date("2020-01-30"), "Cages", "6:00 PM",
  "Shetland", as_date("2020-01-30"), "Cages", "7:00 PM",
  "Pinto", as_date("2020-01-30"), "Field", "5:30 PM",
  "Pinto", as_date("2020-01-30"), "Field", "7:00 PM",
  "Pinto", as_date("2020-01-30"), "Cages", "6:00 PM",
  "Pinto", as_date("2020-01-30"), "Cages", "7:00 PM",
  "Mustang", as_date("2020-01-30"), "Field", "5:30 PM",
  #"Mustang", as_date("2020-01-30"), "Field", "7:00 PM",
  "Mustang", as_date("2020-01-30"), "Cages", "6:00 PM",
  #"Mustang", as_date("2020-01-30"), "Cages", "7:00 PM",
  #"Bronco", as_date("2020-01-30"), "Field", "5:30 PM",
  #"Bronco", as_date("2020-01-30"), "Field", "7:00 PM",
  #"Bronco", as_date("2020-01-30"), "Cages", "6:00 PM",
  #"Bronco", as_date("2020-01-30"), "Cages", "7:00 PM",
  # Friday
  "Shetland", as_date("2020-01-31"), "Field", "5:30 PM",
  "Shetland", as_date("2020-01-31"), "Field", "7:00 PM",
  "Shetland", as_date("2020-01-31"), "Cages", "6:00 PM",
  "Shetland", as_date("2020-01-31"), "Cages", "7:00 PM",
  "Pinto", as_date("2020-01-31"), "Field", "5:30 PM",
  #"Pinto", as_date("2020-01-31"), "Field", "7:00 PM",
  "Pinto", as_date("2020-01-31"), "Cages", "6:00 PM",
  #"Pinto", as_date("2020-01-31"), "Cages", "7:00 PM",
  "Mustang", as_date("2020-01-31"), "Field", "5:30 PM",
  #"Mustang", as_date("2020-01-31"), "Field", "7:00 PM",
  #"Mustang", as_date("2020-01-31"), "Cages", "6:00 PM",
  "Mustang", as_date("2020-01-31"), "Cages", "7:00 PM",
  #"Bronco", as_date("2020-01-31"), "Field", "5:30 PM",
  #"Bronco", as_date("2020-01-31"), "Field", "7:00 PM",
  #"Bronco", as_date("2020-01-31"), "Cages", "6:00 PM",
  #"Bronco", as_date("2020-01-31"), "Cages", "7:00 PM",
)  %>% 
  mutate(week = 1, team = " ")

practice_slots <- bind_rows(week1_slots,
                            week1_slots %>%
                              mutate(date = date + 7, 
                                     week = week + 1), 
                            week1_slots %>%
                              mutate(date = date + 14, 
                                     week = week + 2)) %>% 
  mutate(day = weekdays(date),
         numeric_time = parse_date_time(paste(date, time), '%Ymd %I:%M %p'))

#assign first 3 weeks ----------------------------------------------------
  
set.seed(114)
# weekend
shetland_order <- sample_n(teams %>% 
                             filter(division == "Shetland") %>%
                             select(team),
                           replace = FALSE, size = n())
practice_slots[practice_slots$division == "Shetland" & 
               practice_slots$week == 1 &
               practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  shetland_order
# weekday fields
practice_slots[practice_slots$division == "Shetland" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Field" & 
                 !practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  shetland_order
# weekday cages
field_days <- practice_slots %>% 
  filter(division == "Shetland", week == 1, type == "Field", 
         !day %in% c("Saturday", "Sunday")) %>%
  select(team, day)
wgts <- 0
repeat{
    day_slots <- practice_slots %>%
      filter(division == "Shetland", week == 1, type == "Cages") %>%
      select(day) %>%
      mutate(team = " ")
    for (i in 1:nrow(day_slots)) {
      available <- teams %>% 
        filter(division == "Shetland") %>%
        pull(team)
      day_to_fill <- pull(day_slots, day)[i]
      wgts <- rep(1, length(available))
      booked <- field_days %>% filter(day == day_to_fill) %>% pull(team)
      wgts[available %in% booked] <- 0
      had_cages <- day_slots %>% pull(team)
      wgts[available %in% had_cages] <- 0
      if (sum(wgts) == 0) {break}
      day_slots$team[i] <- sample(available, size = 1, prob = wgts)
    }
    if(sum(grepl(" ", day_slots$team)) == 0) {break}
}
practice_slots[practice_slots$division == "Shetland" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Cages",
                 "team"] <- 
  day_slots %>% pull(team)

pinto_order <- sample_n(teams %>% 
                          filter(division == "Pinto") %>%
                          select(team),
                        replace = FALSE, size = n())
# weekend fields
practice_slots[practice_slots$division == "Pinto" & 
                 practice_slots$week == 1 &
                 practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  pinto_order
# weekday fields
practice_slots[practice_slots$division == "Pinto" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Field" & 
                 !practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  pinto_order
# weekday cages
field_days <- practice_slots %>% 
  filter(division == "Pinto", week == 1, type == "Field", 
         !day %in% c("Saturday", "Sunday")) %>%
  select(team, day)
wgts <- 0
repeat{
  day_slots <- practice_slots %>%
    filter(division == "Pinto", week == 1, type == "Cages") %>%
    select(day) %>%
    mutate(team = " ")
  for (i in 1:nrow(day_slots)) {
    available <- teams %>% 
      filter(division == "Pinto") %>%
      pull(team)
    day_to_fill <- pull(day_slots, day)[i]
    wgts <- rep(1, length(available))
    booked <- field_days %>% filter(day == day_to_fill) %>% pull(team)
    wgts[available %in% booked] <- 0
    had_cages <- day_slots %>% pull(team)
    wgts[available %in% had_cages] <- 0
    if (sum(wgts) == 0) {break}
    day_slots$team[i] <- sample(available, size = 1, prob = wgts)
  }
  if(sum(grepl(" ", day_slots$team)) == 0) {break}
}
practice_slots[practice_slots$division == "Pinto" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Cages",
               "team"] <- 
  day_slots %>% pull(team)

mustang_order <- sample_n(teams %>% 
                          filter(division == "Mustang") %>%
                          select(team),
                        replace = FALSE, size = n())
# weekend fields
practice_slots[practice_slots$division == "Mustang" & 
                 practice_slots$week == 1 &
                 practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  mustang_order
# weekday fields
practice_slots[practice_slots$division == "Mustang" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Field" & 
                 !practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  mustang_order
# weekday cages
field_days <- practice_slots %>% 
  filter(division == "Mustang", week == 1, type == "Field", 
         !day %in% c("Saturday", "Sunday")) %>%
  select(team, day)
wgts <- 0
repeat{
  day_slots <- practice_slots %>%
    filter(division == "Mustang", week == 1, type == "Cages") %>%
    select(day) %>%
    mutate(team = " ")
  for (i in 1:nrow(day_slots)) {
    available <- teams %>% 
      filter(division == "Mustang") %>%
      pull(team)
    day_to_fill <- pull(day_slots, day)[i]
    wgts <- rep(1, length(available))
    booked <- field_days %>% filter(day == day_to_fill) %>% pull(team)
    wgts[available %in% booked] <- 0
    had_cages <- day_slots %>% pull(team)
    wgts[available %in% had_cages] <- 0
    if (sum(wgts) == 0) {break}
    day_slots$team[i] <- sample(available, size = 1, prob = wgts)
  }
  if(sum(grepl(" ", day_slots$team)) == 0) {break}
}
practice_slots[practice_slots$division == "Mustang" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Cages",
               "team"] <- 
  day_slots %>% pull(team)

bronco_order <- sample_n(teams %>% 
                            filter(division == "Bronco") %>%
                            select(team),
                          replace = FALSE, size = n())
# weekend fields
practice_slots[practice_slots$division == "Bronco" & 
                 practice_slots$week == 1 &
                 practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  bronco_order
# weekday fields
practice_slots[practice_slots$division == "Bronco" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Field" & 
                 !practice_slots$day %in% c("Saturday", "Sunday"), "team"] <- 
  bronco_order
# weekday cages
field_days <- practice_slots %>% 
  filter(division == "Bronco", week == 1, type == "Field", 
         !day %in% c("Saturday", "Sunday")) %>%
  select(team, day)
wgts <- 0
repeat{
  day_slots <- practice_slots %>%
    filter(division == "Bronco", week == 1, type == "Cages") %>%
    select(day) %>%
    mutate(team = " ")
  for (i in 1:nrow(day_slots)) {
    available <- teams %>% 
      filter(division == "Bronco") %>%
      pull(team)
    day_to_fill <- pull(day_slots, day)[i]
    wgts <- rep(1, length(available))
    booked <- field_days %>% filter(day == day_to_fill) %>% pull(team)
    wgts[available %in% booked] <- 0
    had_cages <- day_slots %>% pull(team)
    wgts[available %in% had_cages] <- 0
    if (sum(wgts) == 0) {break}
    day_slots$team[i] <- sample(available, size = 1, prob = wgts)
  }
  if(sum(grepl(" ", day_slots$team)) == 0) {break}
}
practice_slots[practice_slots$division == "Bronco" & 
                 practice_slots$week == 1 &
                 practice_slots$type == "Cages",
               "team"] <- 
  day_slots %>% pull(team)

                    
  
out <- left_join(practice_slots, teams, by = c("division", "team")) %>%
  filter(week == 1) %>% 
  select(division, date, day, time, type, team, manager)

# manual updates
out[out$division == "Bronco" & out$day == "Saturday" & out$team == "Rays", "time"] <- "11:30 AM"
out[out$division == "Bronco" & out$day == "Saturday" & out$team == "A's", "date"] <- 
    as_date("2020-01-26")
out[out$division == "Bronco" & out$day == "Saturday" & out$team == "A's", "day"] <- 
  "Sunday"
out[out$division == "Bronco" & out$day == "Sunday" & out$team == "A's", "time"] <- 
  "11:00 AM"

write_csv(out, path = here::here("exports", paste(today(), "week1.csv", sep = "_")))

# 
# # make table of games -----------------------------------------------------
# 
# # random sampling needed to deterimine order of games and home/away balance
# set.seed(8675309)
# left_join(teams, teams, 
#   by = "division"
#   ) %>%
#   filter(team.x != team.y) %>%
#   rowwise %>%
#   mutate(matchup = toString(sort(c(team.x,team.y)))) %>%
#   select(division, matchup) %>%
#   distinct() %>%
#   separate(matchup, into = c("Team1", "Team2"), sep = ", ") 
# 
# # input event series parameters -------------------------------------------
# 
# # these detail how many events (and of which type) occur in each division
# # for each day of the week. Times of day will be added later, as well as
# # exclusions for weeks on/off for game scheduling.
# events <- tribble(
#   ~division, ~day, ~event, ~how_many,
#   "Shetland", "Mon", "Cages", 2,
#   "Shetland", "Tue", "Cages", 2,
#   "Shetland", "Wed", "Cages", 2,
#   "Shetland", "Thu", "Cages", 2,
#   "Shetland", "Fri", "Cages", 2,
#   "Shetland", "Mon", "Field practice", 2,
#   "Shetland", "Tue", "Field practice", 2,
#   "Shetland", "Wed", "Field practice", 2,
#   "Shetland", "Thu", "Field practice", 2,
#   "Shetland", "Fri", "Field practice", 2,
#   "Shetland", "Sat", "Field practice", 5,
#   "Shetland", "Sun", "Field practice", 5,
#   "Shetland", "Mon", "Game", 1,
#   "Shetland", "Tue", "Game", 1,
#   "Shetland", "Wed", "Game", 1,
#   "Shetland", "Thu", "Game", 1,
#   "Shetland", "Fri", "Game", 1,
#   "Shetland", "Sat", "Game", 5,
# )
# 
