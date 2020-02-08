# follows the logic for Berger tables:
# https://en.wikipedia.org/wiki/Round-robin_tournament
# This schedule can also be represented as a (n-1, n-1) table, 
# expressing a round in which players meets each other.
# If a player meets itself, then this shows a bye (odd n),
# or a game against player n (even n).

round_robin <- function(n_teams) {
  # grid works for odd n; if even, needs to be n-1 X n-1
  dims <- ifelse(n_teams %% 2, n_teams, n_teams - 1)
  
  matchup_grid <- matrix(1:dims, nrow = 1)
  for (i in 2:(dims)) {
    matchup_grid <- rbind(matchup_grid, 
                          c(matchup_grid[i-1,-1], matchup_grid[i-1,1]))
  }
  
  rounds <- list()
  for (i in 1:dims) {
    this_round <- which(matchup_grid == i, arr.ind = TRUE)
    this_round[which(this_round[,1] == this_round[,2]), 2] <- 
      ifelse(n_teams %% 2, 0, n_teams)
    #rounds[[i]] <- unique(t(apply(this_round, 1, sort)))
    # this will (mostly) balance home vs away
    if(i %% 2) {
      rounds[[i]] <- unique(t(apply(this_round, 1, sort)))
    } else {
      rounds[[i]] <- unique(t(apply(this_round, 1, sort)))[,c(2,1)]
    }
  }
 
  attr(rounds, "n_teams") <- n_teams
   
  return(rounds)
}

# n_rounds: number of rounds to play
# defaults to each team plays the other once
order_games <- function(robin_order, n_rounds = NULL) {
  if(is.null(n_rounds)) {
    n_teams <- attr(robin_order, "n_teams")
    n_rounds <- ifelse(n_teams %% 2, n_teams, n_teams - 1)
  }
  
  # order in which to access the round_robin grid list
  access_order <- rep(1:length(robin_order), length = n_rounds)
   
  game_order <- data.frame(Game = 1:nrow(robin_order[[1]]))
  counter <- 1
  for (i in 1:n_rounds) {
    # flip the home vs away order for even numbered reps
    current_rep <- ((counter - 1) %/% length(robin_order)) + 1
    
    if (current_rep %% 2) {
      game_order[,paste("Round", i)] <- 
        do.call(paste, as.data.frame(robin_order[[access_order[i]]]))
    } else {
      game_order[,paste("Round", i)] <- 
        do.call(paste, as.data.frame(robin_order[[access_order[i]]][,2:1]))
    }
    
    counter <- counter + 1
  }
  
  return(game_order)
}

matchup_grid <- round_robin(10)
order_games(matchup_grid, n_rounds = 18)

