## functions.r
## 11/19/14 BCS
## Functions necessary for CFB ranking generation


# randomly rank teams in preseason
preseason.rank <- function(teams){
  # randomly rank all FBS teams
  fbs <- teams %>%
    filter(!is.na(conference)) %>%
    sample_frac()
  
  # randomly rank all FCS teams
  fcs <- teams %>%
    filter(is.na(conference)) %>%
    sample_frac()
  
  # combine ranks, all FBS teams ranked higher than FCS
  teams <- rbind(fbs, fcs) %>%
    mutate(rank=row_number(), week=0)
  
  return(teams)
}

# calculate new rank based upon game result
new.rank <- function(game, ranks) {
  # reshape to two observations and merge current rank
  game <- gather(game, outcome, team_id, winner:loser) %>%
    left_join(ranks, by="team_id")
  
  # if the loser is higher ranked, split the difference between the teams' previous ranks
  # else do nothing and keep the existing ranks
  if(select(filter(game, outcome=="winner"), rank) > select(filter(game, outcome=="loser"), rank)){
    # calculate mean rank of two teams
    mean.rank <- as.numeric(summarize(filter(ranks, team_id %in% game$team_id),mean=mean(rank)))
    
    ranks$rank[ranks$team_id==game$team_id[game$outcome=="winner"]] <- mean.rank+.1
    ranks$rank[ranks$team_id==game$team_id[game$outcome=="loser"]] <- mean.rank-.1
  }
  
  return(ranks)
}

# calculate new rank for each week
rank.week <- function(games, ranks, week.num){
  games <- filter(games, week==week.num)
  
  # only keep ranks from previous week
  current.ranks <- ranks %>%
    filter(week==(week.num-1)) %>%
    select(-week)
  
  # for each game, calculate new rank and replace pre.ranks with new ranks
  for(i in 1:nrow(games)){
    current.ranks <- new.rank(games[i,], current.ranks)
  }
  
  return(current.ranks)
}

# calculate new rank for each season
rank.season <- function(games, pre.ranks){
  # drop unnecessary variables from ranks
  pre.ranks <- select(pre.ranks, school, conference, team_id, rank, week)
  current.ranks <- pre.ranks
  
  # for each week, calculate new rank and store in data.frame
  overall.ranks <- pre.ranks
  
  for(i in unique(games$week)){
    current.ranks <- rank.week(games, current.ranks, i) %>%
      mutate(week=i)
    overall.ranks <- rbind(overall.ranks, current.ranks)
  }
  
  return(overall.ranks)
}