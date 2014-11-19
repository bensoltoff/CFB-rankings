## CFB-rank.r
## 11/17/14 BCS
## Automatically rank college football teams based upon random preseason ranking

rm(list=ls())


require(plyr)
require(dplyr)
require(tidyr)
require(snowfall)

set.seed(11091987)


# read in functions for ranking generation
source(file="functions.r")

# parameters to set
nsims <- 1000      # number of simulations to conduct for each season
ncpu <- 6         # number of available CPUs for parallel processing

# read in game outcomes (from Football-Ref)
games <- read.csv(file="data//cfb_games.csv", stringsAsFactors=FALSE)
games <- tbl_df(games)

# read in teams (from Wikipedia)
teams <- read.csv(file="data//teams.csv", stringsAsFactors=FALSE)
teams <- tbl_df(teams)

# merge FCS teams that play an FBS school into teams data frame
teams <- games %>%
  select(school, opponent) %>%
  gather(homeaway, school, school, opponent) %>%
  select(school) %>%
  unique() %>%
  left_join(teams) %>%
  mutate(team_id = row_number())
teams

# add a generic FCS conference
teams <- within(teams, conference[is.na(conference)] <- "FCS")

# replace school and opponent names with team id number
games <- games %>%
  left_join(select(teams, school, team_id)) %>%
  mutate(team1id=team_id) %>%
  select(-team_id) %>%
  left_join(rename(select(teams, school, team_id), opponent=school)) %>%
  mutate(team2id=team_id) %>%
  select(-team_id, -opponent, -school, -id) %>%
  arrange(date, gamenum, team1id, team2id)
games

# correct dates format
games <- games %>%
  mutate(date = as.numeric(strftime(as.Date(date,"%m/%d/%Y"), format = "%j")))
games

# identify in which week each game was play
game.weeks <- 16      # number of weeks in which regular season games are played
weeks <- data.frame(date=seq(from=as.numeric(strftime(as.Date("08/26/2014","%m/%d/%Y"), format = "%j")),
                             length.out=(7*game.weeks)),
                    week=sort(rep(1:game.weeks, times=7)))
weeks <- tbl_df(weeks)

games <- games %>%
  left_join(weeks)

# calculate who won the game
games <- games %>%
  mutate(outcome=(outcome=="W"), winner=(team1id*(outcome==TRUE) + team2id*(outcome==FALSE)),
         loser=(team1id*(outcome==FALSE) + team2id*(outcome==TRUE))) %>%
  select(date, week, winner, loser)
games
 

# simulate rankings with multiple pre-season rankings
# system.time(ranks <- rlply(nsims, rank.season(games, preseason.rank(teams)), .progress="text"))

# parallel method
sfInit(parallel = TRUE, cpus = ncpu, type = "SOCK")
sfExportAll() #export appropriate objects that will be needed inside a function, if applicable
sfSource(file="functions.r")
sfLibrary(snowfall)
sfLibrary(plyr)
sfLibrary(dplyr)
sfLibrary(tidyr)
system.time(ranks <- sfSapply(1:nsims, function(x) rank.season(games, preseason.rank(teams)), simplify = FALSE))
sfStop()


# collapse simulated rankings into one data frame
ranks.flat <- rbind.fill(ranks) %>%
  tbl_df()
ranks.flat


# store all simulated output and a separate image for just ranks and teams
save.image(file="output//CFB-sim.RData")

ranks <- ranks.flat %>%
  select(team_id, rank, week)
save(ranks, teams, file="output//CFB-2014.RData")












