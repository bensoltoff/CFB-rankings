## graph.r
## 11/19/14 BCS
## Graph rankings of CFB rankings

rm(list=ls())


require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)

set.seed(11091987)

load(file="output//CFB-2014.RData")


# drop FCS teams
ranks <- ranks %>%
  left_join(teams) %>%
  filter(conference!="FCS")
teams <- teams %>%
  filter(conference!="FCS")

# calculate rank summaries by week
ranks.week <- ranks %>%
  group_by(team_id, week) %>%
  summarize(mean=mean(rank), sd=sd(rank), min=min(rank), med=median(rank), max=max(rank)) %>%
  group_by(week) %>%
  mutate(rank=rank(mean)) %>%
  left_join(teams) %>%
  group_by()


# top 25 for most recent week
ranks.week %>%
  filter(week==max(week)) %>%
  arrange(rank) %>%
  slice(1:25) %>%
  select(rank, school:nickname, mean:max)


# plot rank of teams by conference over the season
max.week <- max(ranks.week$week)    # what is the most recent week available

week.plot <- ggplot(data=ranks.week, aes(week, rank, color=factor(school))) +
  geom_line(size=1) +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks=0:max.week) +
  labs(title="Average Rank Based Upon Simulated Preseason Rankings",
     x="Week of Season",
     y="Average Rank",
     color="School")
week.plot

## separate plot for each conference
for(conf in sort(unique(teams$conference))){
  conf.plot <- week.plot %+% filter(ranks.week, conference==conf) +
    ggtitle(bquote(atop("Average Rank Based Upon Simulated Preseason Rankings", atop(italic(.(conf)), ""))))
  ggsave(filename=paste("graphics//conf_", conf, ".pdf", sep=""), conf.plot)
}


# plot average rank of each conference over the season
conf.plot <- ranks %>%
  group_by(conference, week) %>%
  summarize(mean=mean(rank), sd=sd(rank), min=min(rank), med=median(rank), max=max(rank)) %>%
  ggplot(aes(week, mean, color=factor(conference))) +
  geom_line(size=2) +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks=0:max.week) +
  scale_color_brewer(type="qual", palette="Set3") +
  labs(title="Average Conference Rank Based Upon Simulated Preseason Rankings",
       x="Week of Season",
       y="Average (Raw) Rank",
       color="Conference")
conf.plot
ggsave(filename="graphics//conf_mean.pdf", conf.plot)





