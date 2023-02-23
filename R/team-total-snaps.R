
# team snap totals

# TODO: load in team blocking, passing, rushing ratings
# TODO: find each team's total passing and rushing plays from pbp data
# TODO: join together to find per snap ratings


# side note we will look at scrambles separately

library(tidyverse)



Offense_pass = pbp %>% 
  select(OffensiveTeam, EventType) %>%
  filter(EventType == "pass") %>%
  group_by(OffensiveTeam) %>%
  summarize(TotalPasses = n())

Offense_run = pbp %>% 
  select(OffensiveTeam, EventType) %>%
  filter(EventType == "rush") %>%
  group_by(OffensiveTeam) %>%
  summarize(TotalRush = n())



OffensiveEvents = cbind(Offense_pass, Offense_run, passers, blockers, rushers)
OffensiveEvents = OffensiveEvents[!duplicated(as.list(OffensiveEvents))]


Offense_freq = OffensiveEvents %>% 
  mutate(Passer_snap = Passer/TotalPasses,
         RunBlock_snap = RunBlock/TotalRush,
         RunPass_snap = PassBlock/TotalPasses,
         Rusher_snap = Rusher/TotalRush)
