
library(tidyverse)

source("R/shrink.R")

# break up by position

blockers = {
  total_points %>%
    filter(RosterPosition %in% c("T", "G", "TE", "C", "FB")) %>%
    select(PlayerId, Player, Team, RosterPosition, RunBlockSnaps, RunBlockPoints,
           PassBlockSnaps, PassBlockPoints) %>%
    mutate(RunBlockPPS = RunBlockPoints /RunBlockSnaps,
           PassBlockPPS = PassBlockPoints / PassBlockSnaps,
           RunBlockTRUE = shrink(RunBlockSnaps, RunBlockPPS, 0, 5000) * RunBlockSnaps,
           PassBlockTRUE = shrink(PassBlockSnaps, PassBlockPPS, 0, 5000) * PassBlockSnaps) %>%
    group_by(Team) %>%
    summarise(RunBlock = sum(RunBlockTRUE, na.rm = TRUE),
              PassBlock = sum(PassBlockTRUE, na.rm = TRUE))

}

passers = {
  total_points %>%
    filter(RosterPosition == "QB") %>%
    select(PlayerId, Player, Team, RosterPosition, PasserSnaps, PasserPoints) %>%
    mutate(PasserPPS = PasserPoints / PasserSnaps,
           PasserTRUE = shrink(PasserSnaps, PasserPPS, 0, 5000) * PasserSnaps) %>%
    group_by(Team) %>%
    summarise(Passer = sum(PasserTRUE, na.rm = TRUE))

}

rushers = {
  total_points %>%
    filter(RosterPosition %in% c("RB", "FB", "WR", "TE", "QB")) %>%
    select(PlayerId, Player, Team, RosterPosition, RusherSnaps, RusherPoints) %>%
    mutate(RusherPPS = RusherPoints / RusherSnaps,
           RusherTRUE = shrink(RusherSnaps, RusherPPS, 0, 5000) * RusherSnaps) %>%
    group_by(Team) %>%
    summarise(Rusher = sum(RusherTRUE, na.rm = TRUE))

}

defenders = {
  total_points %>%
    filter(RosterPosition %in% c("LB", "DT", "DE", "CB", "S")) %>%
    select(PlayerId, Player, RosterPosition, RunDefSnaps, RunDefPoints,
           PassRushSnaps, PassRushPoints, PassCovSnaps, PassCovPoints) %>%
    mutate(RunDefPPS = RunDefPoints /RunDefSnaps,
           PassRushPPS = PassRushPoints / PassRushSnaps,
           PassCovPPS = PassCovPoints / PassCovSnaps,
           RunDefTRUE = shrink(RunDefSnaps, RunDefPPS, 0, 5000),
           PassRushTRUE = shrink(PassRushSnaps, PassRushPPS, 0, 5000),
           PassCovTRUE = shrink(PassCovSnaps, PassCovPPS, 0, 5000))
}

write_csv(passers, "data/passer-ratings.csv")
write_csv(rushers, "data/rusher-ratings.csv")
write_csv(blockers, "data/blocker-ratings.csv")
write_csv(defenders, "data/defender-ratings.csv")
