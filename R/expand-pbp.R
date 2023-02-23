
library(tidyverse)

# join off strength
pbp = {
  pbp %>%
    left_join(OffStrength, by = "OffensiveTeam")
}

# create play id
play_id = {
  pbp %>%
    group_by(GameID, EventID) %>%
    summarise(Players = n())
}
play_id$ID = 1:nrow(play_id)

# join in id and create formation
pbp = {
  pbp %>%
    left_join(play_id, by = c("GameID", "EventID")) %>%
    unite(col = "ExactFormation", c("DL", "LB"), sep = "-") %>%
    mutate(Formation = NA)
}

for(i in 1:nrow(pbp)) {
  if(pbp$ExactFormation[i] %in% c("4-3", "4-2")) {
    pbp$Formation[i] = "4-3"
  } else if(pbp$ExactFormation[i] %in% c("3-4", "3-3", "2-4")) {
    pbp$Formation[i] = "3-4"
  } else {
    pbp$Formation[i] = "0-0"
  }
}

# add in defender strengths
defenders[is.na(defenders)] = 0

def_min = {
  defenders %>%
    select(PlayerId, RunDefTRUE, PassRushTRUE, PassCovTRUE)
}

pbp = {
  pbp %>%
    left_join(def_min, by = "PlayerId") %>%
    mutate(DefenderStrength = ifelse(EventType == "rush", RunDefTRUE,
                                     ifelse(IsRushing == 1, PassRushTRUE,
                                     PassCovTRUE)),
           BlockerStrength = ifelse(EventType == "rush", RunBlockStrength,
                                    PassBlockStrength),
           IsPass = ifelse(EventType == "pass", 1, 0),
           IsRush = ifelse(EventType == "rush", 1, 0))
}
pbp$Scramble[is.na(pbp$Scramble)] = 0
pbp$Spike[is.na(pbp$Spike)] = 0
pbp$PressureOnPlay[is.na(pbp$PressureOnPlay)] = 0
pbp$SackOnPlay[is.na(pbp$SackOnPlay)] = 0

pressure_model = {
  pbp %>%
    mutate(TotalToGo = ifelse(SideOfField == "Own", 100 - StartYard,
                              StartYard)) %>%
    filter(Scramble != 1,
           Spike != 1,
           StartYard > 20,
           EventType == "pass",
           DropType %in% c("0/1 Step", "3 Step", "5 Step", "7 Step")) %>%
    group_by(ID) %>%
    summarise(Pressure = max(PressureOnPlay), Sack = max(SackOnPlay),
              SnapToThrowTime = max(SnapToThrowTime),
              SnapToSackTime = max(SnapToSackTime),
              Down = max(Down), ToGo = max(ToGo),
              TotalToGo = max(TotalToGo), Formation = max(Formation),
              EPA = max(EPA),
              PasserStrength = max(PasserStrength),
              BlockerStrength = max(BlockerStrength),
              DefenderStrength = mean(DefenderStrength, na.rm = TRUE))
}

for(i in 1:nrow(pressure_model)) {
  if(is.na(pressure_model$SnapToThrowTime[i])) {
    pressure_model$SnapToThrowTime[i] = pressure_model$SnapToSackTime[i]
  }
}

pressure_model = {
  pressure_model %>%
    select(-SnapToSackTime) %>%
    drop_na()
}

write_csv(pressure_model, "data/pressure-model.csv")


rush_model = {
  pbp %>%
    mutate(TotalToGo = ifelse(SideOfField == "Own", 100 - StartYard,
                              StartYard)) %>%
    filter(Scramble != 1,
           StartYard > 20,
           EventType == "rush") %>%
    group_by(ID) %>%
    summarise(OffensiveYardarge = max(OffensiveYardage),
              RB = max(RB), TE = max(TE),
              Down = max(Down), ToGo = max(ToGo),
              TotalToGo = max(TotalToGo), Formation = max(Formation),
              EPA = max(EPA),
              RusherStrength = max(PasserStrength),
              BlockerStrength = max(BlockerStrength),
              DefenderStrength = mean(DefenderStrength, na.rm = TRUE))
}

scramble_model = {
  pbp %>%
    mutate(TotalToGo = ifelse(SideOfField == "Own", 100 - StartYard,
                              StartYard)) %>%
    filter(Scramble == 1,
           StartYard > 20) %>%
    group_by(ID) %>%
    summarise(OffensiveYardarge = max(OffensiveYardage),
              Down = max(Down), ToGo = max(ToGo),
              TotalToGo = max(TotalToGo), Formation = max(Formation),
              EPA = max(EPA),
              ScramblerStrength = max(PasserStrength),
              BlockerStrength = max(BlockerStrength),
              DefenderStrength = mean(DefenderStrength, na.rm = TRUE))
}

write_csv(rush_model, "data/rush-model.csv")
write_csv(scramble_model, "data/scramble-model.csv")
