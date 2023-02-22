
# data loader

library(tidyverse)

pbp_list = list()

for(i in 1:4) {
  pbp_list[[i]] = read_csv(paste0("data/pbp",as.character(i),".csv"))
}

pbp = bind_rows(pbp_list, .id = "column_label")

total_points = read_csv("data/total_points.csv")
active_contacts = read_csv("data/active_contracts.csv")

# to numeric

pbp$Scramble <- as.numeric(pbp$Scramble)
pbp$RunDirection <- as.numeric(pbp$RunDirection)
pbp$UsedDesignedGap <- as.numeric(pbp$UsedDesignedGap)
pbp$Attempt <- as.numeric(pbp$Attempt)
pbp$Completion <- as.numeric(pbp$Completion)
pbp$Spike <- as.numeric(pbp$Spike)
pbp$ThrowDepth <- as.numeric(pbp$ThrowDepth)
pbp$PressureOnPlay <- as.numeric(pbp$PressureOnPlay)
pbp$SackOnPlay <- as.numeric(pbp$SackOnPlay)
pbp$PassBreakupOnPlay <- as.numeric(pbp$PassBreakupOnPlay)
pbp$InterceptionOnPlay <- as.numeric(pbp$InterceptionOnPlay)
pbp$FumbleByPasser <- as.numeric(pbp$FumbleByPasser)
pbp$FumbleByRusher <- as.numeric(pbp$FumbleByRusher)
pbp$FumbleByReceiver <- as.numeric(pbp$FumbleByReceiver)
pbp$SnapToThrowTime <- as.numeric(pbp$SnapToThrowTime)
pbp$SnapToPressureTime <- as.numeric(pbp$SnapToPressureTime)
pbp$SnapToPressurePlayerId <- as.numeric(pbp$SnapToPressurePlayerId)
pbp$SnapToSackTime <- as.numeric(pbp$SnapToSackTime)
pbp$SnapToSackPlayerId <- as.numeric(pbp$SnapToSackPlayerId)
pbp$SnapToScrambleTime <- as.numeric(pbp$SnapToScrambleTime)

total_points$TotalPoints <- as.numeric(total_points$TotalPoints)
total_points$PasserPoints <- as.numeric(total_points$PasserPoints)
total_points$RusherPoints <- as.numeric(total_points$RusherPoints)
total_points$ReceiverPoints <- as.numeric(total_points$ReceiverPoints)
total_points$RunBlockPoints <- as.numeric(total_points$RunBlockPoints)
total_points$PassBlockPoints <- as.numeric(total_points$PassBlockPoints)
total_points$PassRushPoints <- as.numeric(total_points$PassRushPoints)
total_points$PassCovPoints <- as.numeric(total_points$PassCovPoints)
total_points$RunDefPoints <- as.numeric(total_points$RunDefPoints)



