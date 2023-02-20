
library(tidyverse)

data = read_csv("data/pbp.csv")

pbp1 = data[1:(145284 * .25),]
pbp2 = data[((145284 * .25)+1):(145284 * .50),]
pbp3 = data[((145284 * .50)+1):(145284 * .75),]
pbp4 = data[((145284 * .75)+1):145284,]

data2 = rbind(pbp1, pbp2, pbp3, pbp4)

sum((data == data2) == FALSE)

write_csv(pbp1, "data/pbp1.csv")
write_csv(pbp2, "data/pbp2.csv")
write_csv(pbp3, "data/pbp3.csv")
write_csv(pbp4, "data/pbp4.csv")
