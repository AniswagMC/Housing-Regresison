library(tidytuesdayR)
library(dplyr)
rawdata <- tidytuesdayR::tt_load('2021-09-14')

audio <- rawdata$audio_features
billboard <- rawdata$billboard
billboard <- select(billboard, -c("song", "performer"))

joined <- merge(billboard, audio, by="song_id")
n <- nrow(joined)
n_half <- floor(n/2)
first_half <- joined[1:n_half,]
second_half <- joined[(n_half + 1):n,]
write.csv(first_half, "joined_billboard_audiofeatures_half_1.csv")
write.csv(second_half, "joined_billboard_audiofeatures_half_2.csv")