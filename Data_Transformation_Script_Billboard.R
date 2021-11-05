library(tidytuesdayR)
library(dplyr)
library(tidyr)
rawdata <- tidytuesdayR::tt_load('2021-09-14')

audio <- rawdata$audio_features
billboard <- rawdata$billboard

unique_songs <- unique(billboard$song_id)
empty_matrix <- matrix(NA, nrow = length(unique_songs), ncol = ncol(billboard))
billboard_singles <- as.data.frame(empty_matrix)
colnames(billboard_singles) <- names(billboard)
for(i in 1:length(unique_songs)){
  versions <- filter(billboard, song_id == unique_songs[i])
  min_peak_idx <- which.min(versions$peak_position)
  billboard_singles[i,] <- versions[min_peak_idx,]
}


billboard_filtered <- select(billboard_singles, -c("song", "performer"))
joined <- merge(billboard_filtered, audio, by="song_id")
joined <- joined %>% select(-c("song_id", "url", "instance", "week_id", 
                                "spotify_track_id", "spotify_track_preview_url", 
                                "week_position", "previous_week_position"))

joined <- joined %>% drop_na()
write.csv(joined, "joined_billboard_audiofeature.csv")
set.seed(406)
n <- nrow(joined)
training_idxs <- sample.int(n, floor(n * .80), FALSE)
training_set <- joined[training_idxs,]
test_set <- joined[-training_idxs,]
write.csv(training_set, "training.csv")
write.csv(test_set, "test.csv")


