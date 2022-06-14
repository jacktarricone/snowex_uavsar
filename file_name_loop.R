# file rename, not 2022!

list <-list.files("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/raw_data",full.names = TRUE)
print(list)


for (i in list) {
  file <- basename(i)
  cleaned <- gsub("*.cs_v", ".csv", file)
  fname <- paste0(dirname(i), "/", cleaned)
  file.rename(i, fname)
}
