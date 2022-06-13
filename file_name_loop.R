# file rename, not 2022!

list <-list.files("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/raw_data",full.names = TRUE)
print(list)


for (i in list) {
  id <- basename(i)
  id_2 <- gsub("_2022", "_2020", id)
  fname <- paste0(dirname(i), "/", id_2, ".csv")
  file.rename(i, fname)
}
