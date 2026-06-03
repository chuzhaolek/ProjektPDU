library(readr)
library(data.table)
install.packages("geosphere")
library(geosphere)
dane1 <- read_csv("JC-202601-citibike-tripdata.zip")
dane2 <- read_csv("JC-202602-citibike-tripdata.csv.zip")
dane3 <- read_csv("JC-202603-citibike-tripdata.csv.zip")
dane <- rbindlist(list(dane1, dane2, dane3))
View(dane)
# 3. Usunięcie wierszy, w których brakuje współrzędnych (ważne, by funkcja nie wyrzuciła błędu!)
dane <- dane[!is.na(start_lat) & !is.na(start_lng) & !is.na(end_lat) & !is.na(end_lng)]

# 4. Tworzenie nowej kolumny z dystansem
# Funkcja distHaversine przyjmuje współrzędne w kolejności: długość (lng), szerokość (lat)
dane[, dystans_metry := distHaversine(
  matrix(c(start_lng, start_lat), ncol = 2),
  matrix(c(end_lng, end_lat), ncol = 2)
)]
dane[, dystans_metry := round(dystans_metry, 2)]
View(dane)