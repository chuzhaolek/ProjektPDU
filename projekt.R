library(readr)
library(data.table)
install.packages("geosphere")
library(geosphere)
library(ggplot2)
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
dane[, dystans_km := round(dystans_metry / 1000, 2)]
dane[, started_at := as.POSIXct(started_at, format="%Y-%m-%d %H:%M:%S")]
dane[, ended_at := as.POSIXct(ended_at, format="%Y-%m-%d %H:%M:%S")]

dane[, czas_godziny := as.numeric(difftime(ended_at, started_at, units = "hours"))]
dane[, czas_jazdy_minuty := round(as.numeric(difftime(ended_at, started_at, units = "mins")), 2)]
# 3. Obliczenie średniej prędkości (km/h) 
dane[czas_godziny > 0, predkosc_kmh := round(dystans_km / czas_godziny, 2)]

dane[, data_jazdy := as.Date(started_at)]

# 2. Tworzymy nową tabelkę z agregacją danych (grupujemy po 'data_jazdy')
dzienne_statystyki <- dane[, .(
  suma_km = sum(dystans_km, na.rm = TRUE),   
  liczba_przejazdow = .N                    
), by = data_jazdy]

# 3. Sortujemy tabelę chronologicznie
setorder(dzienne_statystyki, -suma_km)

# Wyświetlenie pierwszych kilku wierszy, żeby sprawdzić wynik
head(dzienne_statystyki)

setorder(dzienne_statystyki, suma_km)
head(dzienne_statystyki)

pogoda <- fread("pogoda.csv")
#Przerobienie kolumny z datą, aby nazywała się i wyglądała tak samo jak w naszych danych rowerowych
pogoda[, data_jazdy := as.Date(datetime)]

#Wybór najbardziej przydatnych kolumn do analizy
pogoda_wybrane <- pogoda[, .(data_jazdy, temp, precip, snow, windspeed, conditions)]

#Niestety Amerykanie mają inny system miary
pogoda_wybrane[, temp_C := round((temp - 32) * 5 / 9, 1)]
dane_polaczone <- merge(dzienne_statystyki, pogoda_wybrane, by = "data_jazdy", all.x = TRUE)

# Sprawdźmy jak to wygląda:
View(dane_polaczone)


ggplot(dane_polaczone, aes(x = temp_C, y = liczba_przejazdow)) +
  # Dodajemy punkty, a ich kolor zależeć będzie od ogólnej pogody (np. deszcz, słońce)
  geom_point(aes(color = conditions), size = 3, alpha = 0.7) +
  # Dodajemy linię trendu, która udowodni korelację (im cieplej, tym więcej rowerów)
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  # Upiększamy wykres (tytuły, osie)
  labs(
    title = "Wpływ temperatury na popularność rowerów miejskich (Q1 2026)",
    subtitle = "Zestawienie dziennej liczby przejazdów z danymi pogodowymi dla Jersey City",
    x = "Średnia temperatura (°C)",
    y = "Dzienna liczba przejazdów",
    color = "Warunki pogodowe"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggplot


