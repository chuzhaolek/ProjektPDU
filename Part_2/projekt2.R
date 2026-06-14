library(data.table)
library(ggplot2)
#Wczytujemy pliki 
dane_2018_01 <- fread("JC-201801-citibike-tripdata.csv.zip")
dane_2018_02 <- fread("JC-201802-citibike-tripdata.csv.zip")
dane_2018_03 <- fread("JC-201803-citibike-tripdata.csv.zip")

#Łączymy w jedną tabelę
stare_dane <- rbindlist(list(dane_2018_01, dane_2018_02, dane_2018_03))

stare_dane[, wiek := 2018 - as.numeric(`birth year`)]

# Odfiltrowujemy nierealistyczny wiek
stare_dane <- stare_dane[wiek <= 100 & !is.na(wiek)]

# Podgląd wyniku
View(stare_dane)
#problem 1: Zależność godziny od płci
#Czy kobiety rzadziej pożyczą rowery w godzinach wieczornych?
#Czy mężczyżni częściej korzystają z rowerów w godzinach szczytu
stare_dane[, godzina := data.table::hour(as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S"))]
dane_plec <- stare_dane[gender %in% c(1, 2)]
dane_plec[, plec := fcase(
  gender == 1, "Mężczyzna",
  gender == 2, "Kobieta"
)]
#dane_plec to nowa ramka z płcią

stat_godziny <- dane_plec[, .(liczba = .N), by = .(plec, godzina)]

#Liczymy procent przejazdów w obrębie danej płci
stat_godziny[, procent_grupy := (liczba / sum(liczba)) * 100, by = plec]

# Sortujemy dla porządku
setorder(stat_godziny, plec, godzina)
View(stat_godziny)

ggplot(stat_godziny, aes(x = godzina, y = procent_grupy, color = plec)) +
  # Szare tło dla godzin nocnych/wieczornych (20:00 do 24:00 oraz 0:00 do 4:00)
  annotate("rect", xmin = 20, xmax = 24, ymin = 0, ymax = Inf, alpha = 0.15, fill = "gray30") +
  annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = Inf, alpha = 0.15, fill = "gray30") +
  
  # Linie i punkty dla naszych danych
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  scale_color_manual(values = c("Mężczyzna" = "#2c7fb8", "Kobieta" = "#f03b20")) +
  
  scale_x_continuous(breaks = 0:23) +
  
  # Etykiety i tytuły
  labs(
    title = "O jakiej porze wypożyczamy rowery?",
    subtitle = "Znormalizowany rozkład jazdy w ciągu doby z podziałem na płeć (2018). \nSzarym tłem oznaczono pory wieczorne i nocne.",
    x = "Godzina (0-23)",
    y = "Procent dziennych przejazdów w danej grupie (%)",
    color = "Płeć"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor.x = element_blank() # Usuwa zbędne pionowe linie między godzinami
  )
#Wniosek1: Mężczyżni częściej wypożyczą rowery w godzinach szczytu(8-9 oraz 16-17)
#Wniosek2: Kobiety częściej wypożyczą rowery w godzinach 10-15
#W godzinach wieczornych i nocnych nie ma widocznej różnicy
#Pamiętajmy, że są to miesiące zimy - w nocy jest chłodno :) - będziemy badali inne miesiące
#W NJ zazwyczaj śpią po ciężkim dniu w Nowym Jorku w Manhatannie :)

#Problem 2: PageRank stacji rowerowych
library(data.table)

# 1. Wyciągamy trasy (pamiętając o backtickach wokół nazw ze spacjami!)
trasy <- stare_dane[!is.na(`start station name`) & !is.na(`end station name`), 
                    .(start = `start station name`, end = `end station name`)]

# 2. Zbieramy wszystkie stacje do jednego wektora
wszystkie_stacje_wektor <- c(trasy$start, trasy$end)

# Tworzymy listę wszystkich stacji w systemie, omijając gotowe funkcje deduplikujące
wszystkie_stacje <- names(table(wszystkie_stacje_wektor))

# 3. Wymuszamy na R, aby obie kolumny korzystały z tej samej bazy stacji (wymiary macierzy)
trasy$start <- factor(trasy$start, levels = wszystkie_stacje)
trasy$end <- factor(trasy$end, levels = wszystkie_stacje)

# 4. Tworzymy macierz sąsiedztwa i konwertujemy ją na czystą macierz
A <- table(trasy$end, trasy$start)
A <- as.matrix(A)

# 5. Normalizacja kolumn (aby sumowały się do 1, tworząc macierz przejścia)
col_sums <- colSums(A)

# Zabezpieczenie przed dzieleniem przez zero (dla stacji bez wyjazdów)
col_sums[col_sums == 0] <- 1 

# Dzielimy każdą kolumnę przez jej sumę
M <- sweep(A, 2, col_sums, FUN = "/")

# 6. Algorytm PageRank (Metoda Potęgowa)
d <- 0.85                   # Współczynnik tłumienia
N <- nrow(M)                # Liczba wszystkich stacji w systemie
v <- rep(1/N, N)            # Inicjalizacja wektora początkowego

max_iter <- 100
for(i in 1:max_iter) {
  
  # Główny wzór algebry liniowej: v = d * M * v + (1-d)/N
  v_next <- d * (M %*% v) + (1 - d) / N
  
  # Warunek stopu (zbieżność)
  if(sum(abs(v_next - v)) < 1e-7) {
    cat("Algorytm zbiegł się po", i, "iteracjach!\n")
    v <- v_next
    break
  }
  
  v <- v_next 
}

# 7. Pakujemy wynikowy wektor do przejrzystej tabeli data.table
wyniki_pagerank <- data.table(
  stacja = rownames(M),
  wynik_pagerank = as.numeric(v)
)

# Sortujemy tabelę, by najważniejsze stacje znalazły się na górze
setorder(wyniki_pagerank, -wynik_pagerank)

# Podglądamy ostateczny wynik - ranking stacji!
head(wyniki_pagerank, 10)

library(data.table)

# 1. Wyciągamy stację początkową i godzinę wyjazdu
dane_svd <- stare_dane[!is.na(`start station name`), 
                       .(stacja = `start station name`, 
                         godzina = data.table::hour(as.POSIXct(starttime)))]

# 2. Zliczamy wyjazdy dla każdej stacji w każdej z 24 godzin
ruchy_godzinowe <- dane_svd[, .(liczba = .N), by = .(stacja, godzina)]

# 3. Zmieniamy format na szeroki - stacje jako wiersze, godziny jako kolumny (0-23)
macierz_df <- dcast(ruchy_godzinowe, stacja ~ godzina, value.var = "liczba", fill = 0)

# 4. Normalizacja (Kluczowe przed SVD!)
#nam nie obchodzi wielkość ale jakość
stacje_nazwy <- macierz_df$stacja
X <- as.matrix(macierz_df[, -1, with = FALSE]) # Usuwamy pierwszą kolumnę z nazwami
X <- sweep(X, 1, rowSums(X), "/")              # Dzielimy wiersze przez ich sumy

# Usuwamy stacje, z których w ogóle nikt nie jeździł 
valid_rows <- !is.na(rowSums(X))
X <- X[valid_rows, ]
stacje_nazwy <- stacje_nazwy[valid_rows]

# 5. Centrowanie kolumn (wymóg matematyczny, by SVD działało jak PCA)
X_centered <- scale(X, center = TRUE, scale = FALSE)


rozkład_svd <- svd(X_centered)

U <- rozkład_svd$u       # Macierz profili stacji
D <- diag(rozkład_svd$d) # Macierz diagonalna wartości osobliwych (Sigma)
Vt <- t(rozkład_svd$v)   # Macierz "wzorców idealnych" ruchu

library(ggplot2)

wzorce <- data.frame(
  Godzina = 0:23,
  Wzorzec_1 = Vt[1, ], # Pierwszy wektor własny
  Wzorzec_2 = Vt[2, ]  # Drugi wektor własny
)

ggplot(wzorce, aes(x = Godzina)) +
  geom_line(aes(y = Wzorzec_1, color = "Składowa Główna 1"), size = 1.2) +
  geom_line(aes(y = Wzorzec_2, color = "Składowa Główna 2"), size = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = " Główne wzorce czasowe znalezione przez SVD",
       x = "Godzina", y = "Wartość wektora bazowego") +
  theme_minimal()

stacje_w_przestrzeni_svd <- data.frame(
  Stacja = stacje_nazwy,
  Składowa_1 = U[, 1], # Jak bardzo stacja pasuje do wzorca 1
  Składowa_2 = U[, 2]  # Jak bardzo stacja pasuje do wzorca 2
)

ggplot(stacje_w_przestrzeni_svd, aes(x = Składowa_1, y = Składowa_2)) +
  geom_point(alpha = 0.6, color = "purple") +
  # Dodajemy etykiety tylko dla kilku najbardziej skrajnych stacji
  geom_text(data = subset(stacje_w_przestrzeni_svd, abs(Składowa_1) > 0.1 | abs(Składowa_2) > 0.1),
            aes(label = Stacja), vjust = 1.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Mapa profili stacji rowerowych",
       subtitle = "Rzutowanie wektorów 24-wymiarowych na 2D za pomocą SVD",
       x = "Profil poranny vs wieczorny (Składowa 1)",
       y = "Profil rekreacyjny vs dojazdowy (Składowa 2)") +
  theme_minimal()