# Pakiety 
if(!require("manipulate")) {install.packages("manipulate"); library(manipulate)}
if(!require("mvtnorm")) {install.packages("mvtnorm"); library(mvtnorm)}

# Katalog roboczy
setwd("C:/Users/antek/OneDrive - SGH/1. SGH magisterka/SEM II/Ekonometria bayesowska/projekt")

# Czyszczenie przestrzeni roboczej, wykresów i konsoli
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

# ==============================================================================
# 1. PRZYGOTOWANIE DANYCH I EDA
# ==============================================================================
dane <- read.csv("e8_dane.csv", header = TRUE) 

# Transformacja zmiennych (zarobki w tys. PLN)
dane$wynagrodzenie_median_tys <- dane$wynagrodzenie_median / 1000

# Centrowanie zmiennych
srednie_zarobki_tys <- mean(dane$wynagrodzenie_median_tys, na.rm = TRUE)
srednia_klasa <- mean(dane$klasa_avg, na.rm = TRUE)

dane$wynagrodzenie_median_tys_cent <- dane$wynagrodzenie_median_tys - srednie_zarobki_tys
dane$klasa_avg_cent <- dane$klasa_avg - srednia_klasa

# Obliczenie zmiennej interakcyjnej
dane$interakcja <- dane$klasa_avg_cent * dane$wynagrodzenie_median_tys_cent

# Aktualizacja wektorów do analizy
zmienne_do_analizy <- c("wynik_avg", "klasa_avg", "klasa_avg_cent", "wynagrodzenie_median_tys", 
                        "wynagrodzenie_median_tys_cent", "interakcja")

etykiety_wykresow <- c(
  wynik_avg = "Wynik z egzaminu (pkt)",
  klasa_avg = "Średnia wielkość klasy (osoby)",
  klasa_avg_cent = "Wielkość klasy wycentrowana",
  wynagrodzenie_median_tys = "Mediana wynagrodzeń (tys. PLN)",
  wynagrodzenie_median_tys_cent = "Wynagrodzenia wycentrowane",
  interakcja = "Zmienna interakcyjna"
)

# 1. Generowanie tabeli statystyk opisowych
statystyki_opisowe <- data.frame(
  Zmienna = zmienne_do_analizy,
  Srednia = sapply(dane[zmienne_do_analizy], mean, na.rm = TRUE),
  Odch_Std = sapply(dane[zmienne_do_analizy], sd, na.rm = TRUE),
  Minimum = sapply(dane[zmienne_do_analizy], min, na.rm = TRUE),
  Maksimum = sapply(dane[zmienne_do_analizy], max, na.rm = TRUE)
)

statystyki_opisowe[,-1] <- round(statystyki_opisowe[,-1], 2)

cat("\n--- STATYSTYKI OPISOWE ZMIENNYCH ---\n")
print(statystyki_opisowe)

# 2. Generowanie i eksport histogramów
katalog_wykresy <- "C:/Users/antek/OneDrive - SGH/1. SGH magisterka/SEM II/Ekonometria bayesowska/projekt/figures"
if (!dir.exists(katalog_wykresy)) {dir.create(katalog_wykresy, recursive = TRUE)}

for (zmienna in zmienne_do_analizy) {
  sciezka_pliku <- file.path(katalog_wykresy, paste0("hist_", zmienna, ".png"))
  png(filename = sciezka_pliku, width = 800, height = 600, res = 120)
  
  hist(dane[[zmienna]], 
       main = paste("Rozkład zmiennej:", etykiety_wykresow[zmienna]), 
       xlab = etykiety_wykresow[zmienna], 
       ylab = "Częstość", 
       col = "#2A728A",       
       border = "white",      
       breaks = "Sturges")    
  
  dev.off()
}

cat(sprintf("\nZapisano %d histogramów w folderze:\n%s\n\n", length(zmienne_do_analizy), katalog_wykresy))

# Definicja macierzy danych
y <- as.matrix(dane$wynik_avg)
N.data <- length(y)

# Macierz X: stała, klasa_avg_cent, wynagrodzenie_median_tys_cent, interakcja
X <- cbind(rep(1, N.data), 
           dane$klasa_avg_cent, 
           dane$wynagrodzenie_median_tys_cent, 
           dane$interakcja)
colnames(X) <- c("stala", "klasa_avg_cent", "zarobki_cent", "interakcja")

# Regresja klasyczna (OLS)
OLS_results <- lm(wynik_avg ~ klasa_avg_cent + wynagrodzenie_median_tys_cent + interakcja, data = dane)
summary(OLS_results)

# Zapisanie wyników z próby empirycznej
Beta.ols.data <- as.matrix(OLS_results$coefficients)
v.data <- OLS_results$df.residual
XTX.data <- t(X) %*% X
s2.data <- sum((OLS_results$residuals)^2) / v.data


# ==============================================================================
# 2. PARAMETRY A PRIORI I ELICYTACJA
# ==============================================================================
k <- ncol(X)

# E(stała)           = 60
# E(beta_klasa)      = -0.19
# E(beta_zarobki)    = 6.11
# E(beta_interakcja) = 0.00
Beta.prior <- c(60, -0.19, 6.11, 0.00) 

# Założona a priori wariancja błędu resztowego
s2.prior <- 400            
v.prior <- 10 

# Mnożnik korekcyjny precyzji wprost wynikający z właściwości brzegowego rozkładu t-Studenta
mnoznik_df <- (v.prior - 2) / v.prior

U.prior <- diag(k)       
# Stała: wysoka dyspersja (błąd st. = 100 pkt, Var = 10000).
U.prior[1, 1] <- mnoznik_df * (10000 / s2.prior) 
# Klasa: błąd standardowy z literatury wynosi 0.10 p.p.
U.prior[2, 2] <- mnoznik_df * (0.10^2) / s2.prior  
# Zarobki: proporcja wariancji wyznacza błąd na poziomie 2.3 p.p.
U.prior[3, 3] <- mnoznik_df * (2.3^2) / s2.prior  
# Interakcja: wyższa wariancja warunkowa dla a priori sceptycznego
U.prior[4, 4] <- mnoznik_df * (0.10^2) / s2.prior  
# Oczekiwana suma kwadratów reszt w fikcyjnej próbie
vs2.prior <- v.prior * s2.prior

# ==============================================================================
# 3. Parametry a posteriori i eksport wykresów brzegowych
# ==============================================================================

# Obliczenia parametrów
Beta.posterior <- solve(solve(U.prior) + XTX.data) %*% (solve(U.prior) %*% Beta.prior + XTX.data %*% Beta.ols.data)
U.posterior    <- solve(solve(U.prior) + XTX.data)
v.posterior    <- v.prior + N.data
vs2.posterior  <- v.prior * s2.prior + v.data * s2.data + t(Beta.ols.data - Beta.prior) %*% solve(U.prior + solve(XTX.data)) %*% (Beta.ols.data - Beta.prior)
s2.posterior   <- as.numeric(vs2.posterior / v.posterior)

cat("\n==============================================\n")
cat("       PARAMETRY ROZKŁADU A POSTERIORI        \n")
cat("==============================================\n")
cat("\n[Beta.posterior] Średnie a posteriori dla parametrów:\n")
print(Beta.posterior)
cat("\n[s2.posterior] Skala (wariancja) błędu a posteriori:\n")
cat(s2.posterior, "\n")
cat("\n[v.posterior] Stopnie swobody a posteriori:\n")
cat(v.posterior, "\n")
cat("\n[U.posterior] Macierz precyzji/skali a posteriori:\n")
print(U.posterior)
cat("----------------------------------------------\n\n")

# Definicja palety kolorystycznej
grey_area  <- rgb(160, 160, 160, 80, maxColorValue = 255)
grey_line  <- rgb(80,  80,  80,  160, maxColorValue = 255)
green_area <- rgb(24,  121, 104, 80, maxColorValue = 255)
green_line <- rgb(13,  85,  72,  160, maxColorValue = 255)
ols_line   <- "black" 

# ZAPIS WYKRESU ROZKŁADÓW BRZEGOWYCH
sciezka_brzegowe <- file.path(katalog_wykresy, "rozklady_brzegowe.png")
png(filename = sciezka_brzegowe, width = 1200, height = 900, res = 130)

par(mfrow = c(2, 2))
n_parameters <- length(Beta.posterior)

for(ii in 1:n_parameters) {
  odch_st_post <- sqrt(U.posterior[ii, ii] * s2.posterior)
  odch_st_prior <- sqrt(U.prior[ii, ii] * s2.prior)
  
  if(ii == 1) {
    zakres <- odch_st_post * 20  
  } else {
    zakres <- max(odch_st_post, odch_st_prior)
  }
  
  min_x <- min(Beta.posterior[ii], Beta.prior[ii], Beta.ols.data[ii])
  max_x <- max(Beta.posterior[ii], Beta.prior[ii], Beta.ols.data[ii])
  
  lokalna_siatka <- seq(from = min_x - 2 * zakres, 
                        to = max_x + 2 * zakres, 
                        length.out = 1000)
  
  gestosc_prior <- dt((lokalna_siatka - Beta.prior[ii]) / sqrt(U.prior[ii, ii] * s2.prior * v.prior / (v.prior-2)), df = v.prior) / sqrt(U.prior[ii, ii] * s2.prior * v.prior / (v.prior-2))
  gestosc_post <- dt((lokalna_siatka - Beta.posterior[ii]) / sqrt(U.posterior[ii, ii] * s2.posterior * v.posterior / (v.posterior-2)), df = v.posterior) / sqrt(U.posterior[ii, ii] * s2.posterior * v.posterior / (v.posterior-2))
  
  title <- ifelse(ii==1, "Stała", colnames(X)[ii])
  plot(lokalna_siatka, gestosc_prior, las = 1, lwd = 2, bty = "n", col = grey_area,
       ylim = c(0, max(c(max(gestosc_prior), max(gestosc_post))) * 1.3),
       type = "l", ylab = "gęstość", xlab = "Wartość parametru", main = title)
  
  polygon(c(lokalna_siatka, rev(lokalna_siatka)), c(gestosc_prior, rep(0, 1000)), col = grey_area, border = NA)
  abline(v = Beta.prior[ii], col = grey_line, lwd = 2, lty = 2) 
  
  lines(lokalna_siatka, gestosc_post, lwd = 2, col = green_line)
  polygon(c(lokalna_siatka, rev(lokalna_siatka)), c(gestosc_post, rep(0, 1000)), col = green_area, border = NA)
  abline(v = Beta.posterior[ii], col = green_line, lwd = 2) 
  
  abline(v = Beta.ols.data[ii], col = ols_line, lwd = 2, lty = 1) 
  
  legend("topright", 
         legend = c("A posteriori", "A priori", "OLS"), 
         col = c(green_line, grey_line, ols_line), 
         lty = c(1, 2, 1), 
         lwd = 2, 
         bty = "n",
         cex = 0.9)
}
dev.off()



# ==============================================================================
# 4a. Znaczenie zmiennych: HPDI
# ==============================================================================

# Definicja kolorów dla obszarów HPDI
red_area <- rgb(255, 100, 123, 80, names = NULL, maxColorValue = 255)
red_line <- rgb(200, 0, 30, 160, names = NULL, maxColorValue = 255)

# Pętla iterująca po wszystkich 4 parametrach macierzy X
for (ii in 1:4) {
  
  # Siatka a posteriori dla badanego parametru
  odch_st_post_m <- sqrt(U.posterior[ii, ii] * s2.posterior)
  siatka_manipulate <- seq(Beta.posterior[ii] - 5 * odch_st_post_m, 
                           Beta.posterior[ii] + 5 * odch_st_post_m, 
                           length.out = 1000)
  
  # Ewaluacja gęstości rozkładu brzegowego
  gestosc_manipulate <- apply(as.matrix(siatka_manipulate), 1, dmvt,
                              delta = Beta.posterior[ii], 
                              sigma = as.matrix(U.posterior[ii, ii] * s2.posterior), 
                              df = v.posterior, 
                              log = FALSE)
  dx <- siatka_manipulate[2] - siatka_manipulate[1]
  
  # Algorytm poszukiwania poziomu odcięcia dla zadanego poziomu ufności (95%)
  target_prob <- 0.95
  sorted_dens <- sort(gestosc_manipulate, decreasing = TRUE)
  cumulative_prob <- cumsum(sorted_dens * dx)
  line_level_static <- sorted_dens[which.min(abs(cumulative_prob - target_prob))]
  
  # Generowanie i zapis statycznego wykresu
  sciezka_hpdi <- file.path(katalog_wykresy, paste0("hpdi_", colnames(X)[ii], ".png"))
  png(filename = sciezka_hpdi, width = 800, height = 600, res = 120)
  
  par(mfrow = c(1, 1))
  credible_set_indicator <- as.vector(as.integer(gestosc_manipulate >= line_level_static))
  
  if(sum(credible_set_indicator) > 0) {
    credible_set_begin <- match(1, credible_set_indicator)
    credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
    x1 <- siatka_manipulate[credible_set_begin]
    x2 <- siatka_manipulate[credible_set_end]
    
    plot(siatka_manipulate, gestosc_manipulate, las = 1, lwd = 2, bty = "n", col = green_line,
         ylim = c(0, max(gestosc_manipulate) * 1.2), type = "l", 
         ylab = "gęstość", xlab = colnames(X)[ii], 
         main = paste("HPDI dla:", colnames(X)[ii]))
    
    polygon(c(siatka_manipulate, rev(siatka_manipulate)), 
            c(gestosc_manipulate, rep(0, length(siatka_manipulate))), 
            col = green_area, border = NA)
    
    posterior.cs <- gestosc_manipulate * credible_set_indicator
    
    polygon(c(siatka_manipulate, rev(siatka_manipulate)), 
            c(posterior.cs, rep(0, length(siatka_manipulate))), 
            col = red_area, border = NA)
    
    abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
    abline(h = line_level_static, col = red_line, lwd = 2)
    
    text(siatka_manipulate[1], max(gestosc_manipulate) + 0.05, 
         paste(target_prob * 100, "% HPDI: (", round(x1, 4), " ; ", round(x2, 4), ")"), 
         col = red_line, pos=4)
  }
  
  dev.off()
}

# =========================================================
# WERSJA INTERAKTYWNA MANIPULATE
# =========================================================
# Wybieramy indeks parametru do analizy: 
# 1: stała, 2: klasa_avg_cent, 3: zarobki_cent, 4: interakcja
ii <- 2  

# Wariancja i odchylenie standardowe z rozkładu a posteriori
odch_st_post_m <- sqrt(U.posterior[ii, ii] * s2.posterior)

# Definicja dziedziny (siatki) wykresu
siatka_manipulate <- seq(Beta.posterior[ii] - 5 * odch_st_post_m, 
                         Beta.posterior[ii] + 5 * odch_st_post_m, 
                         length.out = 1000)
dx <- siatka_manipulate[2] - siatka_manipulate[1]

# Ewaluacja gęstości brzegowej (rozkład t-Studenta) dla punktów siatki
gestosc_manipulate <- dt((siatka_manipulate - Beta.posterior[ii]) / 
                           sqrt(U.posterior[ii, ii] * s2.posterior * v.posterior / (v.posterior-2)), 
                         df = v.posterior) / 
  sqrt(U.posterior[ii, ii] * s2.posterior * v.posterior / (v.posterior-2))

# Przygotowanie posortowanego wektora gęstości do szybkiego wyszukiwania całki
sorted_dens <- sort(gestosc_manipulate, decreasing = TRUE)

manipulate(
  {
    # Szukamy poziomu cięcia odpowiadającego zadanemu poziomu ufności na suwaku
    target_prob <- poziom_ufnosci
    cumulative_prob <- cumsum(sorted_dens * dx)
    line_level <- sorted_dens[which.min(abs(cumulative_prob - target_prob))]
    
    # Tworzenie wektora wskaźnikowego dla wartości gęstości powyżej progu odcięcia
    credible_set_indicator <- as.vector(as.integer(gestosc_manipulate >= line_level))
    
    if(sum(credible_set_indicator) > 0) {
      credible_set_begin <- match(1, credible_set_indicator)
      credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
      x1 <- siatka_manipulate[credible_set_begin]
      x2 <- siatka_manipulate[credible_set_end]
      
      posterior.cs <- gestosc_manipulate * credible_set_indicator
      HPDI_probab <- sum(posterior.cs) * dx
      
      # Rysowanie bazowego rozkładu
      plot(siatka_manipulate, gestosc_manipulate, las = 1, lwd = 2, bty = "n", col = green_line,
           ylim = c(0, max(gestosc_manipulate) * 1.2), type = "l", 
           ylab = "gęstość", xlab = colnames(X)[ii], 
           main = paste("HPDI dla:", colnames(X)[ii]))
      
      polygon(c(siatka_manipulate, rev(siatka_manipulate)), 
              c(gestosc_manipulate, rep(0, length(siatka_manipulate))), 
              col = green_area, border = NA)
      
      # Nakładanie elementów wyznaczających HPDI
      abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
      abline(h = line_level, col = red_line, lwd = 3)
      polygon(c(siatka_manipulate, rev(siatka_manipulate)), 
              c(posterior.cs, rep(0, length(siatka_manipulate))), 
              col = red_area, border = NA)
      
      # Opis matematyczny wyznaczonego przedziału na wykresie
      text(siatka_manipulate[1], max(gestosc_manipulate) + 0.05, 
           paste(round(HPDI_probab * 100, digits = 1), "% HPDI: (", 
                 round(x1, digits = 4), " ; ", round(x2, digits = 4), ")"), 
           col = red_line, pos=4)
    }
  },
  poziom_ufnosci = slider(0.01, 0.99, step = 0.01, initial = 0.95, label = "Poziom Ufności HPDI")
)

# ==============================================================================
# 4b. Czynniki Bayesa - badanie istotności wprowadzonych zmiennych
# ==============================================================================

# Obliczenie Logarytmu Wiarygodności Brzegowej dla pełnego modelu (M1)
LML_1 <- 0.5 * log(det(U.posterior)) + lgamma(v.posterior / 2) - (v.posterior / 2) * log(vs2.posterior) - 
  (N.data / 2) * log(pi) - 0.5 * log(det(U.prior)) - lgamma(v.prior / 2) + (v.prior / 2) * log(vs2.prior)

# Przygotowanie wektorów na wyniki dla modeli zrestrykcjonowanych (M2)
LML_2 <- rep(NA, k - 1)
log_BF_1_2 <- rep(NA, k - 1)

for (jj in 2:k) { 
  X_2 <- X[, -c(jj)]
  
  Beta.ols.data_2 <- solve(t(X_2) %*% X_2) %*% t(X_2) %*% y
  v.data_2 <- N.data - ncol(X_2)
  XTX.data_2 <- t(X_2) %*% X_2
  s2.data_2 <- sum((y - X_2 %*% Beta.ols.data_2) ^ 2) / v.data_2
  
  Beta.prior_2 <- Beta.prior[-c(jj)]
  U.prior_2 <- U.prior[-c(jj), -c(jj)]
  
  Beta.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2) %*% (solve(U.prior_2) %*% Beta.prior_2 + XTX.data_2 %*% Beta.ols.data_2)
  U.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2)
  v.posterior_2 <- v.prior + N.data
  vs2.posterior_2 <- v.prior * s2.prior + v.data_2 * s2.data_2 + t(Beta.ols.data_2 - Beta.prior_2) %*% solve(U.prior_2 + solve(XTX.data_2)) %*% (Beta.ols.data_2 - Beta.prior_2)
  
  # Logarytm Wiarygodności Brzegowej dla zredukowanego modelu (M2)
  LML_2[jj - 1] <- 0.5 * log(det(U.posterior_2)) + lgamma(v.posterior_2 / 2) - (v.posterior_2 / 2) * log(vs2.posterior_2) - 
    (N.data / 2) * log(pi) - 0.5 * log(det(U.prior_2)) - lgamma(v.prior / 2) + (v.prior / 2) * log(vs2.prior)
  
  # Obliczenie logarytmu Czynnika Bayesa (LML_1 - LML_2)
  log_BF_1_2[jj - 1] <- LML_1 - LML_2[jj - 1]
}

# Odzyskanie prawdziwych wartości BF poprzez eksponentę
BF_1_2 <- exp(log_BF_1_2)

# Ostrzeżenie na wypadek wartości dążących do nieskończoności (gdy dowód jest absolutnie przytłaczający)
BF_1_2 <- ifelse(is.infinite(BF_1_2), "> 1000 (Ogromny dowód)", round(BF_1_2, 3))

BF_1_2_table <- data.frame(
  Zmienna_usunieta = colnames(X)[2:k], 
  Log_BF = round(log_BF_1_2, 3), 
  BF = BF_1_2
)

cat("\n--- CZYNNIKI BAYESA (Model pełny vs Model bez danej zmiennej) ---\n")
print(BF_1_2_table)



# ==============================================================================
# 5. Prognoza punktowa i przedziałowa
# ==============================================================================

prognozuj_powiat_full <- function(klasa, wynagrodzenie_tys, scenariusz_name,
                                  Beta_post = Beta.posterior, 
                                  U_post = U.posterior, 
                                  s2_post = s2.posterior, 
                                  v_post = v.posterior, 
                                  srednia_krajowa = srednie_zarobki_tys,
                                  srednia_klasa_prob = srednia_klasa) {
  
  # 1. Konstrukcja wektora predykcyjnego x_tau
  prognoza_zarobki_cent <- wynagrodzenie_tys - srednia_krajowa
  prognoza_klasa_cent <- klasa - srednia_klasa_prob # DODANE
  prognoza_interakcja <- prognoza_klasa_cent * prognoza_zarobki_cent 
  
  x_star <- matrix(c(1, prognoza_klasa_cent, prognoza_zarobki_cent, prognoza_interakcja), nrow = 1, ncol = 4) 
  
  # 2. Parametry rozkładu predykcyjnego t-Studenta
  # Wartość oczekiwana: x_tau * Beta_bar
  y_star_mu <- as.numeric(x_star %*% Beta_post)
  # Skala: s2_bar * (1 + x_tau * U_bar * x_tau^T)
  pred_variance_factor <- as.numeric(1 + x_star %*% U_post %*% t(x_star))
  y_star_scale <- sqrt(s2_post * pred_variance_factor)
  
  # 3. Obliczanie przedziałów HPDI (dla rozkładu symetrycznego t to przedziały kwantylowe)
  hpdi_50 <- c(y_star_mu + qt(0.25, df = v_post) * y_star_scale, 
               y_star_mu + qt(0.75, df = v_post) * y_star_scale)
  
  hpdi_90 <- c(y_star_mu + qt(0.05, df = v_post) * y_star_scale, 
               y_star_mu + qt(0.95, df = v_post) * y_star_scale)
  
  # 4. Wizualizacja gęstości predykcyjnej
  sciezka_plot <- file.path(katalog_wykresy, paste0("predykcja_", scenariusz_name, ".png"))
  png(filename = sciezka_plot, width = 800, height = 600, res = 120)
  
  x_seq <- seq(y_star_mu - 4 * y_star_scale, y_star_mu + 4 * y_star_scale, length.out = 1000)
  # Używamy dmvt dla gęstości rozkładu t
  y_dens <- apply(as.matrix(x_seq), 1, dmvt, delta = y_star_mu, 
                  sigma = as.matrix(y_star_scale^2), df = v_post, log = FALSE)
  
  plot(x_seq, y_dens, type = "l", lwd = 2, col = green_line, bty = "n", las = 1,
       main = paste("Rozkład predykcyjny:", scenariusz_name),
       xlab = "Wynik egzaminu (pkt)", ylab = "Gęstość")
  
  # Cieniowanie 90% (zielony) i 50% (czerwony)
  idx_90 <- x_seq >= hpdi_90[1] & x_seq <= hpdi_90[2]
  polygon(c(x_seq[idx_90], rev(x_seq[idx_90])), c(y_dens[idx_90], rep(0, sum(idx_90))), col = green_area, border = NA)
  idx_50 <- x_seq >= hpdi_50[1] & x_seq <= hpdi_50[2]
  polygon(c(x_seq[idx_50], rev(x_seq[idx_50])), c(y_dens[idx_50], rep(0, sum(idx_50))), col = red_area, border = NA)
  
  abline(v = y_star_mu, col = green_line, lwd = 2, lty = 2)
  dev.off()
  
  # 5. Raportowanie wyników
  cat(sprintf("\nSCENARIUSZ: %s\n", scenariusz_name))
  cat(sprintf("Punktowa (E): %.2f pkt\n", y_star_mu))
  cat(sprintf("50%% HPDI: [%.2f ; %.2f]\n", hpdi_50[1], hpdi_50[2]))
  cat(sprintf("90%% HPDI: [%.2f ; %.2f]\n", hpdi_90[1], hpdi_90[2]))
}

# Wywołanie scenariuszy
prognozuj_powiat_full(15, 5.5, "Przeciętny (klasa 15)")
prognozuj_powiat_full(15, 3.5, "Uboższy (klasa 15)")
prognozuj_powiat_full(15, 8.0, "Bogaty (klasa 15)")

prognozuj_powiat_full(30, 5.5, "Przeciętny (klasa 30)")
prognozuj_powiat_full(30, 3.5, "Uboższy (klasa 30)")
prognozuj_powiat_full(30, 8.0, "Bogaty (klasa 30)")