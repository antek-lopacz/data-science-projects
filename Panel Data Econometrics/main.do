use "PATH\graduates-major-data.dta", clear

describe
* ==============================================================================
* OPIS ZMIENNYCH
* ==============================================================================
// ZMIENNE STRUKTURALNE I IDENTYFIKACYJNE PANELU
* P_KIERUNEK_ID     - Unikalny identyfikator kierunku studiów (użyty do budowy panelu)
* P_ROKDYP          - Rok uzyskania dyplomu; definiuje wymiar czasowy panelu (t)
* unique_cohort_id  - Wygenerowany unikalny identyfikator obiektu panelowego (kombinacja: kierunek x poziom x forma); definiuje przekrój (i)

// ZMIENNA OBJAŚNIANA
* P_WWB_NSTUD       - Względny Wskaźnik Bezrobocia absolwentów w okresach, 
* gdy nie studiowali po uzyskaniu dyplomu

// PIERWOTNE ZMIENNE OBJAŚNIAJĄCE
* P_POZIOM          - Poziom studiów (sklasyfikowany: 1-Licencjat, 2-Magisterskie, 3-Jednolite)
* P_FORMA           - Forma studiów (zmienna binarna: 1-Stacjonarne, 0-Niestacjonarne)
* P_DZIEDZINA_ID    - Numer identyfikacyjny dziedziny naukowej (wartości 1-8)
* P_DZIEDZINA       - Nazwa tekstowa dziedziny
* P_N               - Liczba absolwentów w danej kohorcie
* P_PROC_DOSW       - Procent absolwentów z doświadczeniem zawodowym przed dyplomem

// WYGENEROWANE ZMIENNE ZERO-JEDYNKOWE (DUMMY VARIABLES)
* p_licencjackie    - 1 dla studiów licencjackich, 0 w p.p. (Kategoria bazowa).
* p_magisterskie    - 1 dla studiów magisterskich, 0 w p.p.
* p_jednolite_mag   - 1 dla jednolitych magisterskich, 0 w p.p.

* dz_humanistyczne  - 1 dla nauk humanistycznych, 0 w p.p.
* dz_inzynieryjne   - 1 dla nauk inżynieryjno-technicznych, 0 w p.p.
* dz_medyczne       - 1 dla nauk medycznych i o zdrowiu, 0 w p.p.
* dz_rolnicze       - 1 dla nauk rolniczych, 0 w p.p.
* dz_scisle         - 1 dla nauk ścisłych i przyrodniczych, 0 w p.p.
* dz_spoleczne      - 1 dla nauk społecznych, 0 w p.p. (Kategoria bazowa).
* dz_teologiczne    - 1 dla nauk teologicznych, 0 w p.p.
* dz_sztuki         - 1 dla dziedziny sztuki, 0 w p.p.
* rok_2014          - 1 dla roku dyplomu 2014, 0 w p.p. (Kategoria bazowa).
* rok_2015 do 2023 - Seria zmiennych binarnych dla kolejnych lat (rok_2015, rok_2016, itd.).

* ANALIZA ZMIENNYCH ILOŚCIOWYCH
summarize P_WWB_NSTUD P_N P_PROC_DOSW, detail

* ANALIZA ZMIENNYCH KATEGORYCZNYCH
tabulate P_POZIOM
tabulate P_FORMA
tabulate P_DZIEDZINA_ID
tabulate P_DZIEDZINA

* ==============================================================================
* PRZYGOTOWANIE DANYCH
* ==============================================================================

// P_POZIOM
replace P_POZIOM = "3" if P_POZIOM == "JM"
destring P_POZIOM, replace
label define poziom_lbl 1 "Licencjat" 2 "Magisterskie" 3 "Jednolite Magisterskie"
label values P_POZIOM poziom_lbl

// P_FORMA
replace P_FORMA = "1" if P_FORMA == "S"
replace P_FORMA = "0" if P_FORMA == "N"
destring P_FORMA, replace
label define forma_lbl 1 "Stacjonarne" 0 "Niestacjonarne"
label values P_FORMA forma_lbl

// Stworzenie unikalnego ID i ustawienie struktury panelu
egen unique_cohort_id = group(P_KIERUNEK_ID P_POZIOM P_FORMA)
xtset unique_cohort_id P_ROKDYP

// Upewnienie się, że zmienne stałe w czasie takie faktycznie będą
bysort unique_cohort_id (P_ROKDYP): replace P_FORMA = P_FORMA[_N]
bysort unique_cohort_id (P_ROKDYP): replace P_DZIEDZINA_ID = P_DZIEDZINA_ID[_N]
bysort unique_cohort_id (P_ROKDYP): replace P_DZIEDZINA = P_DZIEDZINA[_N]

// P_DZIEDZINA_ID
labmask P_DZIEDZINA_ID, values(P_DZIEDZINA)

// GENEROWANIE JAWNYCH ZMIENNYCH DUMMY (dla estymatora Hausmana-Taylora)

// Utworzenie binarnych zmiennych dla poziomu studiów
gen p_licencjackie = (P_POZIOM == 1)
gen p_magisterskie = (P_POZIOM == 2) 
gen p_jednolite_mag =(P_POZIOM == 3) 


// Utworzenie binarnych zmiennych dla lat
levelsof P_ROKDYP, local(lata)
foreach r of local lata {
    // Tworzy zmienną np. rok_2014, która ma wartość 1 gdy P_ROKDYP to 2014, a 0 w przeciwnym razie
    gen rok_`r' = (P_ROKDYP == `r')
}
// Utworzenie binarnych zmiennych dla dziedzin nauki
gen dz_humanistyczne = (P_DZIEDZINA_ID == 1) // Dziedzina nauk humanistycznych
gen dz_inzynieryjne  = (P_DZIEDZINA_ID == 2) // Dziedzina nauk inżynieryjno-technicznych
gen dz_medyczne      = (P_DZIEDZINA_ID == 3) // Dziedzina nauk medycznych i nauk o zdrowiu
gen dz_rolnicze      = (P_DZIEDZINA_ID == 4) // Dziedzina nauk rolniczych
gen dz_scisle        = (P_DZIEDZINA_ID == 5) // Dziedzina nauk ścisłych i przyrodniczych
gen dz_spoleczne     = (P_DZIEDZINA_ID == 6) // Dziedzina nauk społecznych
gen dz_teologiczne   = (P_DZIEDZINA_ID == 7) // Dziedzina nauk teologicznych
gen dz_sztuki        = (P_DZIEDZINA_ID == 8) // Dziedzina sztuki

// Definicja makr globalnych
global zmienne_w_czasie P_N P_PROC_DOSW
global stale_w_czasie P_FORMA p_magisterskie p_jednolite_mag dz_humanistyczne dz_inzynieryjne dz_medyczne dz_rolnicze dz_scisle dz_teologiczne dz_sztuki
global roczniki rok_2015 rok_2016 rok_2017 rok_2018 rok_2019 rok_2020 rok_2021 rok_2022 rok_2023



* ==============================================================================
* ESTYMACJA MODELI JEDNOKIERUNKOWYCH
* ==============================================================================

/* Model 3.1: Pooled OLS */
reg P_WWB_NSTUD $zmienne_w_czasie $stale_w_czasie
estimates store m_pooled

/* Model 3.2: Efekty Stałe (One-way FE / Within) */
xtreg P_WWB_NSTUD $zmienne_w_czasie, fe
estimates store m_oneway_fe

/* Model 3.3: Efekty Losowe (One-way RE) */
xtreg P_WWB_NSTUD $zmienne_w_czasie $stale_w_czasie, re
estimates store m_oneway_re



* ==============================================================================
* ESTYMACJA MODELI DWUKIERUNKOWYCH
* ==============================================================================
// Kontrolujemy globalne szoki makroekonomiczne w danych latach (np. kryzys, inflacja)

/* Model 4.1: Dwukierunkowe Efekty Stałe (Two-way FE) */
xtreg P_WWB_NSTUD $zmienne_w_czasie i.P_ROKDYP, fe
estimates store m_twoway_fe
testparm i.P_ROKDYP

/* Model 4.2: Dwukierunkowe Efekty Losowe (Two-way RE) */
xtreg P_WWB_NSTUD $zmienne_w_czasie $stale_w_czasie i.P_ROKDYP, re
estimates store m_twoway_re
testparm i.P_ROKDYP

/* Weryfikacja łącznej istotności efektów czasowych */
// Test Walda (testparm)
// H_0: Efekty czasowe są nieistotne
// Dla: Prob > F = 0.0000
// efekty czasowe są wysoce istotne statystycznie -> modele jednokierunkowe są nieodpowiednie
// Wynik (FE i RE): Prob > F = 0.0000 
// Wniosek: Odrzucenie hipotezy zerowej. Efekty czasowe są wysoce istotne statystycznie, co oznacza, że globalne zjawiska makroekonomiczne znacząco wpływają na ryzyko bezrobocia absolwentów, a modele jednokierunkowe są nieodpowiednie



* ==============================================================================
* PROCEDURA DIAGNOSTYCZNA I TESTY SPECYFIKACJI
* ==============================================================================

/* Czy efekty panelowe w ogóle istnieją? (Pooled OLS vs RE) */
// Test mnożnika Lagrange'a Breuscha-Pagana dla modelu RE
// H_0: Var(u) = 0 (unikalne cechy kierunków studiów nie istnieją)
quietly xtreg P_WWB_NSTUD $zmienne_w_czasie $stale_w_czasie i.P_ROKDYP, re
xttest0
// Wynik: Prob > chibar2 =   0.0000
// Wniosek: Zdecydowane odrzucenie hipotezy o braku unikalnych cech kierunków, odrzucenie Pooled OLS.

/* Wybór między efektami stałymi (FE) a losowymi (RE) */
// Test Hausmana dla modeli dwukierunkowych
// H_0: Cov(X, u) = 0 (Brak endogeniczności - specyfika kierunków nie jest skorelowana ze zmiennymi i model RE jest poprawny)
hausman m_twoway_fe m_twoway_re
// Wynik: Prob > chi2 = 0.0000
// Wniosek: Zdecydowane odrzucenie hipotezy o braku korelacji. Występuje endogeniczność. Model RE jest obciążony i niezgodny, co wymusza przejście na model FE (lub estymator Hausmana-Taylora).



* ==============================================================================
* ESTYMATOR HAUSMANA-TAYLORA (HT)
* ==============================================================================

/* HT rozwiązuje problem 
- pozwala uwzględnić zmiennych stałe w czasie, które są bardzo istotne ze względu na ogólną niewielką ilość zmiennych oraz ciekawość wpływu formy czy poziomu studiów
- jednocześnie rozwiązuje problem z endogenicznością, która uniemożliwiła wykorzystanie RE

Klasyfikacja zmiennych:
- Zmienne w czasie egzogeniczne (X1): P_N oraz $roczniki
- Zmienne w czasie endogeniczne (X2): P_PROC_DOSW
- Zmienne stałe egzogeniczne (Z1): p_magisterskie, p_jednolite_mag oraz dziedziny
- Zmienne stałe endogeniczne (Z2): P_FORMA

Logika endogeniczności:
Doświadczenie (P_PROC_DOSW) oraz tryb studiów (d_forma2) są obarczone samoselekcją. 
Lepsze uczelnie i trudniejsze kierunki dzienne przyciągają studentów o wyższym 
ukrytym kapitale ludzkim (ambicja, zdolności, zamożność rodziny), co z kolei 
przekłada się na szybsze wejście na rynek pracy. Resztę zmiennych traktujemy jako 
zewnętrzne uwarunkowania systemowe/administracyjne.
*/

// Estymacja modelu Hausmana-Taylora (HT)
// Pominęte kategorie bazowe: p_licencjackie, dz_spoleczne, rok_2014
xthtaylor P_WWB_NSTUD P_N P_PROC_DOSW p_magisterskie p_jednolite_mag P_FORMA dz_humanistyczne dz_inzynieryjne dz_medyczne dz_rolnicze dz_scisle dz_teologiczne dz_sztuki rok_2016 rok_2017 rok_2018 rok_2019 rok_2020 rok_2021 rok_2022 rok_2023, endog(P_PROC_DOSW P_FORMA)
estimates store m_hausman_taylor



* ==============================================================================
* PREZENTACJA WYNIKÓW
* ==============================================================================
estimates table m_twoway_fe m_twoway_re, b(%9.4f) se stats(N N_g)
estimates table m_hausman_taylor, b(%9.4f) se stats(N N_g)