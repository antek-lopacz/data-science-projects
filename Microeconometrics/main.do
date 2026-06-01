use "PATH\CRONOS-3 country data - PL.dta", clear

* --------------------------------------------------
* PRZYGOTOWANIE ZBIORU DANYCH
* --------------------------------------------------
// Wyciągnięcie odpowiedzi o wartości podatku
egen Y_12 = rowfirst(w5eq12_a w5eq12_b w5eq12_c w5eq12_d w5eq12_e w5eq12_f)
egen Y_13 = rowfirst(w5eq13_a w5eq13_b w5eq13_c w5eq13_d w5eq13_e w5eq13_f)
egen Y_14 = rowfirst(w5eq14_a w5eq14_b w5eq14_c w5eq14_d w5eq14_e w5eq14_f)

// Transformacja z formatu szerokiego na długi
reshape long Y_, i(idno) j(nr_pytania)

// Stworzenie docelowej zmiennej podatek
rename Y_ podatek
label variable podatek "Wskazana stawka podatku od spadku (%)"
drop if missing(podatek)

* UTWORZENIE ZMIENNYCH DOTYCZĄCYCH WARIANTU PYTANIA
// Mapowanie na podstawie klucza (w5eadmin: 1=A, 2=B, 3=C, 4=D, 5=E, 6=F)
gen spadek_tys = .          // 25000: "€25k", 250000:"€250k", 1000000: "€1m"  
gen dochod_spadkobiercy = . // 1: poniżej śr, 2: średni, 3: powyżej śr
gen dlug_publiczny = .      // 0: niski, 1: wysoki

// WIELKOŚĆ SPADKU (w tys. EUR)
replace spadek_tys = 1000 if (nr_pytania==12 & inlist(w5eadmin,1,6)) | (nr_pytania==13 & inlist(w5eadmin,1,6)) | (nr_pytania==14 & inlist(w5eadmin,3,5))
replace spadek_tys = 250  if (nr_pytania==12 & inlist(w5eadmin,3,5)) | (nr_pytania==13 & inlist(w5eadmin,4,5)) | (nr_pytania==14 & inlist(w5eadmin,4,6))
replace spadek_tys = 25   if (nr_pytania==12 & inlist(w5eadmin,2,4)) | (nr_pytania==13 & inlist(w5eadmin,2,3)) | (nr_pytania==14 & inlist(w5eadmin,1,2))

// DOCHÓD SPADKOBIERCY
replace dochod_spadkobiercy = 1 if (nr_pytania==12 & w5eadmin==6) | (nr_pytania==13 & inlist(w5eadmin,1,4)) | (nr_pytania==14 & inlist(w5eadmin,1,2))
replace dochod_spadkobiercy = 2 if (nr_pytania==12 & inlist(w5eadmin,1,2,3,5)) | (nr_pytania==13 & w5eadmin==3) | (nr_pytania==14 & w5eadmin==5)
replace dochod_spadkobiercy = 3 if (nr_pytania==12 & w5eadmin==4) | (nr_pytania==13 & inlist(w5eadmin,2,5,6)) | (nr_pytania==14 & inlist(w5eadmin,3,4,6))

// DŁUG PUBLICZNY
replace dlug_publiczny = 0 if (nr_pytania==12 & inlist(w5eadmin,1,4,5,6)) | (nr_pytania==13 & inlist(w5eadmin,3,6)) | (nr_pytania==14 & inlist(w5eadmin,2,4,6))
replace dlug_publiczny = 1 if (nr_pytania==12 & inlist(w5eadmin,2,3)) | (nr_pytania==13 & inlist(w5eadmin,1,2,4,5)) | (nr_pytania==14 & inlist(w5eadmin,1,3,5))

// Etykietowanie
label define spadek_lbl 1000 "€1m" 250 "€250k" 25 "€25k"
label values spadek_tys spadek_lbl
label define inc_lbl 1 "Poniżej śr" 2 "Średni" 3 "Powyżej śr"
label values dochod_spadkobiercy inc_lbl
label define debt_lbl 0 "Niski" 1 "Wysoki"
label values dlug_publiczny debt_lbl



* --------------------------------------------------
* ANALIZA DANYCH
* --------------------------------------------------
// Zmienne:
// spadek_tys
// dochod_spadkobiercy
// dlug_publiczny
// age - Age of respondent, calculated
// gndr - Gender
// eduyrs - Years of full-time education completed
// w1xq1 - Number of people living regularly as member of household
// hinctnta - Household's total net income, all sources
recode hinctnta (1/3 = 1 "Niski") (4/7 = 2 "Sredni") (8/10 = 3 "Wysoki") (missing = 4 "Missing"), gen(hinctnta_grouped)
// w2eq17 - How likely unable to pay for unexpected expenses next 12 months
recode w2eq17 (1 = 1 "Not at all likely") (2/4 = 2 "Somewhat likely") (5 = 3 "Extremely likely"), gen(unexp_expense)
// w5hq8 - Severe financial difficulties in family during first 18 years of life, how often
recode w5hq8 (1 2 = 1 "Zawsze/Często") (3 = 2 "Czasami") (4 5 = 3 "Prawie wcale/Nigdy"), gen(fin_dif_childhood)
// w4eq9 - Ever received substantial inheritance
// w4eq10 - Expect to receive substantial inheritance in future
// w4eq11 - Importance of leaving inheritance to surviving heirs
// w2eq11 - In favour or against a basic income scheme
recode w2eq11 (1 2 = 1 "Za") (3 = 2 "Neutralnie") (4 5 = 3 "Przeciw"), gen(basic_income)
// w1eq19 - Standard of living for the old, governments' responsibility
// w4eq5 - Importance of hard work for accumulating wealth
// mnactic - Main activity, last 7 days. All respondents. Post coded

histogram podatek, bin(30)
histogram w5eadmin



* --------------------------------------------------
* MODELOWANIE
* --------------------------------------------------

// Final Selection
global core i.spadek_tys ib2.dochod_spadkobiercy dlug_publiczny
global socjodemo age gndr eduyrs
global materialny ib2.unexp_expense ib2.hinctnta_grouped ib2.fin_dif_childhood
global poglady w4eq5 ib2.basic_income

tobit podatek $core $socjodemo $materialny $poglady, ll(0) ul(100)



* --------------------------------------------------
* ANALIZA EFEKTÓW
* --------------------------------------------------

* 1. EFEKT CAŁKOWITY DLA OBSERWOWANEGO PODATKU
// Odpowiada na pytanie:
// "O ile punktów/jednostek zmieni się obserwowany podatek dla przeciętnego 
// obywatela w całej populacji (uwzględniając zera i setki)?"
margins, dydx(*) predict(ys(0, 100))

* 2. EFEKT TYLKO DLA OSÓB "POŚRODKU" (Wewnątrz przedziału)
// Odpowiada na pytanie:
// "Jeśli ktoś już płaci podatek większy niż 0 i mniejszy niż 100, 
// to jak zmieni się jego podatek przy zmianie danej zmiennej?"
margins, dydx(*) predict(e(0, 100))

* 3. EFEKTY DLA PRAWDOPODOBIEŃSTWA (Ekstensywne)
// A. Szansa, że podatek nie będzie ani 0, ani 100 (znajdzie się w środku)
margins, dydx(*) predict(pr(0, 100))

// B. Szansa, że ktoś zapłaci równe 0 (trafi na dolną granicę)
// "Jak np. wyższe wykształcenie wpływa na szansę całkowitego uniknięcia/zwolnienia z podatku?"
margins, dydx(*) predict(pr(., 0))

// C. Szansa, że ktoś zapłaci równe 100 (trafi na górną granicę)
// "Jak dana zmienna "wpycha" ludzi w maksymalny podatek?"
margins, dydx(*) predict(pr(100, .))



* --------------------------------------------------
* ZAPIS EFEKTÓW DO LATEX
* --------------------------------------------------

* 1. EFEKT CAŁKOWITY (Obserwowany podatek)
quietly margins, dydx(*) predict(ys(0, 100)) post
estimates store efekt_caly

tobit podatek $core $socjodemo $materialny $poglady, ll(0) ul(100)
estimates store model_bazowy

* 2. EFEKT TYLKO DLA OSÓB "POŚRODKU"
quietly estimates restore model_bazowy
quietly margins, dydx(*) predict(e(0, 100)) post
estimates store efekt_srodek

* 3. PRAWDOPODOBIEŃSTWO (Ekstensywne)
* A. Szansa na bycie w środku (0 < y < 100)
quietly estimates restore model_bazowy
quietly margins, dydx(*) predict(pr(0, 100)) post
estimates store pr_srodek

* B. Szansa na stawkę 0 (Dolna granica)
quietly estimates restore model_bazowy
quietly margins, dydx(*) predict(pr(., 0)) post
estimates store pr_zero

* C. Szansa na stawkę 100 (Górna granica)
quietly estimates restore model_bazowy
quietly margins, dydx(*) predict(pr(100, .)) post
estimates store pr_sto

// EKSPORT 
esttab efekt_caly efekt_srodek pr_srodek pr_zero pr_sto ///
    using wyniki_tobit.tex, replace ///
    label booktabs ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    title("Efekty krańcowe modelu Tobit") ///
    mtitle("E(y)" "E(y|0<y<100)" "Pr(0<y<100)" "Pr(y=0)" "Pr(y=100)")
	
	
	
histogram podatek, bin(40)
scatter podatek hinctnta