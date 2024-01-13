#Projekt 

library(arrow)
library(lubridate)

dane = read_parquet("Lista4_dane.parquet", col_select = NULL, as_data_frame = TRUE, 
                    props = ParquetArrowReaderProperties$create())
dane = as.data.frame(dane)

dane_oryg <- dane


### OCZYSZCZANIE DANYCH ### 

#Usuwam kolumny, których nie będziemy uwzględniały w analizie
dane <- subset(dane, select = -c(BIK_BANKLiczbaZap2_11m, BIK_PSNLiczbaZap11m, BIK_BANKSaldoNalWymagKred, BIK_PSNNajgorszyHistStatusKred,
                                  CAR_SredLiczbaDniPrzekLim13_24m, CAR_SredLiczbaDniPrzekLim12m, CAR_SredWykOV1m, CAR_LiczbaTrans30dni,
                                  CAR_SredLiczbaDniPrzeterm6m, CAR_WartAktywow, CAR_KwPrzetermDzis, CAR_LiczbaDniDecNegKred, EBANK_MinKwPrzelBBC,
                                  TRANS_LiczbaTransOplUrzad, TRANS_LiczbaTransWyplZewn, TRANS_LiczbaTransUruchomKred, CAR_KwMaxPrzekrLimOV, 
                                  CAR_SumaWplywowOczyszcz1m))

colnames(dane)

# Oczyszczenie kolumn dotyczących wieku
# Zamiana notacji wykładniczej na l. całkowite

dane$CAR_KlWiek <- as.integer(dane$CAR_KlWiek)
dane$CAR_KlWiekY <- as.integer(dane$CAR_KlWiekY)

# (1) W kolumnie, gdzie powinien być podany wiek w latach - podany jest wiek w miesiącach
# (2) W kolumnach miesiąc/rok zdarza się, że podane są różne wartości
# Usuwam wiersze, dla których wartości są różne

dane_wiek <- dane[dane$CAR_KlWiek == dane$CAR_KlWiekY, ]

# (3) Usunięcie kolumny, gdzie wiek jest w miesiącach (powielona informacja)
dane_wiek <- subset(dane_wiek, select = -CAR_KlWiek)

# (4) Zamiana wieku z miesięcy na lata
dane_wiek$CAR_KlWiekY <- as.integer(dane_wiek$CAR_KlWiekY/12)

# (5) Usunięcie NA
dane_wiek <- subset(dane_wiek, !is.na(CAR_KlWiekY))

# (6) Usunięcie klientów, których wiek jest <18 i >85
dane_wiek <- subset(dane_wiek, CAR_KlWiekY >= 18 & CAR_KlWiekY <= 85)

dane <- dane_wiek

# Usunięcie NA z wybranych kolumn (pomijam np. $MIESIACE_ZATRUDNIENIA, ponieważ NA dla rodzaju zatrudniania: Emerytura, Renta)

dane_bez_na <- dane[!(is.na(dane$CUSTOMER_CODE) | is.na(dane$APPLICATION_DATE) | is.na(dane$DEFAULT)
                      | is.na(dane$PRODUKT) | is.na(dane$WNIOSKOWANA_KWOTA) | is.na(dane$WOJEWODZTWO)
                      | is.na(dane$STAN_CYWILNY) | is.na(dane$STATUS_MIESZKANIOWY) | is.na(dane$WYKSZTAlCENIE)
                      | is.na(dane$ZAWOD_WYKONYWANY) | is.na(dane$Dochod) | is.na(dane$RODZAJ_ZATRUDNIENIA)
                      | is.na(dane$SEKTOR) | is.na(dane$TYP_PRACODAWCY) | is.na(dane$WIELKOSC_ZATRUD)), ]

dane <- dane_bez_na


# Usunięcie nieprawdiłowych danych

# (1) Wartość DEFAULT inna niż 0 i 1
dane <- dane[!(dane$DEFAULT != 0 & dane$DEFAULT != 1), ]

# (2) Wartość "X" w województwie
unique(dane$WOJEWODZTWO)
dane <- dane[!(dane$WOJEWODZTWO == "X"), ]

# (3) Ujemna wartość w kol. MIESIACE_ZATRUDNIENIA
dane <- dane[!(dane$MIESIACE_ZATRUDNIENIA < 0 & !is.na(dane$MIESIACE_ZATRUDNIENIA)), ]

# Sprawdzenie dat
min(dmy(dane$APPLICATION_DATE)) #ok
max(dmy(dane$APPLICATION_DATE)) #ok

# Dochód - przedziały i kategorie

min(dane$Dochod)
max(dane$Dochod)
median(dane$Dochod)

kwantyle <- quantile(dane$Dochod, probs = seq(0.2, 1, by = 0.2))
przedzialy_kwotowe <- c(-1, 2800, 3800, 4700, 9800, 20000, Inf) #przybliżam do pełnych, "ładnych" kwot :)

# Tworzenie kategorii dochodu na podstawie przedziałów
X <- cut(dane$Dochod, przedzialy_kwotowe, labels = c(1, 2, 3, 4, 5, 6))

# Stopnie dochodu: 1 - "0-2800", 2 - "2800-3800", 3 - "3800-4700", 4 - "4700-9800", 5 - "9800-20000", 6 - "20000+"

dane$Dochod_kat <- X

# Wielkość zatrudnienia - kategorie 
unique(dane$WIELKOSC_ZATRUD)

dane$Wielk_zatr_kat <- ifelse(dane$WIELKOSC_ZATRUD == "[0 ; 3]", 1,
                              ifelse(dane$WIELKOSC_ZATRUD == "[4 ; 29]", 2,
                                     ifelse(dane$WIELKOSC_ZATRUD == "[30 ; 59]", 3,
                                            ifelse(dane$WIELKOSC_ZATRUD == "[60 ; 119]", 4, 5))))

# Wnioskowana kwota - przedziały i kategorie
min(dane$WNIOSKOWANA_KWOTA)
max(dane$WNIOSKOWANA_KWOTA)
median(dane$WNIOSKOWANA_KWOTA)

kwantyle <- quantile(dane$WNIOSKOWANA_KWOTA, probs = seq(0.2, 1, by = 0.2))
przedzialy_kwotowe <- c(-1, kwantyle[-length(kwantyle)], Inf) 

X <- cut(dane$WNIOSKOWANA_KWOTA, przedzialy_kwotowe, labels = c(1, 2, 3, 4, 5))

# Stopnie: 1 - "0-3000", 2 - "3000-5000", 3 - "5000-10100", 4 - "10100-26000", 5 - "26000+

dane$Wnioskowana_kw_kat <- X


#usuwanie kolumn które zostały przekształone na inne (Dochod i WIELKOSC_ZATRUD)
dane <- subset(dane, select = -c(Dochod, WIELKOSC_ZATRUD))




### PODZIAŁ NA PRÓBY TEST I TRAIN ### 

kolumny_do_przekształcenia <- 2:ncol(dane)
dane[, kolumny_do_przekształcenia] <- lapply(dane[, kolumny_do_przekształcenia], as.factor)
str(dane)

# Podział na próby train, test
set.seed(100000) 
rozmiar = 0.7 #może będzie można wybrać w aplikacji i zobaczyć, jak zmienia się model?
smp = floor(rozmiar * nrow(dane))
ind = sample(seq_len(nrow(dane)), size = smp)

proba_train = dane[ind,]
proba_test = dane[-ind,]

#Statystyki WOE i IV 
library(woeBinning)

bucket = woe.binning(proba_train, 'DEFAULT', proba_train, stop.limit = 0, min.perc.total = 0.1)

#tak sprawdzić IV: 
#bucket[,3] Total IV
#woe.binning.table(bucket)[1]


