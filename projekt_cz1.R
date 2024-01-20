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

# Usunięcie kolumn, gdzie jest najwięcej NA

ilosc_NA <- colSums(is.na(dane[,17:ncol(dane)])) #wcześniej oczyszczone

kolumny_z_NA <- names(sort(ilosc_NA, decreasing=TRUE))[1:5] #Sortowanie wyników i wybieranie nazw 5 kolumn z największą ilością NA

dane <- dane[, -which(names(dane) %in% kolumny_z_NA)] #Usunięcie tych 5 kolumn 


### PODZIAŁ NA PRÓBY TEST I TRAIN ### 

kolumny_do_przekształcenia <- 2:ncol(dane)
dane[, kolumny_do_przekształcenia] <- lapply(dane[, kolumny_do_przekształcenia], as.factor)
str(dane)

# Podział na próby train, test
set.seed(100000) 
rozmiar = 0.7
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




### BUDOWA MODELU ###

proba_train_2 = woe.binning.deploy(proba_train, bucket, add.woe.or.dum.var = 'woe')
proba_test_2 = woe.binning.deploy(proba_test, bucket, add.woe.or.dum.var = 'woe')

#Zmiana wagi defaulta: z 1 na 10, z 0 na 1
def_waga <- ifelse(as.numeric(proba_train_2$DEFAULT) == 0, 1, 10)

colnames(proba_train_2) #chcemy te kolumny, które mają nazwy postaci "woe.XXXXXXXX.binned"


# Residual Deviance to miara oceny dopasowania modelu do danych
# Im mniejsza wartość Residual Deviance, tym lepiej model pasuje do danych. 

# AIC (Akaike Information Criterion) to miara oceny jakości modelu statystycznego. 
# Niższa wartość AIC wskazuje na lepszą jakość modelu.



# Mniej więcej taka funkcja ogólnie do tworzenia modelu: 
buduj_model <- function(dane_train, wybrane_kolumny, waga){ #wybrane_kolumny to wektor nazw kolumn
  zmienne_objasniajace <- paste("woe.", wybrane_kolumny, ".binned", sep = "")
  model = glm(DEFAULT ~ ., 
              data = dane_train[, c("DEFAULT", zmienne_objasniajace)], family = binomial, weights = waga)
  
}

# 1) 10 najistotniejszych w bucket

IV_max_10 <- bucket[1:10, ]

#zmienne_objasniajace <- paste("woe.", IV_max_10[,1], ".binned", sep = "")
#model_max = glm(DEFAULT ~ ., 
#                data = proba_train_2[, c("DEFAULT", zmienne_objasniajace)], family = binomial, weights = def_waga)

model_max <- buduj_model(proba_train_2, IV_max_10[, 1] , def_waga)

summary(model_max)
summary(model_max)$deviance
summary(model_max)$aic

# 2) 10 najmniej istotnych 

IV_min_10 <- bucket[(length(bucket[,1])-9):length(bucket[,1]), ]

#zmienne_objasniajace <- paste("woe.", IV_min_10[,1], ".binned", sep = "")

#model_min = glm(DEFAULT ~ ., 
#                data = proba_train_2[, c("DEFAULT", zmienne_objasniajace)], family = binomial, weights = def_waga)


model_min <- buduj_model(proba_train_2, IV_min_10[, 1], def_waga)
summary(model_min)
summary(model_min)$deviance
summary(model_min)$aic

# 3) 10 losowych zmiennych 

losowe_kolumny <- sample(bucket[, 1], 10)

model_random <- buduj_model(proba_train_2, losowe_kolumny, def_waga)
summary(model_random)$deviance
summary(model_random)$aic


# 4) wybór użytkownika

# np:
model_uzytkownika <- buduj_model(proba_train_2, c("TYP_PRACODAWCY", "ZAWOD_WYKONYWANY", "SEKTOR"), def_waga)


# GINI, AUC, ROC

#Im wyżej znajduje się krzywa ROC, tym lepszy model.
#Im większe AUC i Gini, tym lepsza zdolność modelu do rozróżniania klas.

library(randomForest)
library(pROC)
library(ggplot2)

rysuj_ROC <- function(rzeczywiste_wart, model){
  prawdopodobienstwa <- predict(model, type = "response")
  roc_curve <- roc(rzeczywiste_wart, prawdopodobienstwa)
  roc_data <- coords(roc_curve, "all")
  
  auc_value <- auc(roc_curve)
  gini_value <- 2 * auc_value - 1
  
  ggplot(data = roc_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)",
         title = "Krzywa ROC") +
    annotate("text", x = 0.95, y = 0.05,
             label = paste("AUC =", round(auc_value, 3), ", Gini =", round(gini_value, 3)),
             color = "black", size = 5, hjust = 1, vjust = 0) +
    theme_minimal()
}

rysuj_ROC(proba_train_2$DEFAULT, model_max)
rysuj_ROC(proba_train_2$DEFAULT, model_min)
rysuj_ROC(proba_train_2$DEFAULT, model_random)
rysuj_ROC(proba_train_2$DEFAULT, model_uzytkownika)


#wykres procentowy rozkładu wykształcenia do wnioskowanej kwoty
procenty <- dane %>%
  group_by(WYKSZTAlCENIE, Wnioskowana_kw_kat) %>%
  summarise(Liczba = n()) %>%
  group_by(WYKSZTAlCENIE) %>%
  mutate(Procent = Liczba / sum(Liczba) * 100)

ggplot(procenty, aes(x = WYKSZTAlCENIE, y = Procent, fill = factor(Wnioskowana_kw_kat))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Procentowy rozkład Wnioskowanej kwoty w zależności od wykształcenia", 
       x = "Wykształcenie", y = "Procent")+
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Wnioskowana kwota", labe = c("0-3000", "3000-5000", "5000-10100", "10100-26000", "26000+"))

#wykres procentowy rozkładu dochodu od wnioskowanej kwoty
procenty_dochod <- dane %>%
  group_by(Dochod_kat, Wnioskowana_kw_kat) %>%
  summarise(Liczba = n()) %>%
  group_by(Dochod_kat) %>%
  mutate(Procent = Liczba / sum(Liczba) * 100)

ggplot(procenty_dochod, aes(x = Dochod_kat, y = Procent, fill = factor(Wnioskowana_kw_kat))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Procentowy rozkład dochodu w zależności od wykształcenia",
      x = "Dochód", y = "Procent") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Wnioskowana kwota", labe = c("0-3000", "3000-5000", "5000-10100", "10100-26000", "26000+"))

#wykres kołowy rozkładu województwa
dane %>%
  count(WOJEWODZTWO) %>%
  mutate(Procentowy_udzial = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = Procentowy_udzial, fill = WOJEWODZTWO, label = sprintf("%.1f%%", Procentowy_udzial))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.4), size = 3, angle = 45) +
  coord_polar(theta = "y") +
  labs(title = "Udział województw", x = "", y = "") +
  theme_void()
