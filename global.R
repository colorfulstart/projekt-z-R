library(shiny)
library(DT)
library(arrow)
library(lubridate)
library(woeBinning)
library(randomForest)
library(pROC)
library(ggplot2)
library(dplyr)

dane <- read_parquet("Lista4_dane.parquet", col_select = NULL, as_data_frame = TRUE, 
                    props = ParquetArrowReaderProperties$create())
dane <- as.data.frame(dane)
dane_oryg <- dane
dane <- subset(dane, select = -c(BIK_BANKLiczbaZap2_11m, BIK_PSNLiczbaZap11m, BIK_BANKSaldoNalWymagKred, BIK_PSNNajgorszyHistStatusKred,
                                 CAR_SredLiczbaDniPrzekLim13_24m, CAR_SredLiczbaDniPrzekLim12m, CAR_SredWykOV1m, CAR_LiczbaTrans30dni,
                                 CAR_SredLiczbaDniPrzeterm6m, CAR_WartAktywow, CAR_KwPrzetermDzis, CAR_LiczbaDniDecNegKred, EBANK_MinKwPrzelBBC,
                                 TRANS_LiczbaTransOplUrzad, TRANS_LiczbaTransWyplZewn, TRANS_LiczbaTransUruchomKred, CAR_KwMaxPrzekrLimOV, 
                                 CAR_SumaWplywowOczyszcz1m))
dane$CAR_KlWiek <- as.integer(dane$CAR_KlWiek)
dane$CAR_KlWiekY <- as.integer(dane$CAR_KlWiekY)
dane_wiek <- dane[dane$CAR_KlWiek == dane$CAR_KlWiekY, ]
dane_wiek <- subset(dane_wiek, select = -CAR_KlWiek)
dane_wiek$CAR_KlWiekY <- as.integer(dane_wiek$CAR_KlWiekY/12)
dane_wiek <- subset(dane_wiek, !is.na(CAR_KlWiekY))
dane_wiek <- subset(dane_wiek, CAR_KlWiekY >= 18 & CAR_KlWiekY <= 85)
dane <- dane_wiek
dane_bez_na <- dane[!(is.na(dane$CUSTOMER_CODE) | is.na(dane$APPLICATION_DATE) | is.na(dane$DEFAULT)
                      | is.na(dane$PRODUKT) | is.na(dane$WNIOSKOWANA_KWOTA) | is.na(dane$WOJEWODZTWO)
                      | is.na(dane$STAN_CYWILNY) | is.na(dane$STATUS_MIESZKANIOWY) | is.na(dane$WYKSZTAlCENIE)
                      | is.na(dane$ZAWOD_WYKONYWANY) | is.na(dane$Dochod) | is.na(dane$RODZAJ_ZATRUDNIENIA)
                      | is.na(dane$SEKTOR) | is.na(dane$TYP_PRACODAWCY) | is.na(dane$WIELKOSC_ZATRUD)), ]

dane <- dane_bez_na
dane <- dane[!(dane$DEFAULT != 0 & dane$DEFAULT != 1), ]

dane <- dane[!(dane$WOJEWODZTWO == "X"), ]
dane <- dane[!(dane$MIESIACE_ZATRUDNIENIA < 0 & !is.na(dane$MIESIACE_ZATRUDNIENIA)), ]
kwantyle <- quantile(dane$Dochod, probs = seq(0.2, 1, by = 0.2))
przedzialy_kwotowe <- c(-1, 2800, 3800, 4700, 9800, 20000, Inf) 
dane$Dochod_kat <- cut(dane$Dochod, przedzialy_kwotowe, labels = c("[0 ; 2800)", "[2800 ; 3800)", "[3800 ; 4700)", "[4700 ; 9800)", "[9800 ; 20000)", "20000+"))
 

kwantyle <- quantile(dane$WNIOSKOWANA_KWOTA, probs = seq(0.2, 1, by = 0.2))
przedzialy_kwotowe <- c(-1, kwantyle[-length(kwantyle)], Inf) 

dane$Wnioskowana_kw_kat <- cut(dane$WNIOSKOWANA_KWOTA, przedzialy_kwotowe, labels = c("[0 ; 3000)", "[3000 ; 5000)", "[5000 ; 10100)", "[10100 ; 26000)", "26000+"))
dane <- subset(dane, select = -Dochod)
ilosc_NA <- colSums(is.na(dane[,16:ncol(dane)]))

kolumny_z_NA <- names(sort(ilosc_NA, decreasing=TRUE))[1:5]
dane <- dane[, -which(names(dane) %in% kolumny_z_NA)] 
kolumny_do_przekształcenia <- 2:ncol(dane)
dane[, kolumny_do_przekształcenia] <- lapply(dane[, kolumny_do_przekształcenia], as.factor)

rozmiar = 0.7
smp = floor(rozmiar * nrow(dane))
ind = sample(seq_len(nrow(dane)), size = smp)
column_names = colnames(dane)
column_names_bez_default = column_names[column_names != 'DEFAULT']

proba_train = dane[ind,]
proba_test = dane[-ind,]
bucket = woe.binning(proba_train, 'DEFAULT', proba_train, stop.limit = 0, min.perc.total = 0.1)


proba_train_2 = woe.binning.deploy(proba_train, bucket, add.woe.or.dum.var = 'woe')
proba_test_2 = woe.binning.deploy(proba_test, bucket, add.woe.or.dum.var = 'woe')
def_waga <- ifelse(as.numeric(proba_train_2$DEFAULT) == 0, 1, 10)

###@## od tego miejsca chyba już w funkcjach
buduj_model <- function(dane_train, wybrane_kolumny, waga){
  zmienne_objasniajace <- paste("woe.", wybrane_kolumny, ".binned", sep = "")
  model <- glm(DEFAULT ~ ., 
              data = dane_train[, c("DEFAULT", zmienne_objasniajace)], family = binomial, weights = waga)
  
}

IV_max_10 <- bucket[1:10, ]
model_max <- buduj_model(proba_train_2, IV_max_10[, 1] , def_waga)
max_1_znaczaca <- summary(model_max)$deviance
max_2_znaczaca <- summary(model_max)$aic
napis_max <- paste(IV_max_10[,1], collapse = ", ")


IV_min_10 <- bucket[(length(bucket[,1])-9):length(bucket[,1]), ]
model_min <- buduj_model(proba_train_2, IV_min_10[, 1], def_waga)
min_1_znaczaca <- summary(model_min)$deviance
min_2_znaczaca <- summary(model_min)$aic
napis_min <- paste(IV_min_10[,1], collapse = ", ")


losowe_kolumny <- sample(bucket[, 1], 10)
model_los <- buduj_model(proba_train_2, losowe_kolumny, def_waga)
los_1_znaczaca <- summary(model_los)$deviance
los_2_znaczaca <- summary(model_los)$aic
napis_los <- paste(losowe_kolumny, collapse = ", ")


model_uzytkownika <- buduj_model(proba_train_2, c("TYP_PRACODAWCY", "ZAWOD_WYKONYWANY", "SEKTOR"), def_waga)
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

wylicz_zero_jeden <- function(war_wnioskowana_kwota, war_dochod, 
                              war_wyksztalcenie, war_sektor){
  sortowanie <- dane %>% filter(Wnioskowana_kw_kat == war_wnioskowana_kwota,
                                Dochod_kat == war_dochod,
                                WYKSZTAlCENIE == war_wyksztalcenie,
                                SEKTOR==war_sektor) 
  podzial <- sortowanie %>% 
    group_by(DEFAULT) %>% 
    summarize(sum_default = n())
  if(is.na(podzial$sum_default[1])){
    zero <- 0
  }
  else{zero <- podzial$sum_default[1]}
  
  if(is.na(podzial$sum_default[2])){
    jeden <- 0
  }
  else{jeden <- podzial$sum_default[2]}
  
  if(nrow(sortowanie)==0){
    return("Dostępne dane nie umożliwiają nam podania odpowiedzi.")
  }
  else{
    return(paste('Twoja szansa na dostanie kredytu wynosi:', round(zero/nrow(sortowanie)*100,2), 'procent.'))
  }
}

