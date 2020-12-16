

# Celem pracy jest porównanie dokładności oszacowań modeli o różnych postaciach oraz o różnych sposobach predykcji. W pierwszej części zostaną przedstawione dwa modele. Model A, który będzie zawierał wszystkie zmienne zawarte w zborze Dane oraz model B, który będzie modelem po zastosowaniu metody "od ogółu do szczegółu" celem wyboru najlepszej postaci modelu. W kolejnej części zostaną przedstawione trzy modele, na których zostanie przeprowadzona walidacja krzyżowa o różnych liczbach iteracji. Następnie przedstawione zostaną modele, w których zostanie zastosowana regularyzacja- regresja grzbietowa oraz Lasso.
# Z pośród modeli zostanie wybrany ten o najdokładniejszej jakości predyckcji i najmniejszej wartości błędów.

# Wczytanie danych

library(readxl)
model1 <- read_excel("~/Desktop/model1.xlsx")
#View(model1)

# Przygotowanie zbioru danych

library(dplyr)
library(readr)
library(AER)
library(lmtest) # lrtest(), waldtest()
library(nnet)
#library(caret)
library(verification)
library(tidyr)
library(foreign)
library(janitor) # tabyl()
library(class)
require(caret)
library(glmnet)
dane= model1
summary(dane)
glimpse(dane)

# Dane pochodzą z ankiety przeprowadzonej poprzez portale społecznościowe, głównie na grupach studenckich (Studenci UW, Studenci Politechniki Warszawskiej itp.). Badanie miało charakter przekrojowy, a grupą docelową były osoby w wieku 20-30 lat. Łącznie zgromadzono 430 obserwacji. Zmienną objaśnianą jest planowany wiek rodzenia pierwszego dziecka, natomiast zmiennymi onjaśniającymi:
#   - płeć respondenta- 0-kobieta, 1-mężczyzna, zmienna binarna,
# - wiek respondenta - zmienna ciągła,
# - wielkość zamieszkiwanej przez respondenta miejscowości oraz wielkość miejsowości urodzenia respondenta-
#   1	poniżej 10 tysięcy	
# 2	- małe miasto - 10-50 tys.	
# 3	- średnie miasto - 50-150 tys.	
# 4	- duże miasto - 150-500 tys.	
# 5	- bardzo duże miasto - 500-1000 tys.	
# 6	- wielkie miasto - pow. miliona	
# - poziom wykształcenia respondenta-
#   1	jestem uczennicą/uczniem szkoły średniej
# 2	ukończone średnie i nie kontynuuję nauki
# 3	jestem studentką/em
# 0	wyższe pełne
# - typ piekunku studiów -
#   1	Kierunki ekonomiczne
# 2	Kierunki medyczne
# 3	Kierunki humanistyczne
# 4	Kierunki społeczne
# 5	Kierunki prawa i administracji
# 6	Kierunki biologiczne i przyrodnicze
# 7	Kierunki ścisłe
# 8	Kierunki wychowania fizycznego
# - ocena swojego stanu zdrowia przez respondenta w skali od 0-5,
# - miejsce na którym respondent podaje "urodzenie dziecka" wśród innych priorytetów życiowych (zawarcie związku małżeńskiego, urodzenie pierwszego dziecka, ukończenie studiów, osiągnięcie satysfakcjonującej sytuacji zawodowej, realizacja innych planów nie związanych z karierą (podróże, sport,…), zdobycie pierwszej pracy związanej z ukończonym kierunkiem studiów, posiadanie własnego mieszkania),
# - liczbę dzieci jaką chciałby mieć respondent.

# Pierwszym krokiem jest przygotowanie danych. W zbiorze nie występują braki danych, ani obserwacje odstające, natomiast niektóre zmienne są zmiennymi jakościowymi.

dane = na.omit(dane)

dane$plec <- as.factor(dane$plec)
dane$wyksz <- as.factor(dane$wyksz)
dane$typ_kierunku <- as.factor(dane$typ_kierunku)

levels(dane$plec)
levels(dane$typ_kierunku)

# Ogólna postać modelu

# Model A zawierający wszystkie dostępne zmienne charakteryzujące respondentów 

# Model przedstawia się w następujący sposób:

dane.lm <- lm(wiek_rodzenia_dziecka ~  
                wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec, # formula
              data = dane) # data

summary(dane.lm)

# Histogram reszt przedstawia się w następujący sposób:

head(dane.lm$residuals)
hist(dane.lm$residuals, breaks = 30)
#install.packages("tseries")
library(tseries)
test=jarque.bera.test(dane.lm$residuals)
test$p.value

# P-value testu Jarque-Bera jest mniejsze niż 5% zatem odrzucamy H0 mówiące o normalności reszt.

# Błędy oszacowań

# Kolejnym krokiem jest wyliczenie błędów oszacowań:

# MSE
mean(dane.lm$residuals^2)

# MAE
mean(abs(dane.lm$residuals))

# macierz błędów

regressionMetrics <- function(real, predicted) {
  # Mean Squera Error
  MSE <- mean((real - predicted)^2)
  # Root Mean Squera Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - predicted))
  # Median Absolute Error
  MedAE <- median(abs(real - predicted))
  # Mean Logarithmic Absolute Error
  MSLE <- mean((log(1 + real) - log(1 + predicted))^2)
  # Total Sum of Squares
  TSS <- sum((real - mean(real))^2)
  # Explained Sum of Squares
  RSS <- sum((predicted - real)^2)
  # R2
  R2 <- 1 - RSS/TSS
  
  result <- data.frame(MSE, RMSE, MAE, MedAE, MSLE)
  return(result)
}

# Macierz błędów dla naszego modelu:
  
a=regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                    predicted = predict(dane.lm))

a

# Histogram wartości rzeczywistych i prognozowanych dla pierwszego modelu:

par(mfrow = c(2, 1))
hist(dane$wiek_rodzenia_dziecka, 
     breaks = 30, 
     col = "lightblue")

hist(predict(dane.lm), 
     breaks = 30,
     col = "lightblue")
par(mfrow = c(1, 1))

# Wybór postaci modelu

# Aby znaleźć najlepszą postać modelu przeprowadzimy metodę od ogółu do szczegołu:

summary(dane.lm)

dane.lm <- lm(wiek_rodzenia_dziecka ~  
                wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec, # formula
              data = dane) # data

# pominięcie poziomu typ_kierunku6
dane$typ_kierunku=factor(dane$typ_kierunku, exclude="6")
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm2 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm2)

# pominięcie zmiennej miejsce_urodzenia

dane.lm3 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm3)

# pominięcie poziomu typ_kierunku7
dane$typ_kierunku=factor(dane$typ_kierunku, exclude=c("7", NA))
levels(dane$typ_kierunku)
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm4 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm4)

# pominięcie poziomu typ_kierunku2
dane$typ_kierunku=factor(dane$typ_kierunku, exclude=c("2", NA))
levels(dane$typ_kierunku)
#dane = na.omit(dane)
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm5 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm5)


# pominięcie poziomu typ_kierunku4
dane$typ_kierunku=factor(dane$typ_kierunku, exclude=c("4", NA))
levels(dane$typ_kierunku)
#dane = na.omit(dane)
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm6 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm6)

# pominięcie zmiennej zdrowie
dane.lm7 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 miejsc_mieszkania+ typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm7)

# pominięcie zmiennej miejsce_mieszaknia
dane.lm8 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm8)

# pominięcie poziomu typ_kierunku3
dane$typ_kierunku=factor(dane$typ_kierunku, exclude=c("3", NA))
levels(dane$typ_kierunku)
#dane = na.omit(dane)
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm9 <- lm(wiek_rodzenia_dziecka ~  
                 wiek_respondenta+ miejsce_dziecka + wiek_matki + ile_dzieci+
                 typ_kierunku + wyksz + plec, # formula
               data = dane) # data
summary(dane.lm9)

# pominięcie zmiennej wiek_respondenta
dane.lm10 <- lm(wiek_rodzenia_dziecka ~  
                  miejsce_dziecka + wiek_matki + ile_dzieci+
                  typ_kierunku + wyksz + plec, # formula
                data = dane) # data
summary(dane.lm10)

# pominięcie poziomu typ_kierunku8
dane$typ_kierunku=factor(dane$typ_kierunku, exclude=c("8", NA))
levels(dane$typ_kierunku)
dane = na.omit(dane)
levels(dane$typ_kierunku)[levels(dane$typ_kierunku)==NA]="1"
levels(dane$typ_kierunku)

dane.lm11 <- lm(wiek_rodzenia_dziecka ~  
                  miejsce_dziecka + wiek_matki + ile_dzieci+
                  typ_kierunku + wyksz + plec, # formula
                data = dane) # data
summary(dane.lm11)

# pominięcie zmiennej płec
dane.lm12 <- lm(wiek_rodzenia_dziecka ~  
                  miejsce_dziecka + wiek_matki + ile_dzieci+
                  typ_kierunku + wyksz, # formula
                data = dane) # data
summary(dane.lm12)

# Histogram wartości rzeczywistych i prognozowanych dla drugiego modelu:

par(mfrow = c(2, 1))
hist(dane$wiek_rodzenia_dziecka, 
     breaks = 30, 
     col = "lightblue")

hist(predict(dane.lm12), 
     breaks = 30,
     col = "lightblue")
par(mfrow = c(1, 1))

# Estymacja modelu B z zachowaniem jedynie istotnych zmiennych

# Porównainie oszacowań modeli:

summary(dane.lm)
summary(dane.lm12)

# W drugim modelu, zmienne są istotne na poziomie 10%. Wzrósł wspólczynnik R^2, który mówi o tym, że zmienne objaśniające wyjaśniają 35% zmienności modelu.

# Porównanie błędów pierwszego modelu i modelu po usunięciu nieistotnych zmiennych:

a

b= regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                     predicted = predict(dane.lm12))
b

# Statystyka R^2 wzrosła, natomiast wzrosły również wartości błędów MSE, RMSE, MAE oraz MSALE.
# Zatem wybieramy pierwszą formułę modelu:

modelformula <- wiek_rodzenia_dziecka ~  
  wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
  miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec

# Podział danych na część treningową i testową.

# Kolejnym krokiem jest podzielenie zbioru danych na część treningową i testową:

set.seed(987654321)
size <- floor(0.8*nrow(dane))

train_indice = sample(seq_len(nrow(dane)), size = size)
dane_train = dane[train_indice,]
dane_test = dane[-train_indice,]

nrow(dane_test)/nrow(dane)
nrow(dane_train)/nrow(dane)

# Dane zostały podzielone na dwie części, z czego część testowa stanowi 20% a część treningowa 80% całego zbioru.

# Rozkład zmiennej zależnej w obu próbkach:

tabyl(dane_train$wiek_rodzenia_dziecka)
tabyl(dane_test$wiek_rodzenia_dziecka)

# Cross validation

# Trening modelu I o domyślnej liczbie iteracji walidacji krzyżowej
# Pierwsza stymacja modelu treningowego:

ctrl_nocv <- trainControl(method = "none")

set.seed(987654321)

dane.model.train <- 
  train(wiek_rodzenia_dziecka ~  
          wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
          miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec, 
        data = dane_train, # training sample
        # model type
        method = "lm",
        # train control
        trControl = ctrl_nocv)
summary(dane.model.train)

# Predykcja na zbiorze treningowym:

dane.model.train_fitted <- predict(dane.model.train,
                                   dane_train)

head(dane.model.train_fitted)

# Predykcja na zbiorze testowym:

dane.model.train_forecasts <- predict(dane.model.train,
                                      dane_test)

# Porównanie predykcji zbioru treningowego i testowego:

ggplot(data=dane_train, aes(x=dane_train$wiek_rodzenia_dziecka, y=dane.model.train_fitted))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu I - zbiór treningowy") +
  theme_minimal()

ggplot(data=dane_test, aes(x=dane_test$wiek_rodzenia_dziecka, y=dane.model.train_forecasts))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu I - zbiór testowy") +
  theme_minimal()

# Histogram wartości rzeczywistych i prognozowanych:

histogram <- hist(dane_test$wiek_rodzenia_dziecka - dane.model.train_fitted)

# Wartości błędów:

# MSE
mse1 <- mean((dane_train$wiek_rodzenia_dziecka - dane.model.train_fitted)^2)
rmse1 <- sqrt(mse1)
# MAE
mae1 <- mean(abs(dane_train$wiek_rodzenia_dziecka - dane.model.train_fitted))

result1 <- data.frame(mse1, rmse1, mae1)
result1

# MSE
mse1_1 <- mean((dane_test$wiek_rodzenia_dziecka - dane.model.train_forecasts)^2)
rmse1_1 <- sqrt(mse1_1)
# MAE
mae1_1 <- mean(abs(dane_test$wiek_rodzenia_dziecka - dane.model.train_forecasts))

result1_1 <- data.frame(mse1_1, rmse1_1, mae1_1)
result1_1

# W zbiorze testowym wartości błędów są mniejsze niż w zbiorze treningowym. 

# Trening modelu II o liczbie iteracji walidacji krzyżowej równej 10

# Druga estymacja modelu treningowego:

ctrl_cv10 <- trainControl(method = "cv",
                          number = 10)
set.seed(987654321)

dane.model2.train <- 
  train(wiek_rodzenia_dziecka ~  
          wiek_respondenta+ zdrowie+ miejsce_dziecka + wiek_matki + ile_dzieci+
          miejsc_mieszkania + miejsc_urodzenia+ typ_kierunku + wyksz + plec, 
        data = dane_train, # training sample
        # model type
        method = "lm",
        # train control
        trControl = ctrl_cv10)
summary(dane.model2.train)
dane.model2.train$results


# Prognoza na zbiorze treningowym:

dane.model2.train_fitted <- predict(dane.model2.train,
                                    dane_train)

head(dane.model2.train_fitted)

# Predykcja na zbiorze testowym:

dane.model2.train_forecasts <- predict(dane.model2.train,
                                       dane_test)

# Porównanie predykcji zbioru treningowego i testowego:

ggplot(data=dane_train, aes(x=dane_train$wiek_rodzenia_dziecka, y=dane.model2.train_fitted))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu II - zbiór treningowy") +
  theme_minimal()

ggplot(data=dane_test, aes(x=dane_test$wiek_rodzenia_dziecka, y=dane.model2.train_forecasts))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu II - zbiór testowy") +
  theme_minimal()

# Wyniki dla wszystkich iteracji walidacji krzyżowej:

dane.model2.train$resample


# Histogram wartości rzeczywistych i prognozowanych:

histogram <- hist(dane_test$wiek_rodzenia_dziecka - dane.model2.train_fitted)


# Wartości błędów:

# MSE
mse2 <- mean((dane_train$wiek_rodzenia_dziecka - dane.model2.train_fitted)^2)
rmse2 <- sqrt(mse2)
# MAE
mae2 <- mean(abs(dane_train$wiek_rodzenia_dziecka - dane.model2.train_fitted))

result2 <- data.frame(mse2, rmse2, mae2)

# MSE
mse2_2 <- mean((dane_test$wiek_rodzenia_dziecka - dane.model2.train_forecasts)^2)
rmse2_2 <- sqrt(mse2_2)
# MAE
mae2_2 <- mean(abs(dane_test$wiek_rodzenia_dziecka - dane.model2.train_forecasts))

result2_2 <- data.frame(mse2_2, rmse2_2, mae2_2)

result2
result2_2

# W zbiorze testowym wartości błędów są mniejsze niż w zbiorze treningowym.

# Trening modelu III zawierającego same istotne zmienne oraz z zastosowaniem walidacji krzyżowej o domyślnej liczbie iteracji

# Walidacja krzyżowa modelu lm12- "A" (ze wszystkimi zmiennymi istotnymi):

ctrl_nocv <- trainControl(method = "none")

set.seed(987654321)

dane.model3.train <- 
  train(wiek_rodzenia_dziecka ~  
          miejsce_dziecka + wiek_matki + ile_dzieci+
          typ_kierunku + wyksz, # formula
        data = dane_train, # training sample
        # model type
        method = "lm",
        # train control
        trControl = ctrl_nocv)
summary(dane.model3.train)

# Predykcja na zbiorze treningowym:

dane.model3.train_fitted <- predict(dane.model3.train,
                                    dane_train)

head(dane.model3.train_fitted)

# Predykcja na zbiorze testowym:

dane.model3.train_forecasts <- predict(dane.model3.train,
                                       dane_test)

# Porównanie predykcji zbioru treningowego i testowego:

ggplot(data=dane_train, aes(x=dane_train$wiek_rodzenia_dziecka, y=dane.model3.train_fitted))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu III - zbiór treningowy") +
  theme_minimal()

ggplot(data=dane_test, aes(x=dane_test$wiek_rodzenia_dziecka, y=dane.model3.train_forecasts))+
  geom_point() +
  geom_smooth(method="lm", formula = y~x-1)+
  xlab("Obserwacja") +
  ylab("Prognoza") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla Modelu III - zbiór testowy") +
  theme_minimal()


# Histogram wartości rzeczywistych i prognozowanych:

histogram <- hist(dane_test$wiek_rodzenia_dziecka - dane.model3.train_fitted)

# Wartości błędów:

# MSE
mse3 <- mean((dane_train$wiek_rodzenia_dziecka - dane.model3.train_fitted)^2)
rmse3 <- sqrt(mse3)
# MAE
mae3 <- mean(abs(dane_train$wiek_rodzenia_dziecka - dane.model3.train_fitted))

result3 <- data.frame(mse3, rmse3, mae3)

mse3_3 <- mean((dane_test$wiek_rodzenia_dziecka - dane.model3.train_forecasts)^2)
rmse3_3 <- sqrt(mse3_3)
# MAE
mae3_3 <- mean(abs(dane_test$wiek_rodzenia_dziecka - dane.model3.train_forecasts))

result3_3 <- data.frame(mse3_3, rmse3_3, mae3_3)

result3
result3_3

# W zbiorze testowym błędy mają mniejszą wartość niż w zbiorze treningowym.

identical(coef(dane.model.train$finalModel),
          coef(dane.model2.train$finalModel))

identical(coef(dane.model.train$finalModel),
          coef(dane.model3.train$finalModel))

identical(coef(dane.model2.train$finalModel),
          coef(dane.model3.train$finalModel))

# Oszacowane parametry są takie same w modelach I i II. Oznacza to, że obydwie przeprowadzone walidacje krzyżowe o róznych parametrach dały takie same oszacowania.
# Natomaist parametry różnią się pomiędzy modelami I i III oraz II i III ze względu na inną postać modelu. Model III zawiera jedynie istotne zmienne.

# Model IV

# Regularyzacja
# Regresja grzbietowa – Ridge Regression

ctrl_cv10 <- trainControl(method = "cv",
                          number = 10)

set.seed(987654321)

parameters_ridge <- expand.grid(alpha = 0, # ridge 
                                lambda = exp(log(10)*seq(-10, 2, length.out = 200)) )


dane.ridge <- train(wiek_rodzenia_dziecka ~  
                      miejsce_dziecka + wiek_matki + ile_dzieci+ typ_kierunku + wyksz,
                    data = dane_train,
                    method = "glmnet", 
                    tuneGrid = parameters_ridge,
                    trControl = ctrl_cv10)

dane.ridge
plot(dane.ridge)

# RMSE rośnie wraz ze wzrostem parametru lambda.
# 
# Najlepsza wartość parametru lambda:

dane.ridge$bestTune$lambda

# Najlepsza wartość parametru lambda wynosi 0.4448783.
# 
# Wartości przewidywane dla parametru lambda = 0.4448783:

predict(dane.ridge$finalModel, # stored model
        s = 0.4448783, # lambda
        type = "coefficients")

set.seed(987654321)

dane.lm <- train(wiek_rodzenia_dziecka ~  
                   miejsce_dziecka + wiek_matki + ile_dzieci+ typ_kierunku + wyksz,
                 data = dane_train,
                 method = "glmnet", 
                 tuneGrid = expand.grid(alpha = 0, 
                                        lambda = 0))

regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                  predicted = predict(dane.lm, 
                                      dane_test)
)

# dla regresji grzebietowej

ridge= regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                         predicted = predict(dane.ridge, 
                                             dane_test)
)

# Zastosowanie regresji grzebietowej zmniejszyło błędy oszacowań modelu.

# Model V

# Regresja Lasso

set.seed(987654321)

parameters_lasso <- expand.grid(alpha = 1, # LASSO
                                lambda = seq(0.01, 2, 0.01))

dane.lasso <- train(wiek_rodzenia_dziecka ~  
                      miejsce_dziecka + wiek_matki + ile_dzieci+ typ_kierunku + wyksz,
                    data = dane_train,
                    method = "glmnet", 
                    tuneGrid = parameters_lasso,
                    trControl = ctrl_cv10)

dane.lasso
plot(dane.lasso)


dane.lasso$bestTune$lambda

# Najlepsza wartość parametru lambda wynosi 0.01.
# 
# Wartości przewidywane dla parametru lambda = 0.01:
  
predict(dane.lasso$finalModel, # stored model
        s = dane.lasso$bestTune$lambda, # lambda
        type = "coefficients")

lasso = regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                          predicted = predict(dane.lasso, 
                                              dane_test)
)

# Model VI

# ridge i lasso razem
# elastic net


set.seed(987654321)

parameters_elastic <- expand.grid(alpha = seq(0, 1, 0.1), 
                                  lambda = seq(0.01, 2, 0.01))

nrow(parameters_elastic)

dane.elastic <- train(wiek_rodzenia_dziecka ~  
                        miejsce_dziecka + wiek_matki + ile_dzieci+ typ_kierunku + wyksz,
                      data = dane_train,
                      method = "glmnet", 
                      tuneGrid = parameters_elastic,
                      trControl = ctrl_cv10)

dane.elastic
plot(dane.elastic)


# Mixing percentage = alpha
# Regularization parameter = lambda

elastic = regressionMetrics(real = dane$wiek_rodzenia_dziecka,
                            predicted = predict(dane.elastic, 
                                                dane_test)
)

# Porównanie wartości błędów dla wszystkich modeli:
a
b
result1_1
result2_2
result3_3
ridge
lasso
elastic

# # Wnioski
# We wszytskich modelach wartości błędów oszacowań w danych testowych były zbliżone lub niższe do wartości błędów dla danych treningowych. 
# Oznacza to, że uczenie przebiegło w sposób prawidłowy.
# Modele A i B, w których nie została przeprowadzona walidacja krzyżowa, uzyskały njwyższe wartości błędów oszacowań. 
# W modelach I, II oraz III została przeprowadzona walidacja krzyżowa. 
# Ze wszystkich modeli namniejsze wartości błędów ma model III, w którym zastosowano walidację krzyżową oraz wszystkie zmienne były istotne. 
# W pozostałych modelach błędy miały wyższe wartości. 
# Metody regresji grzbietowej, lasso oraz elastic net otrzymały wyniki o największych wartościach błędów. 