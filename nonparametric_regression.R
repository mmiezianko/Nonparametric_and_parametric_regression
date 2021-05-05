
library(dplyr)
library(ggplot2)
library(corrplot)



df <- as.data.frame(suma_danych_popr)


df <- df %>% select(-c(8:16))


head(df)
sum(is.na(df))


#usuwanie NA

df_cleaned0<- df[complete.cases(df), ]

df_cleaned <- na.omit(df_cleaned0)

df_cleaned <- df_cleaned0 %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit()

as.numeric(as.character(df_cleaned$dochody_budzetow_powiatow_2019))

df_cleaned$dochody_budzetow_powiatow_2019 <- sub(",", ".", df_cleaned$dochody_budzetow_powiatow_2019)
df_cleaned$dochody_budzetow_powiatow_2019 <-as.numeric(df_cleaned$dochody_budzetow_powiatow_2019)
print(df_cleaned$dochody_budzetow_powiatow_2019, digits=12)



#---------------------------------


# Korelacja przed usuwaniem outlierów



df_cleaned<-df_cleaned %>% 
  rename(

    l_os_na_1_miejsce_kina = liczba_ludnosci_na_1_miejsce_w_kinach_stalych,
    zaludnienie = gestosc_zaludnienia_na_km_2,
    miejsca_wkinach = miejsca_na_widowni_w_kinach_stalych_2019,
    dochod_powiatu = dochody_budzetow_powiatow_2019)
    

M<-cor(df_cleaned[,3:7])
corrplot(M, method="circle")



write.csv(df_cleaned, "/Users/majkamiezianko/Desktop/MNWS/projekt3/data.csv", row.names = FALSE)


##DANE GOTOWE (bez NA i outlierów) data_cleaned2 !!!

#zmiana korelacji po usunięciu outlierów
N<-cor(data_cleaned2[,3:7])
corrplot(N, method="circle")

#badanie rozkładu zmiennych po usunięciu outlierów

shapiro.test(data_cleaned2$l_os_na_1_miejsce_kina)
shapiro.test(data_cleaned2$boiska)
shapiro.test(data_cleaned2$zaludnienie)
shapiro.test(data_cleaned2$dochod_powiatu)
shapiro.test(data_cleaned2$miejsca_wkinach)




#jądra po kolei:  "gaussian", "epanechnikov", "rectangular","triangular", "optcosine"
kernels <- c("gaussian", "epanechnikov", "rectangular","triangular", "optcosine")
for (i in kernels){
plot(density(data_cleaned2$zaludnienie, kernel = i))
}







#---------------------------------------

# REGRESJA PARAMETRYCZNA MNK


fit = lm(data_cleaned2$miejsca_wkinach~data_cleaned2$l_os_na_1_miejsce_kina)
plot(data_cleaned2$l_os_na_1_miejsce_kina, data_cleaned2$miejsca_wkinach,pch=20)
abline(fit, lwd=4, col="red")
summary(fit)

fit2 = lm(data_cleaned2$miejsca_wkinach~data_cleaned2$boiska)
plot(data_cleaned2$boiska, data_cleaned2$miejsca_wkinach,pch=20)
abline(fit2, lwd=4, col="red")
summary(fit2)

fit3 = lm(data_cleaned2$miejsca_wkinach~data_cleaned2$dochod_powiatu)
plot(data_cleaned2$dochod_powiatu, data_cleaned2$miejsca_wkinach,pch=20)
abline(fit3, lwd=4, col="red")
summary(fit3)

fit4 = lm(data_cleaned2$miejsca_wkinach~data_cleaned2$zaludnienie)
plot(data_cleaned2$zaludnienie, data_cleaned2$miejsca_wkinach,pch=20)
abline(fit4, lwd=4, col="red")
summary(fit4)

ggplot(data_cleaned2, aes(zaludnienie,miejsca_wkinach)) +
  geom_point() +
  stat_smooth(method = lm)

#Spróbujemy uratować 2 modele poprzez transformację logarytmiczną

#przed
plot(density(data_cleaned2$l_os_na_1_miejsce_kina))
#po
plot(density(log(data_cleaned2$l_os_na_1_miejsce_kina)))

#po
plot(density(data_cleaned2$zaludnienie))
#przekształcony:
plot(density(log(data_cleaned2$zaludnienie)))

#przed
plot(density(data_cleaned2$miejsca_wkinach))
#po
plot(density(log(data_cleaned2$miejsca_wkinach)))

#zmiana na skale logarytmiczną

data_cleaned2 <- data_cleaned2 
data_cleaned2[,3:7] <- log(data_cleaned2[,3:7])

#ponowna regresja najlepszych modeli

fit_ln = lm(data_cleaned_ln$miejsca_wkinach~data_cleaned_ln$l_os_na_1_miejsce_kina)
plot(data_cleaned_ln$l_os_na_1_miejsce_kina, data_cleaned_ln$miejsca_wkinach,pch=20)
abline(fit_ln, lwd=4, col="red")
summary(fit_ln)

fit_ln2 = lm(data_cleaned_ln$miejsca_wkinach~data_cleaned_ln$zaludnienie)
plot(data_cleaned_ln$zaludnienie, data_cleaned_ln$miejsca_wkinach,pch=20)
abline(fit_ln2, lwd=4, col="red")
summary(fit_ln2)



#---------------------------------------


#NIEPARAMETRYCZNA DLA NIEZLOGARYTMIZOWANYCH (Nadaraya-Watson)
install.packages("np")
library(np)

#l_os_na_1_miejsce_kina

bw0 <- np::npregbw(formula = data_cleaned2$miejsca_wkinach ~ data_cleaned2$l_os_na_1_miejsce_kina, regtype="lc", nmulti=3)
bw0$bw
#lc specifies a local-constant estimator (Nadaraya-Watson) and ll specifies a local-linear estimator. Defaults to lc.
#bw to wspólczynnik regularyzacji


fit_nonparametric <- npreg(txdat=data_cleaned2$l_os_na_1_miejsce_kina, tydat=data_cleaned2$miejsca_wkinach, bws=bw0$bw )
plot(fit_nonparametric)
summary(fit_nonparametric)


plot(fit_nonparametric, col = 2, type = "o")
points(data_cleaned2$l_os_na_1_miejsce_kina, data_cleaned2$miejsca_wkinach)
rug(data_cleaned2$l_os_na_1_miejsce_kina, side = 1); rug(data_cleaned2$miejsca_wkinach, side = 2)

#zaludnienie

bw2 <- np::npregbw(formula = data_cleaned2$miejsca_wkinach ~ data_cleaned2$zaludnienie, regtype="lc", nmulti=3)
bw2$bw

fit_nonparametric2 <- npreg(txdat=data_cleaned2$zaludnienie, tydat=data_cleaned2$miejsca_wkinach, bws=bw2$bw )
plot(fit_nonparametric2, plot.errors.method = "asymptotic")
summary(fit_nonparametric2)


plot(fit_nonparametric2, col = 2, type = "o")
points(data_cleaned2$zaludnienie, data_cleaned2$miejsca_wkinach)
rug(data_cleaned2$zaludnienie, side = 1); rug(data_cleaned2$miejsca_wkinach, side = 2)


# boiska

bw3 <- np::npregbw(formula = data_cleaned2$miejsca_wkinach ~ data_cleaned2$boiska, regtype="lc", nmulti=3)
bw3$bw

fit_nonparametric3 <- npreg(txdat=data_cleaned2$boiska, tydat=data_cleaned2$miejsca_wkinach, bws=bw3$bw )
plot(fit_nonparametric3, plot.errors.method = "asymptotic")
summary(fit_nonparametric3)



plot(fit_nonparametric3, col = 2, type = "o")
points(data_cleaned2$boiska, data_cleaned2$miejsca_wkinach)
rug(data_cleaned2$boiska, side = 1); rug(data_cleaned2$miejsca_wkinach, side = 2)

# dochod_powiatu

bw4 <- np::npregbw(formula = data_cleaned2$miejsca_wkinach ~ data_cleaned2$dochod_powiatu, regtype="lc", nmulti=3)
bw4$bw

fit_nonparametric4 <- npreg(txdat=data_cleaned2$dochod_powiatu, tydat=data_cleaned2$miejsca_wkinach, bws=bw4$bw )
summary(fit_nonparametric4)



plot(fit_nonparametric4, col = 2, type = "o")
points(data_cleaned2$dochod_powiatu, data_cleaned2$miejsca_wkinach)
rug(data_cleaned2$dochod_powiatu, side = 1); rug(data_cleaned2$miejsca_wkinach, side = 2)





# znowu przeprowadzamy regresje dla przekształconcyh logarytmicznie dancyh dla dwóch najlepszych modeli

#l_os_na_1_miejsce_kina
bw0_ln <- np::npregbw(formula = data_cleaned_ln$miejsca_wkinach ~ data_cleaned_ln$l_os_na_1_miejsce_kina, regtype="lc", nmulti=3)
bw0_ln$bw

fit_nonparametric_ln <- npreg(txdat=data_cleaned_ln$l_os_na_1_miejsce_kina, tydat=data_cleaned_ln$miejsca_wkinach, bws=bw0_ln$bw)
plot(fit_nonparametric_ln)
summary(fit_nonparametric_ln)


plot(fit_nonparametric_ln, col = 2, type = "o")
points(data_cleaned_ln$l_os_na_1_miejsce_kina, data_cleaned_ln$miejsca_wkinach)
rug(data_cleaned_ln$l_os_na_1_miejsce_kina, side = 1); rug(data_cleaned_ln$miejsca_wkinach, side = 2)



#zaludnienie
bw1_ln <- np::npregbw(formula = data_cleaned_ln$miejsca_wkinach ~ data_cleaned_ln$zaludnienie, regtype="lc", nmulti=5)
bw1_ln$bw

fit_nonparametric1_ln <- npreg(txdat=data_cleaned_ln$zaludnienie, tydat=data_cleaned_ln$miejsca_wkinach, bws=bw1_ln$bw )
plot(fit_nonparametric1_ln)
summary(fit_nonparametric1_ln)


plot(fit_nonparametric1_ln, col = 2, type = "o")
points(data_cleaned_ln$zaludnienie, data_cleaned_ln$miejsca_wkinach)
rug(data_cleaned_ln$zaludnienie, side = 1); rug(data_cleaned_ln$miejsca_wkinach, side = 2)



#WYBIERAMY MODEL ZE ZMIENNĄ ZALUDNIENIE!!!
#---------------------------------------

#INNA METODA REGRESJI NIEPARAMETRYCZNEJ

SS1 = smooth.spline(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,spar=0.2) 
SS2 = smooth.spline(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,spar=0.8) 
SS3 = smooth.spline(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,spar=1.5) 
plot(data_cleaned2$zaludnienie,data_cleaned2$miejsca_wkinach,pch=20)
lines(SS1, lwd=4, col="orange")
lines(SS2, lwd=4, col="purple")
lines(SS3, lwd=4, col="limegreen")
legend("topright", c("spar=0.2","spar=0.8","spar=1.5"), lwd=6,col=c("orange","purple","limegreen"))


### DODATEK:


### WALIDACJA KRZYŻOWA W CELU USTALENIA WARTOŚCIWSPOLCZYNNIKA REGULARYZACJI
# JAK SIĘ DOMYŚLAMY PODOBNY MECHANIZM TOWARZYSZY USTALANIU WARTOŚCI BW

#REGRESJA ZA POMOCĄ # UŻYCIE FUNKCJI smoothing splin

n = length(data_cleaned2$zaludnienie)
# n: sample size
sp_seq = seq(from=0.05,to=1.0, by=0.05) # values of spar we are exploring
CV_err_sp = rep(NA,length(sp_seq)) 
for(j in 1:length(sp_seq)){
spar_using = sp_seq[j]

  CV_err = rep(NA, n) 
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = data_cleaned2$zaludnienie[-i]
    Y_tr = data_cleaned2$miejsca_wkinach[-i]
    # training set
    SS_fit = smooth.spline(x=data_cleaned2$zaludnienie, y=data_cleaned2$miejsca_wkinach,spar=spar_using) 
    Y_val_predict = predict(SS_fit,x=X_val)
    # we use the 'predict()' function to predict a new value
    CV_err[i] = (Y_val - Y_val_predict$y)^2
  }
  CV_err_sp[j] = sqrt(mean(CV_err)) }
CV_err_sp

plot(x=sp_seq, y=CV_err_sp, type="b", lwd=3, col="blue", xlab="Wartość 'spar'", ylab="RMSE obliczone w LOOCV")


#---------------------------------------


### CROSS VAL REGRESJA NIEPARAMETRYCZNA

RMSE <- function(f, o){
  sqrt(mean((f - o)^2))
}
k <- 5


set.seed(12345)
sim_data <- mutate(data_cleaned2[,3:7],
                   my.folds = sample(1:k,
                                     size = nrow(data_cleaned2),
                                     replace = TRUE))

cv.fun <- function(this.fold, data){
  
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  

  
  bww=npregbw(formula= miejsca_wkinach~zaludnienie, data = train, regtype="lc", nmulti=2)
  reg=npreg(bww, data=train)
  #reg=npreg(bww, newdata=validate)
  pred <-predict(reg, newdata=validate) %>% as.vector()
  
  
  this.rmse <- RMSE(f = pred, o = validate$miejsca_wkinach) # f=reg$mean
  
  return(this.rmse)
}



cv.error <- sapply(seq_len(k),
                   FUN = cv.fun,
                   data = sim_data) %>% mean()
cv.error




### CROSS VAL REGRESJA NIEPARAMETRYCZNA
# DLA RÓŻNYCH PARAMETRÓW WSPÓŁCZYNNIKA REGULARYZACJI
#METODĄ KSSMOOTH

#ZACZYNAMY OD PRZEDSTAWIENIA POJEDYNCZYCH WYKRESOW REGRESJI Z ROZNYMI PARAMETRAMI BANDWITH (WSP. REGULARYZACJI)

Kreg1 = ksmooth(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,kernel = "normal",bandwidth = 0.1) 
Kreg2 = ksmooth(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,kernel = "normal",bandwidth = 0.9) 
Kreg3 = ksmooth(x=data_cleaned2$zaludnienie,y=data_cleaned2$miejsca_wkinach,kernel = "normal",bandwidth = 3.0) 
plot(data_cleaned2$zaludnienie,data_cleaned2$miejsca_wkinach,pch=20)
lines(Kreg1, lwd=4, col="orange")
lines(Kreg2, lwd=4, col="purple")
lines(Kreg3, lwd=4, col="limegreen")
legend("topright", c("h=0.1","h=0.9","h=3.0"), lwd=6, col=c("orange","purple","limegreen")
)


# CROSS VAL DLA RÓŻNYCH PARAMETRÓW WSPÓŁCZYNNIKA REGULARYZACJI:

Y<-data_cleaned2$miejsca_wkinach
X<- data_cleaned2$zaludnienie
n = length(X)
N_cv = 100
k=5
cv_lab = sample(n,n,replace=F) %% k
## randomly split all the indices into k numbers 
h_seq = seq(from=0.2,to=2.0, by=0.1)
CV_err_h = rep(0,length(h_seq)) 
for(i_tmp in 1:N_cv){
  CV_err_h_tmp = rep(0, length(h_seq)) 
  cv_lab = sample(n,n,replace=F) %% k 
  for(i in 1:length(h_seq)){
    h0 = h_seq[i]
    CV_err =0
    for(i_cv in 1:k){
      w_val = which(cv_lab==(i_cv-1)) 
      X_tr = X[-w_val]
      Y_tr = Y[-w_val]
      X_val = X[w_val]
      Y_val = Y[w_val]
      kernel_reg = ksmooth(x = X_tr,y=Y_tr,kernel = "normal",bandwidth=h0,
                           x.points=X_val)
      # WARNING! The ksmooth() function will order the x.points from
      # the smallest to the largest!
      CV_err = CV_err+mean((Y_val[order(X_val)]-kernel_reg$y)^2,na.rm=T)
      # na.rm = T: remove the case of 'NA'
    }
    CV_err_h_tmp[i] = CV_err/k
  }
  CV_err_h = CV_err_h+CV_err_h_tmp
}
CV_err_h = CV_err_h/N_cv
plot(h_seq,CV_err_h, type="b", lwd=4, col="blue", xlab="Smoothing Bandwidth",
     ylab="Błąd predykcji dla 5-CV")



#---------------------------------------


### CROSS VAL REGRESJA PARAMETRYCZNA
### ZA POMOCĄ BIBLIOTEKI CARET

library(caret)
set.seed(12345)
inTrain <- createDataPartition(y = data_cleaned2$miejsca_wkinach, p = .7, list = FALSE)
df.train <- data_cleaned2[inTrain, ]
df.test <- data_cleaned2[- inTrain, ]
fit.control <- caret::trainControl(method = "cv", number = 5)
rf.fit <- caret::train(miejsca_wkinach ~ zaludnienie,
                       data = data_cleaned2,
                       method = "lm",
                       trControl = fit.control)
print(rf.fit)





### CROSS VAL REGRESJA PARAMETRYCZNA
### BEZ UZYCIA BIBLIOTEKI


cv.fun.param <- function(this.fold, data){
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  
  model <- lm(miejsca_wkinach~zaludnienie, data = train)
  pred <- predict(model, newdata = validate) %>% as.vector()
  this.rmse <- RMSE(f = pred, o = validate$miejsca_wkinach)
  return(this.rmse)
}


cv.error.param <- sapply(seq_len(k),
                   FUN = cv.fun.param,
                   data = sim_data) %>%
  mean()
cv.error.param



# CROSS VALIDATION DLA DRUGIEJ NAJLEPSZEJ ZMIENNEJ
#PARAMETRYCZNA

library(caret)
set.seed(12345)
inTrain <- createDataPartition(y = data_cleaned2$miejsca_wkinach, p = .7, list = FALSE)
df.train <- data_cleaned2[inTrain, ]
df.test <- data_cleaned2[- inTrain, ]
fit.control <- caret::trainControl(method = "cv", number = 5)
rf.fit2 <- caret::train(miejsca_wkinach ~ l_os_na_1_miejsce_kina,
                       data = data_cleaned2,
                       method = "lm",
                       trControl = fit.control)
print(rf.fit2)


cv.fun.param2 <- function(this.fold, data){
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  
  model <- lm(miejsca_wkinach~l_os_na_1_miejsce_kina, data = train)
  pred <- predict(model, newdata = validate) %>% as.vector()
  this.rmse <- RMSE(f = pred, o = validate$miejsca_wkinach)
  return(this.rmse)
}


cv.error.param2 <- sapply(seq_len(k),
                         FUN = cv.fun.param2,
                         data = sim_data) %>%mean()
cv.error.param2


#NIEPARAMETRYCZNA

cv.fun2 <- function(this.fold, data){
  
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  
  
  
  bww=npregbw(formula= miejsca_wkinach~l_os_na_1_miejsce_kina, data = train, regtype="lc", nmulti=2)
  reg=npreg(bww, data=train)
  #reg=npreg(bww, newdata=validate)
  pred <-predict(reg, newdata=validate) %>% as.vector()
  
  
  this.rmse <- RMSE(f = pred, o = validate$miejsca_wkinach) # f=reg$mean
  
  return(this.rmse)
}



cv.error2 <- sapply(seq_len(k),
                   FUN = cv.fun2,
                   data = sim_data) %>% mean()
cv.error2
