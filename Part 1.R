library(dplyr) 
library(ggplot2)
library(seasonal)
library(forecast) 
library(fpp)
library(forecast)
library(tseries)
library(urca)
library(glmulti) 
library(Metrics)
library(MASS)
library(caTools)
library(doParallel)


registerDoParallel(cores = 4)
 
setwd("~/Desktop/Statistics Machine Learning/Assignment - 1/OnlineNewsPopularity") 
df <- read.csv('OnlineNewsPopularity.csv') 
head(df)

# Model changes so as to make the dependency among x and y variable linear 
# Outlier Treatment 
df$kw_min_avg[df$kw_min_avg <0] = 0 
df$kw_min_min[ df$kw_min_min <0] = 0 
df$n_non_stop_words[ (df$n_non_stop_words > 1) ]  = 1 
df$n_unique_tokens[ (df$n_unique_tokens > 1) ]  = 1
df$kw_avg_min[df$kw_avg_min <0] = 0 

# Applying transforms 
df$kw_min_avg  <-   log(df$kw_min_avg   + 1)
df$kw_min_min  <-   log( df$kw_min_min + 1)
df$n_tokens_content <-    log( df$n_tokens_content + 1) 
df$num_imgs         <-    log( df$num_imgs + 1)
df$kw_avg_avg      <-     log(df$ kw_avg_avg + 1)
df$kw_avg_max       <-    log( df$kw_avg_max  + 1)
df$kw_avg_min       <-    log(df$kw_avg_min  + 1)
df$kw_max_avg       <-    log( df$kw_max_avg + 1)
df$kw_max_max       <-    log( df$kw_max_max + 1)
df$kw_max_min       <-    log(df$kw_max_min + 1)
df$kw_min_max       <-    log(df$kw_min_max + 1)
df$num_hrefs        <-     log(df$num_hrefs+1)
df$self_reference_avg_sharess  <-    log(df$self_reference_avg_sharess + 1)
df$self_reference_max_shares   <-    log(df$self_reference_max_shares + 1)
df$self_reference_min_shares   <-    log(df$self_reference_min_shares + 1)


drops <- c("url", "is_weekend", "weekday_is_sunday", 
           "self_reference_min_shares", "self_reference_max_shares" , 
           "self_reference_avg_shares", "timedelta", "kw_min_min")
df <- df[ , !(names(df) %in% drops)] 

# Removing these variables w.r.t. the VIF analysis 
df <- df[, !(colnames(df) %in% c("LDA_03"))] 
df <- df[, !(colnames(df) %in% c("rate_positive_words"))] 
df <- df[, !(colnames(df) %in% c("kw_min_max"))] 
df <- df[, !(colnames(df) %in% c("n_non_stop_words"))] 
df <- df[, !(colnames(df) %in% c("kw_avg_max"))] 
df <- df[, !(colnames(df) %in% c("kw_max_min"))] 
df <- df[, !(colnames(df) %in% c("kw_avg_avg"))] 
 

####################################################################################################################################################################################
#Q1  

hist(df$shares)

mean(df$shares) 

var(df$shares) 




### Gaussian 
# Copying the linear regression
gaussian<-glm(formula = shares  ** -0.2222222   ~. , family="gaussian",data = df) 
# Goodness of fit 
with(summary(gaussian), 1 - deviance/null.deviance)* 100
gaussian  
plot(gaussian)
regression<-lm(formula = shares  ** -0.2222222   ~. ,data = df) 


### Poison  
poision<-glm(formula = shares ~. , family="poisson",data = df) 
poision
# Goodness of fit 
with(summary(poision), 1 - deviance/null.deviance) * 100
plot(poision)
 
predict(poision, df, type = "link", se.fit=TRUE) 


# Negative binomial 
negative_binomial <- glm.nb(shares ~. , data = df)  
negative_binomial 
with(summary(poision), 1 - deviance/null.deviance) * 100

nbc <- data.frame(negative_binomial$coefficients) 
plot(negative_binomial)  


 





####################################################################################################################################################################################
#Q2

#prediction interval 

# take a random df value 
# poission
df_all = data.frame()
for (i in seq(0,100)){
  print(i)
  dfrandom = df[sample(nrow(df), 10), ]
  dfrandom_share = dfrandom$shares
  dfrandom$shares = NULL
  dfrandom <- cbind(dfrandom, predict(poision, dfrandom, type = "link", se.fit=TRUE))
  dfrandom <- within(dfrandom, {
    forecast <- exp(dfrandom$fit)
    LL <- exp(dfrandom$fit - 1.96 * dfrandom$se.fit)
    UL <- exp(dfrandom$fit + 1.96 * dfrandom$se.fit)
  }) 
  dfrandom$shares = dfrandom_share
  df_all = rbind(df_all, dfrandom)
}
print(dim(df_all))
plot(df_all$share, df_all$forecast)
lines(supsmu(df_all$share, df_all$forecast) , col = "purple")
lines(supsmu(df_all$share, df_all$LL) , col="green" )
lines(supsmu(df_all$share, df_all$UL) , col="green" )
print(rmse(df_all$shares , df_all$forecast))


# nb
df_all_nb = data.frame()
for (i in seq(0,100)){
  print(i)
  dfrandom = df[sample(nrow(df), 10), ]
  dfrandom_share = dfrandom$shares
  dfrandom$shares = NULL
  dfrandom <- cbind(dfrandom, predict(negative_binomial, dfrandom, type = "link", se.fit=TRUE))
  dfrandom <- within(dfrandom, {
    forecast <- exp(dfrandom$fit)
    LL <- exp(dfrandom$fit - 1.96 * dfrandom$se.fit)
    UL <- exp(dfrandom$fit + 1.96 * dfrandom$se.fit)
  }) 
  dfrandom$shares = dfrandom_share
  df_all_nb = rbind(dfrandom,  df_all_nb)
  
}
print(dim(df_all_nb))
plot(df_all_nb$share, df_all_nb$forecast)
lines(supsmu(df_all_nb$share, df_all_nb$forecast) , col = "purple")
lines(supsmu(df_all_nb$share, df_all_nb$LL) , col="green" )
lines(supsmu(df_all_nb$share, df_all_nb$UL) , col="green" )
print(rmse(df_all_nb$shares , df_all_nb$forecast))

# Gaussian
df_all_g = data.frame()
for (i in seq(0,100)){
  print(i)
  dfrandom = df[sample(nrow(df), 10), ]
  dfrandom_share = dfrandom$shares
  dfrandom$shares = NULL
  dfrandom <- cbind(dfrandom, predict(gaussian, dfrandom, type = "link", se.fit=TRUE))
  dfrandom <- within(dfrandom, {
    forecast <- (dfrandom$fit) **  -(1/ 0.222)
    LL <- (dfrandom$fit - 1.96 * dfrandom$se.fit) ** -(1/ 0.222)
    UL <- (dfrandom$fit + 1.96 * dfrandom$se.fit) ** -(1/ 0.222)
  }) 
  dfrandom$shares = dfrandom_share
  df_all_g = rbind(dfrandom,  df_all_g)
  
}
print(dim(df_all_g))
plot(df_all_g$share, df_all_g$forecast)
lines(supsmu(df_all_g$share, df_all_g$forecast) , col = "purple")
lines(supsmu(df_all_g$share, df_all_g$LL) , col="green" )
lines(supsmu(df_all_g$share, df_all_g$UL) , col="green" )
print(rmse(df_all_g$shares , df_all_g$forecast))


# RMSE 
dfrandom = df[sample(nrow(df), 1000), ] 
dfrandom_share = dfrandom$shares
dfrandom$shares = NULL 
dfrandom$gaussian = (predict(gaussian, dfrandom)) **  -(1/ 0.222)
dfrandom$nb = exp(predict(negative_binomial, dfrandom, type = "link", se.fit=TRUE)$fit) 
dfrandom$poison = exp(predict(poision, dfrandom, type = "link", se.fit=TRUE)$fit) 
dfrandom$shares = dfrandom_share 
head(dfrandom)

rmse(dfrandom$shares, dfrandom$gaussian)
rmse(dfrandom$shares, dfrandom$nb)
rmse(dfrandom$shares, dfrandom$poison)

 
#########################################################################################
