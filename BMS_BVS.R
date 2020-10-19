install.packages("MASS")
install.packages("BMA")
install.packages("BAS")
install.packages("readr")
install.packages("GGally")
install.packages("lmtest")
install.packages("car")
install.packages("devtools")

library(MASS)
library(BMA)
library(BAS)
library(devtools)
library(readr) # to read data
library(dplyr) # to tidy data
library(GGally) # to make correlation matrix
library(lmtest) # for Breusch-Pagan/heteroscedasticity test
library(car) # for multicollinearity test


dev.off() 
data(UScrime)
names(UScrime) <- c("percent_m", "is_south", "mean_education", "police_exp60", "police_exp59", "labour_participation", "m_per1000f", "state_pop", "nonwhites_per1000", "unemploy_m24", "unemploy_m39", "gdp", "inequality", "prob_prison", "time_prison", "crime_rate")
glimpse(UScrime)
pairs(UScrime, pch = 19, lower.panel = NULL)
ggcorr(UScrime, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)

mlm.step <- lm(formula = crime_rate ~ percent_m + is_south + mean_education + labour_participation + 
    m_per1000f + state_pop + unemploy_m24 + 
    unemploy_m39 + gdp + inequality + prob_prison + time_prison + 
    police_exp60, UScrime)
summary(mlm.step)

hist(mlm.step$residuals, breaks = 5)
shapiro.test(mlm.step$residuals)

plot(UScrime$crime_rate, mlm.step$residuals)
abline(h = 0, col = "red")

vif(mlm.step)

x<- UScrime[,-16]
y<- log(UScrime[,16])
x[,-2]<- log(x[,-2])
lma<- bicreg(x, y, strict = FALSE, OR = 20) 
summary(lma)
plot(lma)
imageplot.bma(lma)


UScrime[,16]<- log(UScrime[,16])
UScrime[,-2]<- log(UScrime[,-2])
poll.bma = bas.lm(crime_rate~time_prison+prob_prison+inequality+gdp+unemploy_m39+unemploy_m24+nonwhites_per1000+state_pop+m_per1000f+labour_participation+police_exp59+police_exp60+mean_education+is_south+percent_m, 
data=UScrime,
prior="g-prior",
alpha=41, # g = n
n.models=2^7,# enumerate (can omit)
modelprior=uniform(),
method="deterministic")

plot(poll.bma, which=1)
plot(poll.bma, which=3)
plot(poll.bma, which=4)
summary(poll.bma)
image(poll.bma)
beta = coef(poll.bma, n.models=1)
beta




