install.packages("MASS")
install.packages("car")
install.packages("xtable")
install.packages("ggplot2")
install.packages("alr3")
install.packages("dplyr")

library(MASS)
library(car)
library(xtable)
library(ggplot2)
library(alr3)
library(dplyr)

data(Animals, package="MASS")

ggplot(Animals, aes(x=body, y=brain)) +  geom_point() +
  xlab("Body Weight (kg)") + ylab("Brain Weight (g)")
Animals= mutate(Animals, log.body = log(body))
ggplot(Animals, aes(log.body, brain)) + geom_point()

brain.lm = lm(brain ~ body, data=Animals)
pval = 2*(1 - pt(abs(rstudent(brain.lm)), brain.lm$df -1))
rownames(Animals)[pval < .05/nrow(Animals)]

car::outlierTest(brain.lm)
brain2.lm = lm(brain ~ body, data=Animals,
               subset = !cooks.distance(brain.lm)>1)
par(mfrow=c(2,2)); plot(brain2.lm)

Dino=0*Animals$body
Dino[6]=1
Dino[16]=1
Dino[26]=1
Animals = cbind(Animals, Dino)

Dino.T=0*Animals$body
Dino.T[16]=1
Animals=cbind(Animals, Dino.T)

brain1.lm = lm(log(brain) ~ log(body),data=Animals)
par(mfrow=c(2,2)); plot(brain1.lm)

brain2.lm = lm(log(brain) ~ log(body) + Dino,
               data=Animals)
par(mfrow=c(2,2)); plot(brain2.lm)

brain3.lm = lm(log(brain) ~ log(body) + Dino +
                 Dino:log(body),
               data=Animals)
par(mfrow=c(2,2)); plot(brain3.lm)

brain4.lm = lm(log(brain) ~ log(body) + Dino +
                 Dino:log(body) +
                 Dino.T,
               data=Animals)
par(mfrow=c(2,2)); plot(brain4.lm)

confint(brain2.lm)

beta= coef(brain2.lm)
gp = ggplot(Animals, aes(y=log(brain), x=log(body)))+ geom_point(aes(colour=factor(Dino)))+geom_abline(aes(intercept=beta[1], slope=beta[2]))+geom_abline(aes(intercept=(beta[1]+beta[3]),slope=beta[2]))
gp


newdata = data.frame(body=.0259, Dino=0)

fit = predict(brain2.lm, newdata=newdata,
              interval="confidence", se=T)
exp(fit$fit)

pred = predict(brain2.lm, newdata=newdata,
               interval="predict", se=T)
exp(pred$fit)


data(UN3, package="alr3")
UN = dplyr::select(UN3, c(Fertility, PPgdp, Purban)) %>%
  mutate(logPPgdp = log(PPgdp),
         logFertility = log(Fertility)) %>%
  na.omit()

e_Y = residuals(lm(logFertility ~ Purban, data=UN))
e_X1 = residuals(lm(logPPgdp ~ Purban, data=UN))

df = data.frame(e_Y = e_Y, e_X1 = e_X1)
ggplot(data=df, aes(x = e_X1, y = e_Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

summary(lm(logFertility ~ logPPgdp + Purban, data=UN))$coef
summary(lm(e_Y ~ e_X1, data=df))$coef

car::avPlots(lm(logFertility ~ logPPgdp + Purban, data=UN))



