install.packages("MASS")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("arm")
install.packages("foreign")

library(MASS)
library(dplyr)
library(ggplot2)
library(arm)
library(foreign)


hiv <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/risky.behavior/risky_behaviors.dta", convert.factors=TRUE)
options(width=50)
hiv = mutate(hiv, fupacts=round(fupacts))
summary(hiv)

hiv.glm = glm(fupacts ~ bs_hiv + log(bupacts +1) + sex +
                couples + women_alone, data=hiv,
              family=poisson(link="log"))
summary(hiv.glm)
op <- par(mfrow=c(2,2))
plot(hiv.glm)
x <- predict(hiv.glm)
y <- resid(hiv.glm)
binnedplot(x,y)

pchisq(hiv.glm$deviance, hiv.glm$df.residual,
       lower.tail=F)

hiv.glmod = glm(fupacts ~ bs_hiv + log(bupacts +1) + sex +
                  couples + women_alone, data=hiv,
                family=quasipoisson(link="log"))
summary(hiv.glmod)
op <- par(mfrow=c(2,2))
plot(hiv.glmod)
x <- predict(hiv.glmod)
y <- resid(hiv.glmod)
binnedplot(x,y)
pchisq(hiv.glmod$deviance, hiv.glmod$df.residual,
       lower.tail=F)

hiv.glm.nb = glm.nb(fupacts ~ bs_hiv + log(bupacts + 1) +
                      sex + couples + women_alone,
                    data=hiv)

summary(hiv.glm.nb)
op <- par(mfrow=c(2,2))
plot(hiv.glm.nb)
x <- predict(hiv.glm.nb)
y <- resid(hiv.glm.nb)
binnedplot(x,y)

nsim = 10000
n = nrow(hiv)
X = model.matrix(hiv.glm)
sim.hiv.poi = sim(hiv.glm, nsim)
y.rep = array(NA, c(nsim, nrow(hiv)))
for (i in 1:nsim) {
  mu = exp(X %*% sim.hiv.poi@coef[i,])
  y.rep[i,] = rpois(n, lambda=mu)
}
perc_0 = apply(y.rep, 1, function(x) {mean(x == 0)})
perc_10 = apply(y.rep, 1, function(x) {mean( x > 10)})
op <- par(mfrow=c(1,2))
hist(perc_0)
hist(perc_10)
pchisq(hiv.glm.nb$deviance, hiv.glm.nb$df.residual,
       lower.tail=F)



nsim = 10000
n = nrow(hiv)
X = model.matrix(hiv.glm.nb)
class(hiv.glm.nb) <- "glm" # over-ride class of "glm.nb"
sim.hiv.nb = sim(hiv.glm.nb, nsim) # use GLM to generate beta's
sim.hiv.nb@sigma = rnorm(nsim, hiv.glm.nb$theta,
                         hiv.glm.nb$SE.theta) # add slot for theta overide sigma
y.rep = array(NA, c(nsim, nrow(hiv)))
for (i in 1:nsim) {
  mu = exp(X %*% sim.hiv.nb@coef[i,])
  y.rep[i,] = rnegbin(n, mu=mu, theta=sim.hiv.nb@sigma[i])
}
perc_0 = apply(y.rep, 1, function(x) {mean(x == 0)})
perc_10 = apply(y.rep, 1, function(x) {mean( x > 10)})

op <- par(mfrow=c(1,2))
hist(perc_0)
hist(perc_10)

