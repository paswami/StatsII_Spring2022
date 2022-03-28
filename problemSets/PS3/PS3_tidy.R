getwd()


library(tidyverse)
library(ggplot2)
library(nnet)
library(dplyr)
library(MASS)
library(stargazer)

# Q1
data <- read.csv('gdpChange.csv')
data

# GDPWdiff is what we are interested in: up, down, no change
# predictors: regime type, oil

# Construct and interpret an unordered multinomial logit with GDPWdiffas the 
# output and 'no change' as the reference category, including the estimated 
# cutoff points and coefficients.

# data wrangling
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff < 0, '-A')
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff > 0, 'B')

data$GDPWdiff <- factor(data$GDPWdiff, 
                        levels = c( "-A" , 0, "B"),
                        labels = c("negative",
                                   "no change",
                                   "positive"))
# unordered multinomial logit
data$GDPWdiff <- relevel(data$GDPWdiff, ref= "no change")
mult.log <- multinom(GDPWdiff ~ REG + OIL, data = data)
summary(mult.log)
stargazer(mult.log)

# p-values 
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2) 
stargazer(p)

# coefficients 
exp(coef(mult.log))

# confidence
confidence <- (ci <- confint(mult.log))
stargazer(p)

# Construct and interpret an ordered multinomial logit with GDPWdiff as the 
# outcome variable, including the estimated cutoff points and coefficients. 

ordinal.log <- polr(GDPWdiff ~ REG + OIL, data = data, Hess = TRUE)
summary(ordinal.log)
stargazer(ordinal.log)

# p value
ctable <- coef(summary(ordinal.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# confidence 
(ci <- confint(ordinal.log))

# converting to an odds ratio
exp(cbind(OR = coef(ordinal.log), ci))

#
#
#

# Q2
mexico <- read.csv('MexicoMuniData.csv')
summary(mexico)
# outcome: PAN.visits.06
# predictors: competitive.district, marginality.06, PAN.governor.06

# poisson regression
# is there evidence that PAN presidential candidates visit swing districts more?
# t statistic and p-value

pos.reg <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico, family = poisson)
summary(pos.reg)
stargazer(pos.reg)

# estimated mean number of visits from the winning PAN presidential candidate 
# hypothetical competitive district (competitive.district = 1)
# average poverty level (marginality.06 = 0)
# PAN governor (PAN.governor.06 = 1)
coeff <- coef(pos.reg)
coeff
est.visits <- exp(coeff[1] + coeff[2]*1 + coeff[3]*0 + coeff[4]*1)
est.visits
