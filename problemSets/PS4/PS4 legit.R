# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# load data 
data("infants")
infants

infants_surv <- with(infants, Surv(enter, exit, event))

km <- survfit(infants_surv ~ 1, data = infants)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

cox <- coxph(infants_surv ~ age + sex, data = infants)
cox.fit <- survfit(cox)
autoplot(cox.fit)
