# Jung Mee Park
# first sync R to github
# https://jcoliver.github.io/learn-r/010-github.html
# where to add https://github.com/jmp243/work_in_progress.git

# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

# build a predictive model around Q30

library(VIM)
library("mice")
library(vioplot)
library(ISLR)
library(caTools)
library(neuralnet)
library(MASS)
# https://stackoverflow.com/questions/57297771/interpretation-of-l-q-c-4-for-logistic-regression
# mod1 <- glm(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191)
# summary(mod1)
# mod0 <- polr(dm2$Q30 ~ ., data = dm2, Hess = TRUE)
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

lapply(dm2 [,c("Q30", "Q3","Q15", "Q16","Q25", "CumGPA2191", "Career", "Residency", "Ethnicity", "Gender")], table)

# ftable(xtabs(~ public + apply + pared, data = dat))

ftable(xtabs(~ Q30 + Q15 + Q16, data = dm2))

ftable(xtabs(~ Class.Standing + Q30, data = dm2))

# 
# dm2$GPA <- as.numeric(dm2$CumGPA2191)
# summary(dm2$GPA)
sd(dm2$GPA, na.rm = TRUE) # 0.59

# Zoom plot to see spread
ggplot(dm2, aes(x = Q30, y = GPA)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Q25 ~ Q16, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# appears Q3, Q15, Q16 might be best predictors from what we have
# building the model
mod1 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191, Hess=FALSE)
summary(mod1) # AIC: 3189.663 

mod2 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191, Hess=TRUE)
summary(mod2) #AIC: 3189.663

mod3 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191 + dm2$Q25 + dm2$Career, 
             Hess=TRUE)
summary(mod3) #AIC: 3153.74
# The first is linear (.L), the second is quadratic (.Q), the third is cubic (.C), and so on. 
# choose mod4
mod4 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16, Hess = TRUE) 
summary(mod4) # AIC: 3362.268 

# 
mod5 <- polr(dm2$Q30 ~ dm2$Q15 + dm2$Q16, Hess = TRUE)
summary(mod5) #AIC: 3672.99 

mod6 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$Class.Standing, Hess = TRUE)
summary(mod6) # AIC: 3367.126 
# try a different model 
mod7 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$GPA + dm2$Class.Standing, Hess=TRUE)
summary(mod7) #AIC: 3393.234 

mod8 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$GPA, Hess=TRUE)
summary(mod8) #AIC: 3395.471

# check for multicollinearity
library(mctest)

# ggpairs(X) # impossible to interpret


# cor(dm2$GPA, as.numeric(dm2$Q3), method = "spearman")

# create a table to calculate p values
(ctable <- coef(summary(mod8)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))  # only click once

(ci <- confint(mod8)) # default method gives profiled CIs

confint.default(mod8) # CIs assuming normality

## odds ratios
exp(coef(mod8))

## OR and CI
exp(cbind(OR = coef(mod8), ci))

# Proportional odds assumption
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(dm2, summary(as.numeric(Q30) ~ Q3 + GPA, fun=sf)))

# turn into binary 
# code below estimates the effect of pared on choosing “unlikely” applying versus “somewhat likely” or “very likely”.

glm(I(as.numeric(Q30) >= 2) ~ Q3, family="binomial", data = dm2)

#code estimates the effect of pared on choosing “unlikely” or “somewhat likely” applying versus “very likely” applying.
glm(I(as.numeric(Q30) >= 3) ~ Q3, family="binomial", data = dm2)

glm(I(as.numeric(Q30) >= 4) ~ Q3, family="binomial", data = dm2)

### for GPA

glm(I(as.numeric(Q30) >= 2) ~ GPA, family="binomial", data = dm2)

#code estimates the effect of pared on choosing “unlikely” or “somewhat likely” applying versus “very likely” applying.
glm(I(as.numeric(Q30) >= 3) ~ GPA, family="binomial", data = dm2)

glm(I(as.numeric(Q30) >= 4) ~ GPA, family="binomial", data = dm2)


#
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print

# create a plot
plot(s, which=1:4, pch=1:4, xlab='logit', main=' ', xlim=range(s[,3:4]))

# create a new data frame for prediction
# can't get this to work
newdat <- data.frame(
   Q3 = rep(0:3, 609),
   GPA = rep(seq(from = 1, to = 4, length.out = 2436), 1))

newdat[nrow(newdat) + 1,] <- c("Very effective", "3.3")

newdat <- cbind(newdat, predict(mod8, newdat, type = "probs"))
# newdat <- data.frame(Q3=c(0:3, each = 609))
# (phat <- predict(object = mod8, newdat, type="p"))

lnewdat <- melt(newdat, id.vars = c("GPA","Q3"),
                variable.name = "Level", value.name="Probability")
# lnewdat[nrow(lnewdat) + 1,] <- c("Very effective", "3.3")
head(lnewdat)

ggplot(lnewdat, aes(x = GPA, y = Probability, colouar = Level)) +
  geom_line() 
#   facet_grid(Q3, labeller="label_both")
  # show first few rows

# check for NA's 
sum(is.na(dm2$Age))

# check for collinearity 
# https://stats.stackexchange.com/questions/288345/check-for-multicollinearity-ordinal-logistic-regression

# https://marissabarlaz.github.io/portfolio/ols/
library(tidyverse)
library(ordinal)
library(MASS)
library(ggeffects)
library(effects)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# # get significant results with this
# mod3 <- clm(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191, Hess=FALSE)
# summary(mod3)
# 
# # gets better results
# mod4 <- clm(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16)
# summary(mod4)  


# Glm

dm2_ugrad <- dm2 %>% group_by(Career) %>% 
  dplyr::filter(Career == "UGRD")

dm2_grad <- dm2 %>% group_by(Career) %>% 
  dplyr::filter(Career != "UGRD")

# add a new column
dm2_ugrad = mutate(dm2_ugrad, Q30_binary = (Q30 == 'Very likely'))

# # create the variable for students
# dm2$Students <- as.factor(dm2$Graduate)
dm2_ugrad$Q30_bin <- as.factor(dm2_ugrad$Q30_binary)
is.factor(dm2_ugrad$Q30_bin)

# dm2$Students <- recode_factor(dm2$Students, "TRUE" = "Graduate", "FALSE" = "Undergraduate") # this worked

# # create a glm 
# mod5 <- glm(dm2_ugrad$Q30_bin ~ dm2_ugrad$Q3 + dm2_ugrad$Q15 + dm2_ugrad$Q16 + 
#               dm2_ugrad$Q20 + dm2_ugrad$Q21 + dm2_ugrad$Q25,
#             data = dm2_ugrad)

# look for someone
grad_honors <- dm2 %>% group_by(Career) %>% 
  dplyr::filter(Career != "UGRD") %>%
  dplyr::filter(Honors.Flag == "Y")
  