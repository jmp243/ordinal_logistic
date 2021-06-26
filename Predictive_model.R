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
dm2$GPA <- as.numeric(dm2$CumGPA2191)
# summary(dm2$GPA)
sd(dm2$GPA, na.rm = TRUE) # 0.59

# Zoom plot to see spread
ggplot(dm2, aes(x = GPA, y = Q30)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Q25 ~ Q16, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# appears Q3, Q15, Q16 might be best predictors from what we have
# building the model
# mod1 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191, Hess=FALSE)
# summary(mod1) # AIC: 3189.663 

mod1 <- polr(dm2$Q30 ~ dm2$Q3, Hess = TRUE)
summary(mod1)

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
mod7 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Gender + dm2$Class.Standing, Hess=TRUE)
summary(mod7) 

mod8 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$GPA, Hess=TRUE)
summary(mod8) #AIC: 3395.471

mod9 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$GPA, Hess = TRUE)
summary(mod9) # AIC: 3224.221 

mod10 <- polr(Q30 ~ Q3 + Q15 + Q20 + Q25 + Age, data = dm2, Hess = TRUE)
summary(mod10)

mod11 <- polr(Q30 ~ Q3 + Q15 + Q21 + Q25 + Age, data = dm2, Hess = TRUE)
summary(mod11)


mod12 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$Q25, Hess = TRUE) 
summary(mod12) #AIC: 3325.953 

mod13 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Age + dm2$Q25, Hess = TRUE) 
summary(mod13) #AIC: 3357

# 
# mod7 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Gender + dm2$Class.Standing, Hess=TRUE)
# summary(mod7) 
# Anova test of variance
# analysis of deviance
Anova(model,
      type = "II")

library(splines)
Anova(mod3) # not signif is GPA
Anova(mod4) # all signif
Anova(mod7)
Anova(mod8) # Anova shows GPA is not significant
Anova(mod9) # with Q15, 20, 25, Gender all sign
Anova(mod10) # Age not signif
Anova(mod11) #Q21 not signif, age not signif
Anova(mod12) # all signif

# check for multicollinearity
library(mctest)

# ggpairs(X) # impossible to interpret


# cor(dm2$GPA, as.numeric(dm2$Q3), method = "spearman")

# create a table to calculate p values
(ctable <- coef(summary(mod9)))

(ctable <- coef(summary(mod4)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2


## combined table
(ctable <- cbind(ctable, "p value" = p))  # only click once

(ci <- confint(mod9)) # default method gives profiled CIs

confint.default(mod9) # CIs assuming normality

# for a different model 
(ci <- confint(mod4)) # default method gives profiled CIs

confint.default(mod4) # CIs assuming normality
## odds ratios
exp(coef(mod9))

## OR and CI
exp(cbind(OR = coef(mod9), ci))

# for another model 12
exp(coef(mod12))

## OR and CI
exp(cbind(OR = coef(mod12), ci))

# Proportional odds assumption
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(dm2, summary(as.numeric(Q30) ~ Q3 + Q15 + Q25, fun=sf))) # for other relationships

plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
# turn into binary 
# code below estimates the effect of pared on choosing “unlikely” applying versus “somewhat likely” or “very likely”.

glm(I(as.numeric(Q30) >= 2) ~ Q3 + Q15 + GPA, family="binomial", data = dm2)

# or try
glm(I(as.numeric(Q30) >= 2) ~ Q3 + Q15 + Q25, family="binomial", data = dm2)

glm(I(as.numeric(Q30) >= 3) ~ Q3 + Q15 + Q25, family="binomial", data = dm2)
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
# newdat <- data.frame(
#    # Q3 = rep(0:3, 200),
#    # GPA = rep(seq(from = 1, to = 4, length.out = 2436), 1))
# GPA = rep(seq(from = 1, to = 4), 1))

# newdat[nrow(newdat) + 1,] <- c("Very effective", "3.3")
# 
# newdat <- cbind(newdat, predict(mod9, newdat, type = "probs"))
newdat <- data.frame(Q3=c(0,1,2))
(phat <- predict(object = mod1, newdat, type="p")) # lots of warnings

# 
# lnewdat <- melt(newdat, id.vars = c("GPA","Q3"),
#                 variable.name = "Level", value.name="Probability")
# # lnewdat[nrow(lnewdat) + 1,] <- c("Very effective", "3.3")
# head(lnewdat)
# 
# ggplot(lnewdat, aes(x = GPA, y = Probability, colouar = Level)) +
#   geom_line() 
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

#### chi sq contingency table
library(janitor)
dm2 %>% 
  tabyl(Q30, Gender) %>% 
  chsq.test()

#### likert model ##
# https://rcompanion.org/handbook/G_07.html
library(ordinal)
model = clm(Q30 ~ GPA + Q3,
            data = dm2)
library(car)
library(RVAideMemoire)


# partition the data for training and test
# https://www.youtube.com/watch?v=qkivJzjyHoA

ind <- sample(2, nrow(dm2), replace = TRUE, prob = c(0.8,0.2))

train <- dm2[ind==1,]
test <- dm2[ind==2,]

# polr
mod8a <- polr(Q30 ~ Q3 + Q15 + GPA + Age, data = train, Hess = TRUE)
summary(mod8a)


# mod9 <- polr(Q30 ~ Q3 + Q15+ Q16+ Q20 + Q21+ Q22+ Q25 + GPA + Career + Gender + Age + Ethnicity, data = train, Hess = TRUE)
# summary(mod9)

mod9 <- polr(Q30 ~ Q15+ Q20 + Q25 + Gender, data = train, Hess = TRUE)
summary(mod9) 

mod12 <- polr(Q30 ~ Q3 + Q15 + Q16 + Q25, data=train, Hess = TRUE) 
summary(mod12)

# get p values
(ctable <- coef(summary(mod8a)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# for mod12
(ctable <- coef(summary(mod12)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# for model 9 
(ctable2 <- coef(summary(mod9)))
p <- pnorm(abs(ctable2[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable2, "p value" = p))

# prediction
# pred <- predict(mod8a, train[1:5,], type = 'prob')
pred <- predict(mod8a, train)

pred <- predict(mod12, train)
# pred <- predict(mod8, train)
pred
print(pred, digits = 3)

# pred2 <- predict(mod9, train[1:5,], type = 'prob')
pred2 <- predict(mod12, train)
pred2
print(pred2, digits = 3)

# pred2 <- predict(mod12)
# pred2
# confusion matrix and error for training data
(tab <- table(pred, train$Q30))

# (tab2 <- table(pred2, train$Q30))
# misclassification error at 36%
1-sum(diag(tab))/sum(tab)


# confusion matrix and error for test data
pred1 <- predict(mod8, test)
(tab1 <- table(pred1, test$Q30))

pred1 <- predict(mod12, test)
(tab1 <- table(pred1, test$Q30))
# misclassification error of about 39%
1-sum(diag(tab1))/sum(tab1) 

## odds ratios
exp(coef(mod12))

## OR and CI
exp(cbind(OR = coef(mod12), ci))

# https://data.library.virginia.edu/fitting-and-interpreting-a-proportional-odds-model/
# https://data.library.virginia.edu/visualizing-the-effects-of-proportional-odds-logistic-regression/

# proportional odds model 
# mod9 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$GPA, Hess = TRUE)
# summary(mod9) # AIC: 3224.221 
# 
# mod10 <- polr(Q30 ~ Q3 + Q15 + Q20 + Q25 + Age, data = dm2, Hess = TRUE)
# summary(mod10)
# 
# mod11 <- polr(Q30 ~ Q3 + Q15 + Q21 + Q25 + Age, data = dm2, Hess = TRUE)
# summary(mod11)

# # predict an outcome
# predict(mod10, newdata = data.frame(Q25="Yes"), type = "p")

# visualizing 
library(effects)
# Effect(focal.predictors = c("Q3","Q15"), mod9)
# Effect(focal.predictors = c("Q3","Class.Standing"), mod7, data = dm2)

Effect(focal.predictors = c("Q25","Q3"), mod10)

Effect(focal.predictors = c("Age","Q3"), mod11)

# plot 
plot(Effect(focal.predictors = c("Q25","Q3"), mod10), rug = FALSE)

plot(Effect(focal.predictors = c("Age", "Q3"), mod11), rug = FALSE)

# cumulative probabilities
exp(mod4$zeta - mod4$coefficients)/(1 + exp(mod4$zeta - mod4$coefficients))

exp(mod4$zeta)/(1 + exp(mod4$zeta))

variable <- exp(mod4$zeta - mod4$coefficients)/(1 + exp(mod4$zeta - mod4$coefficients))

outcome <- exp(mod4$zeta)/(1 + exp(mod4$zeta))

# variables
(variable/(1-variable))

# outcome
(outcome/(1-outcome))

# # log of odds ratio
# log((outcome/(1-outcome))/(variable/(1-variable)))
