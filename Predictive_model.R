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
# mod1 <- glm(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191)
# summary(mod1)

mod1 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191)
summary(mod1)

mod2 <- polr(dm2$Q30 ~ dm2$Q3 + dm2$Q15 + dm2$Q16 + dm2$CumGPA2191, Hess=FALSE)
summary(mod2)

# https://marissabarlaz.github.io/portfolio/ols/
library(tidyverse)
library(ordinal)
library(MASS)
library(ggeffects)
library(effects)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
