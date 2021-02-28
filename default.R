# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  19/05/2020 *                             #
#                                                                           #
# ************************************************************************* #

# Authors: Dr. Mario Fordellone
# University of Rome La Sapienza
# mail: mario.fordellone@uniroma1.it

# Require packages --------------------------------------------------------

# install.packages("corrplot")
library(corrplot)
library(MASS)
# library(matlib)
library(ISLR)

# Require dataset ---------------------------------------------------------

data()
data("Default")
attach(Default)
summary(Default)

par(mfrow=c(2,2))
boxplot(balance ~ student, lwd = 2, col = "blue")
boxplot(income ~ student, lwd = 2, col = "blue")
boxplot(balance ~ default, lwd = 2, col = "red")
boxplot(income ~ default, lwd = 2, col = "red")

y = income
x = student

mod <- lm(formula = income ~ student, data = Default)
summary(mod)
