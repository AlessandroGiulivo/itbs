# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  13/05/2020 *                             #
#                                                                           #
# ************************************************************************* #

# Authors: Dr. Mario Fordellone
# University of Rome La Sapienza
# mail: mario.fordellone@uniroma1.it

# Require packages --------------------------------------------------------

# install.packages("corrplot")
library(corrplot)
library(MASS)
library(matlib)

# Simulate data -----------------------------------------------------------

y <- rnorm(n = 100, mean = 25, sd = 3)

x1 <-  0.3*y + rnorm(n = 100, mean = 0, sd = 1.5)
x2 <- -0.7*y + rnorm(n = 100, mean = 0, sd = 1.5)
x3 <-  0.5*y + rnorm(n = 100, mean = 0, sd = 1.5)
x4 <-  rnorm(n = 100, mean = 10, sd = 1.5)
pairs(x = cbind(y, x1, x2, x3, x4), pch = 19, col = "blue")

# OLS estimates -----------------------------------------------------------

?lm

mod1 <- lm(formula = y ~ x1 + x2 + x3 + x4)
summary(mod1)

mod2 <- lm(formula = y ~ x1 + x2 + x3)
summary(mod2)

RSS1 <- sum(mod1$residuals^2)
RSS2 <- sum(mod2$residuals^2)

F  = ((RSS2-RSS1)/1)/(RSS1/(100-4-1))
pv = 1-pf(q = F, df1 = 1, df2 = (100-4-1))