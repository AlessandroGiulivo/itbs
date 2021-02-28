# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  07/05/2020 *                             #
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

n  = 100
b0 = -75
b1 = 0.85
b2 = -0.97

x <- cbind(rep(x = 1, times = n), rnorm(n = n, mean = 0, sd = 1), rnorm(n = n, mean = 0, sd = 1))
head(x)
e <- rnorm(n = n, mean = 0, sd = 0.01)
y = b0 + b1*x[,2] + b2*x[,3] + e
pairs(x = cbind(y, x[,2], x[,3]), lwd = 2, labels = c("y", "x1", "x2"))

# OLS estimation ----------------------------------------------------------

B = inv(t(x)%*%x)%*%t(x)%*%y
