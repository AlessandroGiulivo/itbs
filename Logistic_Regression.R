# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  20/05/2020 *                             #
#                                                                           #
# ************************************************************************* #

# Authors: Dr. Mario Fordellone
# University of Rome La Sapienza
# mail: mario.fordellone@uniroma1.it

# Download packages -------------------------------------------------------

install.packages("ISLR")
install.packages("corrplot")
library(ISLR)
library(MASS)
library(corrplot)

# Import and analyze data -------------------------------------------------

data(Default)
attach(Default)

head(Default)
summary(Default)
str(Default)

par(mfrow=c(2,2))
boxplot(balance ~ default, col = c(2,3), lwd = 2, xlab = "Default", ylab = "Balance")
boxplot(income  ~ default, col = c(2,3), lwd = 2, xlab = "Default", ylab = "Income")
boxplot(balance ~ student, col = c(2,3), lwd = 2, xlab = "Student", ylab = "Balance")
boxplot(income  ~ student, col = c(2,3), lwd = 2, xlab = "Student", ylab = "Income")

# Example  ----------------------------------------------------------------

pi_1     <- exp(-0.5+0.05*c(-100:100))/(1+ exp(-0.5+0.05*c(-100:100)))
pi_2     <- exp(-0.5+0.15*c(-100:100))/(1+ exp(-0.5+0.15*c(-100:100)))
pi_3     <- exp(-0.5+0.30*c(-100:100))/(1+ exp(-0.5+0.30*c(-100:100)))
pi_1n    <- exp(-0.5-0.05*c(-100:100))/(1+ exp(-0.5-0.05*c(-100:100)))
pi_2n    <- exp(-0.5-0.15*c(-100:100))/(1+ exp(-0.5-0.15*c(-100:100)))
pi_3n    <- exp(-0.5-0.30*c(-100:100))/(1+ exp(-0.5-0.30*c(-100:100)))

par(mfrow=c(1,2))
plot(x = c(-100:100), y = pi_1, type = "l", lwd = 2, col = "blue", xlab = "Predictor", ylab = "Probability")
lines(x = c(-100:100), y = pi_2, lwd = 2, col = "red")
lines(x = c(-100:100), y = pi_3, lwd = 2, col = "green")
legend("topleft", legend=c(expression(paste(beta, "=", 0.05)), expression(paste(beta, "=", 0.15)), 
                           expression(paste(beta, "=", 0.30))), lty = 1, col=c("blue", "red", "green"), cex=1.2, lwd =2, bty = "n") 
plot(x = c(-100:100), y = pi_1n, type = "l", lwd = 2, col = "blue", xlab = "Predictor", ylab = "Probability")
lines(x = c(-100:100), y = pi_2n, lwd = 2, col = "red")
lines(x = c(-100:100), y = pi_3n, lwd = 2, col = "green")
legend("topright", legend=c(expression(paste(beta, "=", -0.05)), expression(paste(beta, "=", -0.15)), 
                            expression(paste(beta, "=", -0.30))), lty = 1, col=c("blue", "red", "green"), cex=1.2, lwd =2, bty = "n") 
