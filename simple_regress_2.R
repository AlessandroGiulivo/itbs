# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  06/05/2020 *                             #
#                                                                           #
# ************************************************************************* #

# Authors: Dr. Mario Fordellone
# University of Rome La Sapienza
# mail: mario.fordellone@uniroma1.it

# Require packages --------------------------------------------------------

# install.packages("corrplot")
library(corrplot)
library(MASS)

# Import and analyse data -------------------------------------------------

data <- read.table(file = "diabete.txt", header = TRUE)
dim(data)
head(data)
str(data)
data <- data[,-9]
head(data)
c <- cor(x = data)
corrplot(corr = c, method = "pie", type = "lower")
attach(data)

# Simple linear regression: OLS estimates ---------------------------------

?lm
mod1 <- lm(formula = Glucose ~ Age, data = data)
summary(mod1)
plot(x = Age,y = Glucose, pch = 1, lwd = 2, col = "blue")
abline(a = mod1, lwd = 2, col = "red", lty = 2)

mod2 <- lm(formula = Glucose ~ BMI, data = data)
summary(mod2)

mod3 <- lm(formula = Glucose ~ Insulin, data = data)
summary(mod3)

mod4 <- lm(formula = Glucose ~ BloodPressure, data = data)
summary(mod4)

par(mfrow = c(2,2))
plot(x = Age,y = Glucose, pch = 1, lwd = 2, col = "blue")
abline(a = mod1, lwd = 2, col = "red", lty = 2)
text(x = 65, y = 10, labels = "y = 97.08 + 0.71x", col = "red")
plot(x = BMI, y = Glucose, pch = 1, lwd = 2, col = "blue")
abline(a = mod2, lwd = 2, lty = 2, col = "red")
text(x = 50, y = 10, labels = "y = 92.21 + 0.90x", col = "red")
plot(x = Insulin, y = Glucose, pch = 1, lwd = 2, col = "blue")
abline(a = mod3, lwd = 2, lty = 2, col = "red")
text(x = 600, y = 10, labels = "y = 113.60 + 0.09x", col = "red")
plot(x = BloodPressure, y = Glucose, pch = 1, lwd = 2, col = "blue")
abline(a = mod4, lwd = 2, lty = 2, col = "red")
text(x = 80, y = 10, labels = "y = 103.48 + 0.25x", col = "red")
