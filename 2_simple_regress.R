# ************************************************************************* #
#                                                                           #
#             ** Introduction to Biomedical Statistics II **                #
#                     Bachelor Degree in Bioinformatics                     #
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

data <- read.table(file = "data.txt", header = TRUE)
summary(data)
dim(data)
Sx <- cov(data)
Cx <- cor(data)
corrplot(corr = Cx, method = "pie", type = "lower")
Zx <- scale(x = data, center = TRUE, scale = TRUE)
summary(Zx)
Cz <- cov(Zx)
attach(data)

# Simple linear regression: OLS estimates ---------------------------------

?lm
mod1 <- lm(formula = Running_minutes ~ Age, data = data)
summary(mod1)
mod2 <- lm(formula = Zx[,2] ~ Zx[,1], data = data)
summary(mod2)

plot(x = Age, y = Running_minutes, pch = 19, lwd = 2, col = "blue")
abline(mod1, lwd = 2, col = "red", lty = 2)
text(x = 55, y = 50, labels = "y = 77.62 - 0.94x", lwd = 2, col = "red", cex = 1.5)

mod3 <- lm(formula = height ~ weight, data = data)
summary(mod3)
mod4 <- lm(formula = Zx[,3] ~ Zx[,4], data = data)
summary(mod4)

plot(weight, height, pch = 19, lwd = 1, col = 'green')
abline(mod3, lwd = 2, col = "orange", lty = 1)
text(x = 61, y = 190, labels = "y = 128.2918 - 0.9044x", lwd = 2, col = "black", cex = 1.5)
View(data)

mod5 <- lm(formula = height ~ Running_minutes, data = data)
summary(mod5)
plot(Running_minutes, height, pch = 19, lwd = 1, col = 'green')
abline(mod5, lwd = 2, col = "orange", lty = 1)
text(x = 61, y = 190, labels = "y = 128.2918 - 0.9044x", lwd = 2, col = "black", cex = 1.5)
