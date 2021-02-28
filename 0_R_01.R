# ************************************************************************* #
#                                                                           #
#                ** Bachelor Degree in Bioinformatics **                    #
#                Introduction to Biomedical Statistics II                   #
#           *** Association between quantitative variables ***              #
#                                                                           #
# ************************************************************************* #

# Authors: Mario Fordellone
# Sapienza University of Rome
# mail: mario.fordellone@uniroma1.it 

# Download packages -------------------------------------------------------

#install.packages("matlib")
library("matlib")

# Application -------------------------------------------------------------

?sample
height <- sample(x = seq(from = 150, to = 190, by = 0.5), size = 100, replace = TRUE)
summary(height)
hist(height)

a = -65
b = 0.80
w = sample(x = seq(from = -5, to = 5, by = 0.01), size = 100, replace = TRUE)
  
weight <- a + b*height + w 
summary(weight)
hist(weight)
cor(height, weight)

?lm
mod <- lm(formula = weight ~ height)
summary(mod)

plot(x = height, y = weight, pch = 19, col = "blue", lwd = 2)
abline(mod, col = "red", lwd = 2)
text(x = 160, y = 85, labels = "y = -66-75 + 0.81x", cex = 1.5, col = "red")

x = cbind(rep(x = 1, times = 100), height)
y = weight

B = inv(t(x)%*%x)%*%t(x)%*%y
