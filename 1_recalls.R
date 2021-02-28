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
