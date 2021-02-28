# ************************************************************************* #
#                                                                           #
#                ** Introduction to Biomedical Statistics II **             #
#                        Bachelor Degree in Bioinformatics                  #
#                               *  22/05/2020 *                             #
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

# Import dataset and sampling ---------------------------------------------

data <- read.table(file = "diabete.txt", header = TRUE)
dim(data)
str(data)
data[,9] <- as.factor(data[,9])
data[,9]

set.seed(999) #your id student
idx <- sample(x = (1:dim(data)[1]), size = 300, replace = FALSE) # n = 150
mydata <- data[idx,]
colnames(mydata) <- colnames(data)
dim(mydata)
summary(mydata)
c <- cor(x = mydata[,c(1:8)])
par(mfrow=c(1,1))
corrplot(corr = c, method = "pie", type = "lower")
attach(mydata)

# Logistic regression model -----------------------------------------------

par(mfrow=c(3,3))
for (i in 1:8){
  boxplot(mydata[,i] ~ Outcome, lwd = 2, col = "grey", ylab = colnames(mydata[i]))
}

?glm
model <- glm(formula = Outcome ~., family = binomial, data = mydata)
summary(model)

new_model <- glm(formula = Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, family = binomial, data = mydata)
summary(new_model)

# Assessing the accuracy of the model -------------------------------------

predict <- new_model$fitted.values

pred_table <- table(mydata$Outcome, predict > 0.5) # 0.3, 0.8

Acc <- sum(diag(pred_table))/sum(pred_table)
Mis <- 1-Acc

Sen <- pred_table[2,2]/sum(pred_table[2,])
Spe <- pred_table[1,1]/sum(pred_table[1,])

Eva <- round(matrix(data = c(Acc, Mis, Sen, Spe), nrow = 4, ncol = 1),3)
row.names(Eva) <- c("Overall Accuracy", "Mis-classification Rate", "Sensitivity","Specificity")
