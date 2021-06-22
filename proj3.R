#Jaineel Upadhyay

library(RMySQL)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)

# Connecting to MySql Database.
# Note: Database not available anymore.
con <- dbConnect(MySQL(),
                 user = 'root',
                 password = '',
                 host = 'localhost',
                 dbname = 'proj3',
                 dbtable = 'lending')


lendingTable <- dbReadTable(con, "lending")

lendingTable$residence_property <- (ifelse(lendingTable$residence_property == "Own",0, 1))

# For machine learning 
lendingTable2 <- lendingTable[,-1]
lendingTable2 <- lendingTable2[, c(2,3,4,5,6,7,8,9,10,11,12,1)]

# Descriptive Analysis
corMatrix <- cor(lendingTable, use = "complete.obs")
View(corMatrix)

#Since based on correlation analysis, the three fields with strongest positive/negative correlation are loan_amnt, adjusted_annual_inc, and pct_loan_income, those are the
#three fields to be used for descriptive analysis.
#cor(loan_amnt, pct_loan_income) = 0.544
#cor(loan_amnt, adjusted_annual_income) = 0.349
#cor(pct_loan_income, adjusted_annual_income) = -0.284
# Loan amount people have based on their income.Can be an indicator of debt levels.
ggplot(data = lendingTable, mapping = aes(x = loan_amnt, y = adjusted_annual_inc)) + geom_point()

ggplot(data = lendingTable, mapping = aes(x = loan_amnt)) + geom_histogram(stat = "bin", bins = 30)
#ggplot(data = lendingTable, mapping = aes(x = adjusted_annual_inc, y = loan_amnt)) + geom_boxplot(outlier.shape = NA)

# Percentage of Loan income field and loan amount field, provides interesting observations, as to how much of their income people havev as debt.
ggplot(data = lendingTable, mapping = aes(x = pct_loan_income, y = loan_amnt)) + geom_point()

describe(lendingTable)

# Machine Learning  ##### Naive Bayes model 
##############################################################################################################
# Multiple Regression 
mRegression <- lm(loan_amnt ~ pct_loan_income + adjusted_annual_inc, data = lendingTable)
print(mRegression)
coefficients(mRegression)

summary(mRegression)

# Regression Equation
# y[loan_amount] = -1672.426 + (49876.66)x2 + (0.071539)x3 


##############################################################################################################
# Naive Bayes Model
##############################################################################################################
# Making loan_default the outcome variable and changing its values to a booleans since its a predictor variable.
lendingTable2$loan_default <- factor(lendingTable2$loan_default, levels = c(0,1), labels = c("False", "True"))

ggplot(lendingTable2, aes(loan_amnt, colour = loan_default)) + geom_freqpoly(binwidth = 100)

ggplot(lendingTable2, aes(x = open_acc , fill = loan_default, colour = loan_default)) + geom_histogram(binwidth = 4) + theme_bw()

# Data Splicing 
indxTrain <- createDataPartition(y = lendingTable2$loan_default, p = 0.75, list = FALSE)
training <- lendingTable2[indxTrain,]
testing <- lendingTable2[-indxTrain,]

prop.table(table(training$loan_default)) * 100
prop.table(table(testing$loan_default)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-12]
y = training$loan_default

library(e1071)

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model
#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) #Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, testing$loan_default)


#Plot Variable performance
x <-varImp(model)
plot(x)r
