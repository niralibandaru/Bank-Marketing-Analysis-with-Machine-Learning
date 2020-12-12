#* Nirali Bandaru
#* CPSC6300 - Applied Data Science
#* Prof. Alexander Herzog
#* Clemson University
#* Purpose: Individual Project for 6300 level credit
#* Data Due: December 2, 2020

list(rm=ls())

#LOAD LIBRARIES

library(ggplot2)
library(dplyr)
library(randomForest)
library(boot)
library(caret)
library(e1071)
library(tree)
library(scales)
library(randomForest)
library(ROCR)
library(DMwR)

#LOAD DATA

mydata <- read.csv("bank-full.csv", stringsAsFactors = TRUE)
df <- data.frame(mydata)
attach(mydata)
#View(mydata)

#EXPLORATORY DATA ANALYSIS

#Data Cleaning

any(is.na(mydata))

#no na values
#no unwanted columns

#examine summary of variables

summary(mydata)

table(job)
table(marital)
table(education)
table(default)
table(housing)
table(loan)
table(contact)
table(month)
table(poutcome)
table(day)
table(y)

#visual summary of quantitative predictors

#AGE

age_plot <- ggplot(data = mydata, mapping = aes(x = age)) + 
                  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Age") + theme(plot.title = element_text(hjust = 0.5))
age_plot

#BALANCE

balance_plot <- ggplot(data = mydata, mapping = aes(x = balance)) + 
  geom_histogram(color = "black", fill = "lightblue") + 
  ggtitle("Balance") + theme(plot.title = element_text(hjust = 0.5))
balance_plot

balance_plot <- ggplot(data = mydata, mapping = aes(x = balance)) + 
  geom_boxplot(color = "black", fill = "lightblue") + 
  ggtitle("Balance") + theme(plot.title = element_text(hjust = 0.5))
balance_plot

balance_plot <- ggplot(data = mydata, mapping = aes(x = balance)) + 
  geom_density(color = "black", fill = "lightblue") + 
  ggtitle("Balance") + theme(plot.title = element_text(hjust = 0.5))
balance_plot

#DAY

day_plot <- ggplot(data = mydata, mapping = aes(x = day)) + 
  geom_histogram(color = "black", fill = "limegreen") + 
  ggtitle("Day") + theme(plot.title = element_text(hjust = 0.5))
day_plot


#PDAYS

pday_plot <- ggplot(data = mydata, mapping = aes(x = pdays)) + 
  geom_histogram(color = "black", fill = "brown") + 
  ggtitle("Days Passed Since Last Contact") + theme(plot.title = element_text(hjust = 0.5))
pday_plot


pday_plot <- ggplot(data = mydata, mapping = aes(x = pdays)) + 
  geom_boxplot(color = "black", fill = "brown") + 
  ggtitle("Days Passed Since Last Contact") + theme(plot.title = element_text(hjust = 0.5))
pday_plot

#DURATION

duration_plot <- ggplot(data = mydata, mapping = aes(x = duration)) + 
  geom_histogram(color = "black", fill = "lightyellow") + 
  ggtitle("Duration") + theme(plot.title = element_text(hjust = 0.5))
duration_plot

duration_plot <- ggplot(data = mydata, mapping = aes(x = duration)) + 
  geom_boxplot(color = "black", fill = "lightyellow") + 
  ggtitle("Duration") + theme(plot.title = element_text(hjust = 0.5))
duration_plot

#CAMPAIGNS

campaign_plot <- ggplot(data = mydata, mapping = aes(x = campaign)) + 
  geom_histogram(color = "black", fill = "purple") + 
  ggtitle("Campaign") + theme(plot.title = element_text(hjust = 0.5))
campaign_plot

campaign_plot <- ggplot(data = mydata, mapping = aes(x = campaign)) + 
  geom_boxplot(color = "black", fill = "purple") + 
  ggtitle("Campaign") + theme(plot.title = element_text(hjust = 0.5))
campaign_plot

#PREVIOUS

previous_plot <- ggplot(data = mydata, mapping = aes(x = previous)) + 
  geom_histogram(color = "black", fill = "orange") + 
  ggtitle("Previous") + theme(plot.title = element_text(hjust = 0.5))
previous_plot

previous_plot <- ggplot(data = mydata, mapping = aes(x = previous)) + 
  geom_boxplot(color = "black", fill = "orange") + 
  ggtitle("Previous") + theme(plot.title = element_text(hjust = 0.5))
previous_plot

#JOB

job_count <- mydata %>% count(job)
job_plot <- ggplot(data = mydata, mapping = aes(x = job)) + 
  geom_bar(color = "black", fill = "cyan") + ggtitle("Job") + theme(plot.title = element_text(hjust = 0.5))
job_plot

#MONTH

month_plot <- ggplot(data = mydata, mapping = aes(x = month)) + 
  geom_bar(color = "black", fill = "cornflowerblue") + 
  ggtitle("Month") + theme(plot.title = element_text(hjust = 0.5))
month_plot

#MARITAL STATUS

marital_plot <- ggplot(data = mydata, mapping = aes(x = marital)) + 
  geom_bar(color = "black", fill = "maroon") + ggtitle("Marital Status") + theme(plot.title = element_text(hjust = 0.5))
marital_plot

#EDUCATION

ed_plot <- ggplot(data = mydata, mapping = aes(x = education)) + 
  geom_bar(color = "black", fill = "purple") + ggtitle("Education") + theme(plot.title = element_text(hjust = 0.5))
ed_plot

#DEFAULT

default_plot <- ggplot(data = mydata, mapping = aes(x = default)) + 
  geom_bar(color = "black", fill = "violet") + ggtitle("Default Status") + theme(plot.title = element_text(hjust = 0.5))
default_plot

#HOUSING

housing_plot <- ggplot(data = mydata, mapping = aes(x = housing)) + 
  geom_bar(color = "black", fill = "turquoise") + ggtitle("Housing Loan Status") + theme(plot.title = element_text(hjust = 0.5))
housing_plot

#LOAN

loan_plot <- ggplot(data = mydata, mapping = aes(x = loan)) +
  geom_bar(color = "black", fill = "pink") + ggtitle("Loan Status") + theme(plot.title = element_text(hjust = 0.5))
loan_plot

#CONTACT

contact_plot <- ggplot(data = mydata, mapping = aes(x = contact)) +
  geom_bar(color = "black", fill = "darkblue") + ggtitle("Contact Method") + theme(plot.title = element_text(hjust = 0.5))
contact_plot

#OUTCOME

out_plot <-  ggplot(data = mydata, mapping = aes(x = poutcome)) +
  geom_bar(color = "black", fill = "darkgreen") + ggtitle("Previous Campaign Outcome") + theme(plot.title = element_text(hjust = 0.5))
out_plot

#Y

y_plot <- ggplot(data = mydata, mapping = aes(x = y)) +
  geom_bar(color = "black", fill = "darkred") + ggtitle("Campaign Outcome") + theme(plot.title = element_text(hjust = 0.5))
y_plot


######### MACHINE LEARNING MODEL IMPLEMENTATION ##########

#DECISION TREES- RANDOM FOREST

set.seed(1)

N <- nrow(mydata)

training <- sample(1:N, 0.7*N)
testing <- seq(1:N)[-training]

rf <- randomForest(y ~ .,data=df, mtry = 4, subset = training, importance=TRUE)
summary(rf)
yhat.rf <- predict(rf,df[testing,])
df.testing <- df[testing,"y"]

yhat.predict <- predict(rf, df[testing,])

importance(rf)
print(rf)

# Plotting importance

varImpPlot(rf)

# ROC Curve Generation

pred <- predict(rf, newdata = df[testing,],type = "prob")

perf <- prediction(pred[,2], df[testing,]$y)

auc = performance(perf,"auc")
#auc

pred1 <- performance(perf, "tpr", "fpr")

plot(pred1, main = "ROC Curve for RF MODEL", col = 2, lwd = 2)

abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")


#Balancing the data

table(df[training,]$y)

control = trainControl(
  method = 'cv',
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

train_smote <- SMOTE(y ~ ., df[training,], perc.over = 100, perc.under = 200)

table(train_smote$y)

rf <- randomForest(y ~ .,data=train_smote, mtry = 4, importance=TRUE)
summary(rf)
yhat.rf <- predict(rf,df[testing,])
df.testing <- df[testing,"y"]

yhat.predict <- predict(rf, df[testing,])

importance(rf)
print(rf)

# Plotting importance

varImpPlot(rf)

# ROC Curve Generation

pred <- predict(rf, newdata = df[testing,],type = "prob")

perf <- prediction(pred[,2], df[testing,]$y)

auc = performance(perf,"auc")
#auc

pred1 <- performance(perf, "tpr", "fpr")

plot(pred1, main = "ROC Curve for RF MODEL", col = 2, lwd = 2)

abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")


