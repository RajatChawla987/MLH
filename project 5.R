
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("naniar")
library(naniar)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

#Ques 1
install.packages("Ecdat")
library(Ecdat)

#ques 2
df<-view(MCAS)
df
#a. 
sum(complete.cases(df))
df2 <- data.frame(miss_var_summary(df))
df2

anyNA(df)
colSums(is.na(df))
# there are missing values in totsc8, spc and avgsalary columns. totsc8 has '40' missing values, spc has '9' and avgsalary has '25' missing values. 

#b.missing values could make a wrong pattern/trend in our regression model since our data would be missing out values which is required in finding the right trend, based on which we make our predictions using regression. Our model could overfit if we have missing values in our dataset. 

#c.
anyNA(df)
colSums(is.na(df))
#making a new dataframe df3 to store median values in place of NA values in 'spc' column. 
df3<-df
df3$spc[is.na(df3$spc)] <- median(df3$spc, na.rm = T)
df3

#d.
install.packages("simputation")
library(simputation)
install.packages("corrr")
library(corrr)

cor(df3[, unlist(lapply(df3, is.numeric))],df3$totsc8, use = "complete.obs")
#totsc4 displayed a correlation of 0.857 and lnchpct showed -0.8393. These two variables are the most strongly correlated to totsc8. A negative correlation means that when one variable increases, the other will decrease but it still proves that these two variables are strongly correlated or inversely proportiona. A positive correlation means one variable increases with increase in other variable or directly proportional. 
df3 <- df3 %>% mutate(is_missing = label_missings(totsc8)) %>% 
  impute_lm(totsc8 ~ lnchpct + percap)                

#e. 
cor(df3[, unlist(lapply(df3, is.numeric))],df3$avgsalary, use = "complete.obs")
#percap displayed a correlation of 0.620 and regday showed 0.518
df3 <- df3 %>% mutate(is_missing = label_missings(avgsalary)) %>% 
  impute_lm(avgsalary ~ percap + regday)  

#ques 3
#my seed value is 30. 
set.seed(30)
train <- sample_frac(df3, 0.60) 
valid <- setdiff(df3, train)

#Ques 4
plot1<- ggplot(train, aes(x=percap, y=totsc4)) + geom_point()
plot1+ geom_hline(yintercept=mean(train$totsc4), color="red") + geom_smooth(method="lm", se=FALSE, color="green")
#this plot suggests that there exists a positive correlation between percap and totsc4 which  means as percap increases, total 4th grade scores on MCAS increases. But the increase in totsc4 increases at a faster rate with a slight change in percap. By this we could state that these two variables are strongly correlated. However, the mean of totsc4 lies at 710. Best fit line shows a increasing positive slope as per out training set which again proves that we have a positive correlation between both the variables. A positive coefficient indicates that as the value of the independent variable increases, the mean of the dependent variable also tends to increase.Yes, this does make a intuitive sense to me as I can see from a stronger correlation and the best fit line that when the per capita income increases, the total 4th grade scores also increases through which I can state that if the income levels could be raised by government or corporates, we can likely expect more total 4th grade scores in future.

#ques 5

cor(train$totsc4, train$percap)
cor.test(train$totsc4, train$percap)
# yes both the variables share a strong correlation, which is 0.6005078. ALso, there is a positive correlation which means both the variables are directly proportional. This is a significant correlation because the p-value as per the cor.test() is lesser than 0.05.

#ques 6
model<- lm(totsc4~percap, data=train)
summary(model)

#ques 7
#a. 
train$Residuals <- model$residuals

train[train$Residuals == max(train$Residuals),]

#b. 
train[train$Residuals == min(train$Residuals),]
# ques 8
install.packages("forecast")
library(forecast)
pred1<-predict(model, train)
pred2<-predict(model, valid)
#ques 9
accuracy(pred1, train$totsc4)
accuracy(pred2, valid$totsc4)

#ques 10

sd(train$totsc4)

#Multiple Linear Regression
#Ques1
#a.
str(df3)
?MCAS
df4<-df3
df4$code <- as.factor(df4$code)
str(df4)

#c.
unique_code<- unique(df4$code)
unique_municipa<- unique(df4$municipa)
unique_district<- unique(df4$district)

Uniqueness_quotient1<-length(unique_code)/nrow(df4)
Uniqueness_quotient2<-length(unique_municipa)/nrow(df4)
Uniqueness_quotient2<-length(unique_district)/nrow((df4))

#ques 2
train$code<- as.factor(train$code)
str(train)
cor(train[, unlist(lapply(train, is.numeric))], use = "complete.obs")
train2<-train[,-c(8)]

#ques 3
install.packages("stats")
library(stats)

train2<-train2[,-c(18)]

model2 <- lm(totsc4~regday+specneed+bilingua+occupday+spc+speced+lnchpct+tchratio+percap+totsc8+avgsalary+pctel, train2) 
model2 <- step(model2, direction = "backward")
predict(model2,newdata = data.frame(percap = c(20)))
#a.
summary(model2)

#ques 4
install.packages("car")
library(car)
vif(model2)

lnchpct_vif<-lm(lnchpct~tchratio+totsc8, train2)
summary(lnchpct_vif)


  