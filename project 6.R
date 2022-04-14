install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("naniar")
library(naniar)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
install.packages("caret")
library(caret)

#Ques 1
top1<- read.csv("/Users/swati/Desktop/top2020_21.csv")
#choosing 'Bad Habits' as my song from the song.name

#Ques 2
top2<- filter(top1, Song.Name=="Bad Habits")
str(top2)
chosen_song <- data.frame(danceability=0.808, duration_ms=231041, liveness=0.364, loudness=-3.712, speechiness=0.0348, valence=0.591)
#Ques 3

spotify1<- read.csv("/Users/swati/Desktop/spotify.csv")
str(spotify1)

#a. 
spotify1$target <- as.factor(spotify1$target)
str(spotify1)

#b. 
unique_target<- unique(spotify1$target)
spotify3<- filter(spotify1, target==1)
spotify4<- filter(spotify1, target==0)
nrow(spotify3)
nrow(spotify4)

#Ques 4
anyNA(spotify1)
colSums(is.na(spotify1))
summary(spotify1)

#ques 5
spotify5<- spotify1[-c(1, 10)]
str(spotify5)
#Ques 6
set.seed(30)
train <- sample_frac(spotify5, 0.60) 
valid <- setdiff(spotify5, train)

#Ques 7
#a. 
train2<-train

#b. 
George_liked_training<-filter(train2, target==1)
George_notliked_training<- filter(train2, target==0)

George_notliked_training<- George_notliked_training[c(1,2,4,6,8,9,12,3,10)]
George_liked_training <- George_liked_training[c(1,2,4,6,8,9,12,3,10)]

percentage_difference<- ((colMeans(George_liked_training)-colMeans(George_notliked_training))/colMeans(George_liked_training))*100

#c.
train<- train[-c(4,10)]

valid<- valid[-c(4,10)]


#Ques 8
train.norm.df <- train
valid.norm.df <- valid
spotify5.norm.df <- spotify5


norm.values <- preProcess(train[,c(2,3,7,6,8,10)], method=c("center", "scale"))
train.norm.df[, c(2,3,7,6,8,10)] <- predict(norm.values, train[, c(2,3,7,6,8,10)])
View(train.norm.df)

valid.norm.df[, c(2,3,7,6,8,10)] <- predict(norm.values, valid[, c(2,3,7,6,8,10)])
View(valid.norm.df)
new.norm.df <- predict(norm.values, chosen_song)

# ques 9
install.packages("FNN")
library(FNN)
nn <- knn(train = train.norm.df[, c(2,3,7,6,8,10)], test = new.norm.df, 
          cl = train.norm.df[, 11], k = 7)
row.names(train.norm.df)[attr(nn, "nn.index")]
nn
class(nn)

#ques 10

accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
accuracy.df

for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, c(1,2,4,5,6,8)], valid.norm.df[, c(1,2,4,5,6,8)], 
                  cl = train.norm.df[, 11], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 11])$overall[1] 
}
accuracy.df
mean(accuracy.df$accuracy)

#ques 11
plot1 <- ggplot(accuracy.df, aes(x=k, y=accuracy)) + geom_point()

#Ques 12
knn.new_preciction <- knn(train = train.norm.df[, c(1,2,4,5,6,8)], test = new.norm.df, 
          cl = train.norm.df[, 11], k = 2)
row.names(train.norm.df)[attr(nn, "nn.index")]
knn.new_preciction


#Naive Bayes
 
#Ques 1
install.packages("fivethirtyeight")
library(fivethirtyeight)

w<-weather_check
w
#Ques 2

unique(w$weather_source)
#a.
w1<-filter(w, !is.na(weather_source))

w1$weather_source[w1$weather_source %in% c("The default weather app on your phone", 
                                           "A specific website or app (please provide the answer)", "Internet search")] <- "Internet_Based"
w1$weather_source[w1$weather_source %in% c("Local TV News")] <- "TV"
w1$weather_source[w1$weather_source %in% c("The Weather Channel")] <- "WeatherChan"
w1$weather_source[w1$weather_source %in% c("Newsletter", "Newspaper")] <- "NEWS"
w1$weather_source[w1$weather_source %in% c("Radio weather")] <- "Radio"
unique(w1$weather_source)

#b.
str(w1)
w1$weather_source<-as.factor(w1$weather_source)
w1$weather_source_site<-as.factor(w1$weather_source_site)
w1$region<-as.factor(w1$region)

#c.
n_miss(w)
miss_var_summary(w)

#-----left

table_highDegree<- table(w1$weather_source_site)
#ii.

w2 <- subset(w1, select = -weather_source_site)

w2<-subset(w1, select = -respondent_id)
colnames(w2)


w2<-na.omit(w2)
#Ques 3
set.seed(30)
train_w <- sample_frac(w2, 0.60) 
valid_w <- setdiff(w2, train_w)

#ques4
plot2<-ggplot(train_w, aes(fill=weather_source, x=hhold_income)) + geom_bar(position="fill")
plot3<-ggplot(train_w, aes(fill=weather_source, x=region)) + geom_bar(position="fill")
plot4<-ggplot(train_w, aes(fill=weather_source, x=age)) + geom_bar(position="fill")
plot5<-ggplot(train_w, aes(fill=weather_source, x=female)) + geom_bar(position="fill")
plot6<- ggplot(train_w, aes(fill=weather_source, x=ck_weather_watch)) + geom_bar(position="fill")

#Ques 5
library(e1071)
Model1_bayes<-naiveBayes(weather_source~., data=train_w)

#Ques 6
Pred_train_w <- predict(Model1_bayes, newdata = train_w)
confusionMatrix(Pred_train_w, train_w$weather_source)
  
Pred_valid_w <- predict(Model1_bayes, newdata = valid_w)
confusionMatrix(Pred_valid_w, valid_w$weather_source)

#Ques 7
prop.table(table(train_w$weather_source))

(61.45-56.72)/56.72
#Ques 8
#a. 
pred_valid_w2 <- predict(model_nb, newdata = valid_w, type='raw')
pred_valid_w2

validPred_df <- as.data.frame(pred_valid_w2)
validPred_df2 <- cbind(valid_w, validPred_df)

likely_group <- validPred_df2 %>%                                     
  arrange(desc(Radio)) %>% 
  slice(1:50)

#b.
prop.table(table(likely_group$weather_source))

#ques 9
#a.
rajat <- data.frame(ck_weather = TRUE , ck_weather_watch = 'Somewhat likely',  age = '18-29', female = FALSE, hhold_income = '$75000 to $99999', region = 'Middle Atlantic' )

predict(Model1_bayes, newdata = rajat)

#b.
predict(Model1_bayes, newdata = rajat, type='raw')

#c. 
Model2_bayes<-naiveBayes(weather_source~., data=valid_w)
predict(Model2_bayes, newdata = rajat)

predict(Model2_bayes, newdata = rajat, type='raw')

Internet_score2<-   0.54626866*0.1912568*0.32240437*0.27868852*0.4153005*0.08196721*0.10227273
news_score2<-       0.04477612*0.2000000*0.26666667*0.06666667*0.3333333*0.20000000*0.06666667
radio_score2<-      0.02985075*0.2000000*0.40000000*0.20000000*0.4000000*0.20000000*0.20000000
tv_score2<-         0.21791045*0.2191781*0.27397260*0.08219178*0.3972603*0.09589041*0.11111111
weatherchan_score2<-0.16119403*0.1481481*0.35185185*0.24528302*0.4339623*0.16981132*0.09615385

sum_score2<-Internet_score2+news_score2+radio_score2+tv_score2+weatherchan_score2
sum_score<-Internet_score+news_score+radio_score+tv_score+weatherchan_score
Internet_score2/sum_score2 


