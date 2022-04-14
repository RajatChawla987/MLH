library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(tidyr)
#Ques2
austin<- read.csv("/Users/swati/Desktop/AustinDataset.csv")
#a.
str(austin)

#ques 3

austin1<- filter(austin, Zip.Code==78702)
austin1
str(austin1)
ncol(austin)
austin2<-austin1
#ques 4
#a. 

anyNA(austin1)
summary(austin1)
sum(is.na(austin1))
#b.

sum(complete.cases(austin1))
nrow(austin1)
Complete_cases <-(sum(complete.cases(austin1))/nrow(austin1))*100
Complete_cases
#c.

austin2[austin2==""] <-NA 

#d. 

sum(is.na(austin2))

#e. 

sum(complete.cases(austin2))
summary(austin2)
Complete_cases2 <-(sum(complete.cases(austin2))/nrow(austin2))*100

#f.

install.packages("naniar")
library(naniar)
?naniar
install.packages("anytime")
library(anytime)

austin3 <- data.frame(miss_var_summary(austin7))

#g. removing records where city is NA
austin6 <- filter(austin2, City!="NA")
nrow(austin6)
 
#Ques 5
#a. 

str(austin6)
#b.

austin6$Created.Date<- as.Date(austin6$Created.Date, format= "%m/%d/%y")
str(austin6)
austin6$Close.Date<- as.Date(austin6$Close.Date, format= "%m/%d/%y")

#c. Adding Duration Column 

austin7<- mutate(austin6, Duration= Close.Date- Created.Date)

#d. 
install.packages("lubridate")
library(lubridate)

#i.

bday.Date<-06
bday.Month<-09
requests_on_bday<-filter(austin7, month(Created.Date)==bday.Month, day(Created.Date)==bday.Date)
str(requests_on_bday)
#find the most common (most repeated) SR Description-
austin8<- sort(requests_on_bday$SR.Description, decreasing = TRUE)[0:1]

# ques 6
str(austin7)
#b.
austin9<- filter(austin7, Method.Received=="Spot311 Interface")
str(austin9)
percentage_spot<- (nrow(austin9)/nrow(austin7))*100
nrow(austin9)
#c. 

austin10<-filter(austin7, SR.Description =="Loose Dog")
str(austin10)
percentage_spot2<-(nrow(austin10)/nrow(austin7))*100
nrow(austin10)

#d. 

Unique_method<- n_distinct(austin7$Method.Received)

#ques 7

install.packages("carat")
library(carat)
austin11<- austin7
austin11 <- subset(austin11, select = -Map.Page)
str(austin11)
colnames(austin11)

#ques 8

austin12<- austin7
?quarter()
austin12$Season<- quarter(austin12$Created.Date) 

austin12$Season[austin12$Season==1]<-'Winter'
austin12$Season[austin12$Season==2]<-'Spring'
austin12$Season[austin12$Season==3]<-'Summer'
austin12$Season[austin12$Season==4]<-'Fall'

#ques9

install.packages("ggplot2")
library(ggplot2)
plot1<- ggplot(austin12, aes(x= Season, fill= Season)) + geom_bar()
str(austin12)

#ques 10

austin13<-names(sort(table(austin12$SR.Description), decreasing = TRUE)[1:6])
austin13<-filter(austin12, SR.Description %in% austin13)

nrow(austin13)

plot2<- ggplot(austin13, aes(x= SR.Description)) + geom_bar(fill=rainbow(n=6))
ggplot(data = austin13, aes(x=reorder(SR.Description, SR.Description, function(x)-length(x))))+ geom_bar(fill=rainbow(n=6))
ggplot(data = austin13, aes(x=reorder(SR.Description, SR.Description, function(x)-length(x))))+ geom_bar(fill=rainbow(n=6))

#ques 11
#a.
austin15<-names(sort(table(austin13$Method.Received), decreasing = TRUE)[1:6])
austin15<- filter(austin13, Method.Received %in% austin15)
nrow((austin15))

#b. 
plot3<- ggplot(austin15, aes(x= Method.Received, fill=Method.Received)) + geom_bar() + ylim(-15,18000)

plot3 + facet_wrap(SR.Description~.)
?facet_wrap
#ques 12

plot5<- ggplot(austin12, aes(x=Duration)) +geom_histogram(binwidth = 1.0, bins = 200, fill= 'tomato', color='peachpuff') + xlim(-0.1,20) + ylim(0,15000)

#ques 14
install.packages("leaflet")
library(leaflet)

plot4 <- leaflet(austin15) %>% addTiles() %>% addCircles(lng= ~Longitude.Coordinate , lat=~Latitude.Coordinate)
plot4


#------------------------------------------------
#ques 15

austin17<- leaflet(austin15) %>% addTiles() %>% addCircles(lng= ~Longitude.Coordinate , lat=~Latitude.Coordinate) %>%
  addProviderTiles(providers$Esri.WorldImagery)
