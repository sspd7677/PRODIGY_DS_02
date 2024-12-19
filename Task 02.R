#TITANIC DATASET EXPLORATORY DATA ANALYSIS
data<-read.csv("D:/Prodigy Internship/titanic.csv")
str(data)
dim(data)
head(data)
tail(data)
data<-na.omit(data)
#Univariate Data Analysis
summary(data)
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Sex<-as.factor(data$Sex)

#PIECHART
par(mfrow=c(2,2))
#Survived Pie chart
pie_percent<- round(100 * table(data$Survived) / sum(table(data$Survived)), 1)
pie(table(data$Survived), labels = pie_percent,main = "Number of people survived", col = c("light pink","light blue"))
legend("topright", c("Not Survived","Survived"),cex = 0.5, fill = c("light pink","light blue"))

#Gender Pie chart
pie_percent1<- round(100 * table(data$Sex) / sum(table(data$Sex)), 1)
pie(table(data$Sex), labels = pie_percent1,main = "Gender classification", col = c("light yellow","light pink"))
legend("topright", c("Female","Male"),cex = 0.5, fill = c("light yellow","light pink"))

#Passenger Class Pie chart
pie_percent2<- round(100 * table(data$Pclass) / sum(table(data$Pclass)), 1)
pie(table(data$Pclass), labels = pie_percent2,main ="Passenger class", col = c("light yellow","light blue","light pink"))
legend("topright", c("Upper","Middle","Lower"),cex = 0.5, fill = c("light yellow","light blue","light pink"))

#BARCHART
library(ggplot2)
par(mfrow=c(2,2))
barplot(table(data$Survived), main="Number of People survived",xlab="Survival Status",ylab="number of people",col="light blue")
barplot(table(data$Sex), main="Gender bargraph",xlab="Gender",ylab="number of people",col="light blue")
barplot(table(data$Pclass), main="Passenger class bar graph",xlab="Passenger class",ylab="number of people",col="light blue")

#HISTOGRAM
hist(data$Age,main="Histogram of Age", ylab="Age", prob=TRUE)
lines(density(data$Age),col=2,lwd=3)

#Combined Histogram
ggplot(data, aes(x=Age, fill=Survived))+geom_histogram(bins=20, colour="#1380A1") +
  labs(title="Survival Rate by Gender", y="Number of passengers", subtitle = "Distribution by age, gender and ticket class", caption="Author: Hnin")+
  theme_bw() +facet_grid(Sex~Pclass, scales="free")

#SCATTER PLOT
plot(data$Age,data$Fare,main="Scatter plot of Age and Fare")



