

mydata<-data.frame(x1 = c(2, 2, 6, 4),
                   x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)
mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)


manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)


leadership

leadership[leadership$age == 99,"age"] <- NA

leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })

str(leadership$agecat)

leadership$agecat <- factor(leadership$agecat)

fix(leadership)

names(leadership)

names(leadership)[2] <- "date"

newdata <- na.omit(leadership)

newdata


myformat <- "%m/%d/%y"
leadership$date <- as.Date(leadership$date, myformat)

startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
leadership[which(leadership$date >= startdate &
                              leadership$date <= enddate),]

leadership

leadership$age
order(leadership$age)
leadership[order(leadership$age),]

t <- c(3,1,4,2,5)
t
leadership[t,]

leadership[order(gender, -age),]


t <- paste("q", 1:5, sep="")
t

leadership[c("q3", "q4")]

t <- names(leadership) %in% c("q3", "q4")
leadership[!t]

leadership[leadership$gender=="M" &
             leadership$age > 35,]

subset(leadership, gender=="M" & age > 25,
       select=gender:q4)


leadership[sample(1:nrow(leadership), 3, replace=FALSE),]

sample(1:10, 3)

install.packages("sqldf")
library(sqldf)
sqldf("select Cyl, Count(1) from mtcars group by 1",
      row.names=TRUE)



















