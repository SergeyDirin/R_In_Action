x <- c(1,2,3,4,5,6,7,8)


mean(x)
sd(x)

n <- length(x)
meanx <- sum(x)/n

css <- sum((x - meanx)^2)
sdx <- sqrt(css / (n-1))

scale(x)

library(MASS)
options(digits=3)
set.seed(1234)
mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.2, -47.1,
                    6721.2, 4700.9, -16.5,
                    -47.1, -16.5, 0.3), nrow=3, ncol=3)


mydata <- mvrnorm(500, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c("y","x1","x2")
dim(mydata)
head(mydata, n=10)


paste("x",1:3,sep="M")
paste("Today is", date())

cut(x, 3)
x

0:10
pretty(0:10, 5)
1:10
pretty(1:10, 5)


#resetting the options to default
default_opts <- callr::r(function(){options()}); options(default_opts)

options("digits")

cat("Hello" , "name")


options(digits=2)



Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
               "David Jones", "Janice Markhammer", "Cheryl Cushing",
               "Reuven Ytzrhak", "Greg Knox", "Joel England",
               "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                       stringsAsFactors=FALSE)

#Obtains the performance scores
z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

#Grades the students
y <- quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

#Extracts the last and first names
name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,Lastname, roster[,-1])

roster <- roster[order(Lastname,Firstname),]

roster

roster[order(roster$grade),]

str(roster)

aggregate(mtcars, by=list(Group.cyl=mtcars$cyl, Group.gears=mtcars$gear), FUN=mean, na.rm=TRUE)


install.packages("reshape2")
library(reshape2)

setwd("C:/Study/DS/R in Action Book Study/2020-11-27 Capter 5")
mydata <- read.csv("reshape data.csv")
mydata
md <- melt(mydata, id=c("ID", "Time"))
md
dcast(md, ID+Time~variable)
dcast(md, ID+variable~Time)
dcast(md, ID~variable+Time)
dcast(md, ID~variable, sum)
dcast(md, ID~variable, length)
dcast(md, Time~variable, sum)
dcast(md, ID~Time, sum)


