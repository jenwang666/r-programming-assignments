# r-programming-assignments
#Jenkin Wang
#LIS6371
#Repository for R Programming Assignments

#Assignment 1
assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)
myMean <- function(assignment2) {
  return(sum(assignment) / length(someData))
}
myMean(assignment2)

###corrected version
assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)
myMean <- function(assignment2) {
  return(sum(assignment2) / length(assignment2))
}
myMean(assignment2)


#Assingment 4
freq<-c(0.6,0.3,0.4,0.4,0.2,0.6,0.3,0.4,0.9,0.2)
bloodp<-c(103,87,32,42,59,109,78,205,135,176)
first<-c("bad","bad","bad","bad","good","good","good","good","NA", "bad")
second<-c("low","low","high","high","low","low","high","high","high","high")
finaldecision<-c("low","high","low","high","low","high","low","high","high","high")


df <- data.frame(freq, bloodp)
boxplot(df, 
        main = "Side-by-Side Boxplots",
        col = c("lightblue", "lightgreen"),
        ylab = "Values",
        xlab = "Variables")
boxplot(bloodp,main = "Boxplot of Blood Pressure",ylab = "Blood Pressure (mmHg)",col = "red")
boxplot(freq,main = "Boxplot of Frequency",ylab = "Frequency",col = "blue",horizontal = FALSE)


hist(freq,main = "Histogram of Frequency",xlab = "Frequency",ylab = "Count",col = "yellow",breaks = seq(0, 1, by = 0.1))
hist(bloodp,main = "Histogram of Blood Pressure",xlab = "Blood Pressure (mmHg)",ylab = "Count",col = "lightgreen",breaks = 3)


#Assignemnt 7
head(mtcars)
str(mtcars)
summary(mtcars)
print(mtcars)
plot(mtcars)


s3_obj <- list(name = "Myself", age = 29, GPA = 3.5)
class(s3_obj) <- "student_s3"
print(s3_obj)
summary(s3_obj)


setClass("student_s4",
         slots = c(name = "character", age = "numeric", GPA = "numeric"))
s4_obj <- new("student_s4", name = "Myself", age = 29, GPA = 3.5)
print(s4_obj)
summary(s4_obj)

