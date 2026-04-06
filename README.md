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

#Assignment 8
student6 <- read.table(file.choose(), header = TRUE, sep=",")
student6

library(plyr)
gender_mean <- ddply(
  student6,
  "Sex",
  summarise,
  GradeAverage = mean(Grade, na.rm = TRUE)
)
gender_mean

write.table(
  gender_mean,
  file = "gender_mean.txt",
  sep  = "\t",
  row.names = FALSE
)

i_students <- subset(
  student6,
  grepl("i", Name, ignore.case = TRUE)
)

write.csv(
  i_students$Name,
  file      = "i_students.csv",
  row.names = FALSE,
  quote     = FALSE
)
write.csv(
  i_students,
  file      = "i_students_full.csv",
  row.names = FALSE
)


#Assignment 9
cps1988<-read.csv(file.choose())
cps1988
head(1988)

plot(cps1988$education, cps1988$wage,
     main   = "Years in Education vs Wage",
     xlab   = "Years in education",
     ylab   = "Wage per Week")

plot(cps1988$experience, cps1988$wage,
     main   = "Years in Experience vs Wage",
     xlab   = "Years in Experience",
     ylab   = "Wage per Week")

# Histogram
hist(cps1988$wage,
     main   = "Distribution of Wage",
     xlab   = "Wage per Week")

hist(cps1988$education,
     main   = "Distribution of Education",
     xlab   = "Years of Education")

hist(cps1988$experience,
     main   = "Distribution of Experience",
     xlab   = "Year of Experience")

library(lattice)
library(latticeExtra)

plot1 <- bwplot(wage ~ ethnicity | region + smsa,
                data = cps1988,
                main = "Wage by Ethnicity, Region, and SMSA Status",
                xlab = "Ethnicity",
                ylab = "Weekly Wage",
                scales = list(x = list(rot = 45)),
                par.settings = list(box.rectangle = list(col = "darkblue"),
                                    box.dot = list(col = "darkblue"),
                                    plot.symbol = list(col = "darkblue", cex = 0.5)))
print(plot1)


plot2 <- histogram(~ wage | region,
                   data = cps1988,
                   main = "Wage Distribution by Region",
                   xlab = "Weekly Wage",
                   breaks = 50,
                   col = "lightblue",
                   panel = function(x, ...) {
                     panel.histogram(x, ...)
                     panel.densityplot(x, plot.points = FALSE, col = "darkred", lwd = 2)
                   })
print(plot2)


plot3 <- xyplot(wage ~ education | region,
                data = cps1988,
                groups = parttime,
                main = "Wage vs Education by Region group by part-time",
                xlab = "Years of Education",
                ylab = "Weekly Wage",
                pch = 16,
                cex = 0.5,
                auto.key = list(space = "top", columns = 2, 
                                text = c("Full-time", "Part-time")),
                panel = function(x, y, groups, ...) {
                  panel.xyplot(x, y, groups = groups, ...)
                  panel.loess(x, y, col = "black", lwd = 1.5)
                })
print(plot3)


library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(cps1988, aes(x = factor(education), y = wage)) +
  geom_boxplot(fill = "lightblue", 
               alpha = 0.7, 
               outlier.color = "red", 
               outlier.size = 0.5) +
  geom_jitter(alpha = 0.1, 
              size = 0.5, 
              width = 0.2, 
              color = "gray30") +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "darkred") +
  labs(
    title = "Wage Distribution by Education Level",
    subtitle = "Higher education generally associated with higher wages",
    x = "Years of Education",
    y = "Weekly Wage (USD)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(cps1988, aes(x = education, y = wage, color = experience)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ethnicity, ncol = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "Experience") +
  labs(
    title = "Wage vs. Education by Ethnicity",
    subtitle = "Color represents years of experience",
    x = "Years of Education",
    y = "Weekly Wage (USD)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


#Assignment 11
tukey.outlier <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)
tukey_multiple(test_mat)

corrected_tukey <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  outlier.vec
}

corrected_tukey(test_mat)
corrected_tukey <- function(x) {
  if (!is.matrix(x)) {
    stop("x must be a matrix.")
  }
  if (!is.numeric(x)) {
    stop("x must be a numeric matrix.")
  }
  
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  
  outlier.vec
}
