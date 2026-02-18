data(mtcars)
str(mtcars)
summary(mtcars)
head(mtcars)
tail(mtcars)
#View(mtcars)

#descriptive statistics
mean(mtcars$mpg)
median(mtcars$hp)
sd(mtcars$cycle)
sum(mtcars$wt)
var(mtcars$hp)
min(mtcars$mpg)
max(mtcars$mpg)

#simple basic visualization 
hist(mtcars$cyl)
hist(mtcars$hp)
plot(mtcars$hp, mtcars$cyl)
colnames(mtcars)

#iris dataset
data("iris")
str(iris)
summary(iris)

#descriptive statistics
mean(iris$Sepal.Length)
median(iris$Sepal.Width)
sd(iris$Petal.Length)
sum(iris$Petal.Width)
var(iris$Sepal.Length)
min(iris$Sepal.Width)
max(iris$Sepal.Width)

#simple basic visualization 
hist(iris$Sepal.Length)
hist(iris$Petal.Length)
plot(iris$Petal.Length, iris$Sepal.Length)
colnames(iris)

