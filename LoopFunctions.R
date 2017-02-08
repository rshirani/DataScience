by(iris$Sepal.Length, iris$Species, mean)
tapply(iris$Sepal.Length, iris$Species, mean)

tapply(mtcars$cyl, mtcars$mpg, mean)

with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)


lapply(mtcars, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$cyl, mtcars$mpg, mean)
split(mtcars, mtcars$cyl)
sapply(mtcars, cyl, mean)
apply(mtcars, 2, mean)
tapply(mtcars$mpg, mtcars$cyl, mean)
mean(mtcars$mpg, mtcars$cyl)



apply(iris, 1, mean)
rowMeans(iris[, 1:4])
apply(iris, 2, mean)
colMeans(iris)
apply(iris[, 1:4], 1, mean)
apply(iris[, 1:4], 2, mean)


