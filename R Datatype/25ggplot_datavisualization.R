str(iris)
ggplot(iris, aes(x=Sepal.Length))+
  geom_density()
ggplot(iris,aes(x=Petal.Length))+
  geom_histogram(binwidth = 1)+
  theme_classic()
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length))+
  geom_point(size=2, color='red')
