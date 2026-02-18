library('ggplot2')
ls("package:ggplot2")
str(mtcars)
ggplot(mtcars,aes(x=mpg,y=hp, fill=factor(cyl)))+
  geom_point(size=2, color='red')+
  geom_smooth(method="lm", se=TRUE)
  labs(title = "Scatter plot diagram",
       x="miles per gallon",
       y="Horse power")+
  theme_minimal()+
  theme_classic()+
  theme_gray()+
  theme_bw()

ggplot(mtcars,aes(x=mpg))+
  geom_histogram(binwidth = 1)+
  theme_classic()

ggplot(mtcars,aes(x=hp))+
  geom_histogram(binwidth = 3)+
  theme_classic()

ggplot(mtcars, aes(x=mpg, y=disp, fill=factor(vs)))+
  geom_boxplot(color="green")

ggplot(mtcars, aes(x=mpg, fill=factor(vs)))+
  geom_density()

ggplot(mtcars, aes(x=mpg, fill=factor(cyl)))+
  geom_density()

ggplot(mtcars, aes(x=mpg))+
  geom_bar()


