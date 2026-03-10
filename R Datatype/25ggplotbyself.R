str(mtcars)
mtcars%>%
  select(mpg, cyl, disp, hp)|>
  filter(cyl>6)|>
  mutate(new=ifelse(disp<360,"Round", "Half"))
ggplot(mtcars,aes(x=mpg))+
  geom_histogram(binwidth = 1)+
  theme_classic()
ggplot(c,aes(x=mpg,y=hp))+
  geom_point(size=2, color='blue')

mtcar<-mtcars%>%
  select(mpg,cyl)%>%
  filter(mpg>20)%>%
  group_by(mpg)%>%
  summarise(count= mean(mpg))
print(mtcar)

mtcar%>%ggplot(aes(x=mpg, y=count)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Scatter plot diagram",
       x="miles per gallon",
       y="count")+
  theme_minimal()

ggplot(mtcars, aes(x=mpg, y=cyl))+
  geom_boxplot(color="green")

ggplot(mtcars,aes(x=cyl, y=hp))+
  geom_violin()