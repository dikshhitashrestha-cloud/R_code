library("readr")
ls("package:readr")
data<-read_csv("E:/Dikshhita/3rd sem/PFDA/6. retail_data.csv")
head(data)
select(data, Email, Phone)
select(data, -City)
filter(data, Country=='Canada')
filter(data, Age<20)
filter(data, Age>25 & Age<50)
filter(data, Country=='Australia'| Gender=='Female')

select(data, Name, Email, Age,Phone, starts_with("Ph"))

c<-data%>%
  select(Name, Age, Email)|>
  filter(Age>15 & Age<30)|>
  group_by(Name)%>%
  mutate(result=ifelse(Age>20,"Adult", "Teenage"))
ggplot(c,aes(x=Age))+
  geom_histogram(binwidth = 1)+
  theme_classic()



  
  
