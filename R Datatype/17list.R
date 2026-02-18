my_list<- list(name= 'bishal',
               age=c(23,24,25),
               gender=c('m','f','m','f'),
               mat=matrix(1:9, nrow=3, ncol=3),
               df1=data.frame(education=c('+2','bachelor','Master'),
                               rollnum=c(1,2,3)),
               newlist<-list(occupation=c('Doctor','Scientist'),
                             salary= c(12000,13000,1500),
               globalVariables(name, age, gender=TRUE)
name
print(my_list)
str(my_list)
my_list$age
my_list[3]  #indexing of list
my_list[[2]][3] #index vitra ko indexing
my_list$df1
my_list$df1$education[2]
my_list[[4]][2,3]
mean(my_list$age)

