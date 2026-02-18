#numeric vector
vec<-c(2,3,4,5)
class(vec)

#integer vector
vec1<-c(2L, 3L,4L)
class(vec1)

#character vector
vec2<- c("dixita","shrestha","dipson")
class(vec2)

#vector operation function 
vec<-c(2,3,4,5)
sum(vec)
mean(vec)
median(vec)
summary(vec)
min(vec)
max(vec)
str(vec)
head(vec) #top 6 value from top
tail(vec) #top 6 value from bottom
head(vec, 2) #top 2 value from top restrict other
tail(vec, 2) #top 6 value from bottom restrict other
length(vec)

#mixed datatype 
vec_x<-c(2,3,4,5)
print(vec_x)
vec_y<-c(2,3L,4)
class(vec_y)


#vector operations with different length
num_a<-c(2,3,4,5)
num_b<-c(1,2,3,4)
add<-num_a+num_b
print(add)
sub<-num_a-num_b
print(sub)
mul<-num_a*num_b
print(mul)
div<-num_a/num_b
print(div)

num_c<-c(2,4,3)
num_d<-c(4,5,6,7,5)
add_a<-num_c+num_d

num_c+5

#membership operator
6%in%num_d  #is 6 is in numd
15%in%num_d

#vector accessing using index
print(num_d[4])

#removing certain index
num_d[-4]

add_ab<-num_d[3]+num_d[4]
print(add_ab)