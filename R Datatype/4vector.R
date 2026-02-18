#why use vector bc it saves storage and single variable
a<-5
b<-6
d<-7
e<-8
sum<-a+b+d+e
print(sum)
vec<-c(5,6,7,8)
print(vec) #small bracket is function
add1<-vec[1] #indexing for this use big bracket
add2<-vec[4]
sum<-add1+add2
print(paste("the sum of two number is ", sum))
print(add)
object.size(vec)
vec[1] 

#vector operations
vec1<-c(1,2,3,4,5)
class(vec1)
vec2<-c(2L,3L,4L,5L) 
class(vec2) 
vec3<-c("ritik","bishal","roji")
class(vec3)
vec4<-c(TRUE, FALSE, TRUE)
class(vec4)
vec5<-c(2L,3,4,5.5,)
class(vec5) #if data is mix still shows numeric but if you add chara it will give chara

#vector indexing
vec1[2] #no need to use print to display but to display sentence you need print

#arith ops using vector
vec_a<-c(1,2,3,4)
add<-vec_a+1
print(add)

vec_a<-c(1,2,3,4)
mul<-vec_a*2
print(mul)

vec_a<-c(1,2,3,4)
div<-vec_a/2
print(div)




