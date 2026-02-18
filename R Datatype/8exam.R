#Declare the integer vector
vec<-c(1L,3L,4L)
length(vec)
class(vec)
vec1<-c(5L,vec)
print(vec1)
vec2<-vec[2:4]
print(vec2)
vec3<-vec[vec==5]
print(vec3)
