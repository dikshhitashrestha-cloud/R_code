#Declaring other options of vector

vec<-c(1,2,33)
print(vec)

vec1<-1:10
print(vec1)

vec2<-seq(1:20)
print(vec2)

vec3<-seq(from=2, to=20, by=2)
print(vec3)

vec_4<-rep(2:4,3)
print(vec_4)

#adding elements in vector at start, end, between
vec5<-c(2,12,14,5)
vec6<-c(vec5, 88) #element added at last
print(vec6)

vec7<-c(90,vec5)
print(vec7)

vec8<-append(vec5,87, after=12)
print(vec8)

vec9<-c(vec5,vec6)
print(vec9)

#conditional
vec12<- vec5[vec5>13]
print(vec12)

vec13<-c(1,2,4,5,NA)
print(vec13)
is.na(vec13)
a<-na.omit(vec13)
print(a)

vec14<-c(1,2,3,NA)
mean(vec14)

b<-na.omit(vec14)
print(b)
mean(b)


