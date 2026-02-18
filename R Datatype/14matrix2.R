#MATRIX

ls("package:base") # to find base functions defined in package called base
ls("package:stats")

m<-matrix(1:6, nrow = 3) #priorities column first
print(m)

m1<-matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE)
print(m1)

#*last class:*
#----------------------------------------------------------------------------
#*new class:*

#matrix is a collection of vectors

vec<-1:9
print(vec)
mat<-matrix(1:9, nrow = 3, ncol = 3) #by default fills table by column
print(mat)

mat1<-matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE) #filling element by row
print(mat1) #1:9 sequential data

mat2<-matrix(c(1,2,3,4), nrow = 2) #user defined data
print(mat2)

vec1<-c(12,10,3,4,7,9)
mat3<-matrix(vec1, nrow = 3, ncol = 2)
print(mat3)

print(mat)

#using inbuilt functions to determine the matrix

dim(mat) # gives row and column
sum(mat)
colSums(mat)
rowSums(mat)
rowMeans(mat)
colMeans(mat)
t(mat) #transpose makes it utlta row to col, col to row

#accessing matrix elments
print(mat)
print(mat[2,3])
print(mat[3,1]) # frist one rep row and 2nd one is col
print(mat[2,])
print(mat[,2])
print(mat[-2,2]) #takes the whole row but exculdes 2 row ko value

#arithmetic ops
print(mat)
print(mat1)
add<-mat+mat1 #does addition correspondingly element wise
print(add)
print(mat-mat1)

# cannot use mul if the data is not in the same ratio such as 2x2 and 3x3 cannot mul
# but 2x2 and 2x2 can be mul or 3x3 or 4x4 needs to be same

det(mat)
det(mat2)

mat4<-matrix(1:9, nrow = 3, ncol = 3)
print(mat4)
mat5<-matrix(1:9, nrow = 3, ncol = 3, byrow = )
print(mat5)

#arithmetic operations
print(mat+mat1)
print(mat-mat1)
print(mat*mat1)
print(mat%*%mat1)

#accessing elements
mat4[3,1]

#update particular element
mat4[1,2]<-44
mat4

vec<-c(1,2,3,4,5,6)
mat6<-matrix(vec,nrow=2, ncol = 3)
print(mat6)

#row bind and column bind
vec1<-c(10,12)
column_bind<-cbind(mat6, vec1)
print(column_bind)
dim(column_bind)
row_bind<-rbind(mat6, c(22,14,13))
print(row_bind)