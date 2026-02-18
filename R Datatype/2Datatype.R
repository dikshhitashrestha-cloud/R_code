num_one<-4
typeof(num_one)
class(num_one)
num_two<-5L
class(num_two)
num_three<-4.5
class(num_three)
num_four<-"dixita"
class(num_four)
num_five<-'shrestha'
class(num_five)
var_one<-TRUE
class(var_one)
var_two<-2+3i
class(var_two)

#size or memory
object.size(num_one)
object.size(num_two)
object.size(num_three)
object.size(num_four)

#arithmetic operations
x<-5
y<-7
sum<-x+y
print("the sum of two num is",sum)
print(sum)
cat("the sum of two num is", sum)
print(paste("the sum of two num is",sum))
print(paste0("the sum is",sum))

