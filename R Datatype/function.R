#Function 
add<-function()
{
  5+6
}
add()


#with parameter
sum_number<-function(x,y)
{
  sum<-x+y
  return(sum)
}
sum_number(5,6)

#return paxi ko print hudaina
sub_number<-function(x,y)
{
  sub<-x-y
  print("Hello")
  return(sub)
  print("Welcome")
}
sub_number(9,6)


#return is not compulsory
mul<-function(x,y)
{
  m=x*y
  print(m)
}



div<-function(x,y)
{
  mul(5,6)
  d=x/y
  
}
print(div(10,2))

#vector
vec_calc<-function(v)
{
  mean(v)
}
vec_calc(c(1,2,3,4,5))



#oddeven 
oddeven<-function(n)
{
  n<-as.numeric(readline(prompt = "Enter any number"))
  if(n%%2==0){
    print("It is even")
  }else{
    print("It is odd")
  }
  
}
oddeven()

#factorial
f=1
fact<-function(n)
{
 for (i in 1:n){
   f=f*i
 }
  return(f)
}
fact(5)


#palindrome
palindrome<-function(n)
  {n<-as.numeric(readline(prompt = "Enter any number"))  #user defined 
  temp<-n
  sum<-0
  while (n!=0) {
    r=n%%10
    sum=sum*10+r
    n=n%/%10
  }
  if (temp==sum){
    print("Palindrome Number")
  }else{
    print("not palindrome number")
  }

}
palindrome()
