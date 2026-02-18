5%%2
132%%10
5%/%2
5/2

#reverse
n<-123
sum<-0
while (n!=0) {
  r=n%%10
  sum=sum*10+r
  n=n%/%10
}
print(sum)
print(n)

#Palindrome
n<-123
temp<-n
sum<-0
while (n!=0) {
  r=n%%10
  sum=sum*10+r
  n=n%/%10
}
print(sum)
print(n)
if (temp==sum){
  print("Palindrome Number")
}else{
  print("not palindrome number")
}

#Palindrome
n<-as.numeric(readline(prompt = "Enter any number"))  #user defined 
temp<-n
sum<-0
while (n!=0) {
  r=n%%10
  sum=sum*10+r
  n=n%/%10
}
print(sum)
print(n)
if (temp==sum){
  print("Palindrome Number")
}else{
  print("not palindrome number")
}
