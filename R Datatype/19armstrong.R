
#Armstrong
n<-as.numeric(readline(prompt = "Enter any number"))
digit<-as.numeric(readline(prompt = "Enter the length of digit"))
temp<-n
sum<-0
while (n!=0) {
  r=n%%10
  sum= sum+r^digit
  n=n%/%10
}
print(sum)
print(n)
if (temp==sum){
  print("Armstrong Number")
}else{
  print("not Armstrong number")
}


