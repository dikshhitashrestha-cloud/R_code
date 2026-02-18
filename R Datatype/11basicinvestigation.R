library("readr")
ls("package:readr")
df<-read_csv("E:/Dikshhita/3rd sem/PFDA/retail_data.csv")
print(df)

#basic investigation
head(df$Customer_ID)
colnames(df)
rownames(df)
dim(df)
str(df)
mean(df$Age)
a<-na.omit(df$Age)
print(a)
mean(a)

sum(is.na(df$Age))

colSums(is.na(df))
summary(df$Age) # NA hatera meaan haru aaucha
hist(df$Age) # hist numeric ma matra lina milcha
hist(df$Name)

#doubleage vanne column add garnu paryo which means 2xage column ma 31 aaunu paryo

df$doubleage<-df$Age*2
dim(df)
View(df)

#to add a new csv adding doubleage in the column
library("readr")
write_csv(df,"E:/Dikshhita/3rd sem/PFDA/demo_data.csv",col_names = TRUE)
