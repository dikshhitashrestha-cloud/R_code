#this is for how to load xl file into the system

library("readxl")
ls("package:readxl")
df<-read_excel("DatasetExcel.xlsx")
print(df)

library("moments")
skewness(df$X1)
