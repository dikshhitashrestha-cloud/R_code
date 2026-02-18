
df<-data.frame( name=c("Dixita","Dipsha","Dipson","Jamuna"),
               age=c(22,22,18,26),
               gender=c("Female","Female","Male","Female"),
               education=c("Bachelors","BIT","+2","SEE"))
print(df)
select(df, name, gender)
