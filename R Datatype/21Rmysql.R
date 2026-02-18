#> install.packages("RMySQL")

library("RMySQL")
ls("package:RMySQL")

#------------------------------------------------------------------

library(RMySQL)
db_name="mydb"
db_host="localhost"
db_user="root"
db_password="" #left blank bc the password is space

con<-dbConnect(MySQL(),
               dbname=db_name,
               host=db_host,
               user=db_user,
               password=db_password) 

if (dbIsValid(con)){
  print("Connection is Successful")
} else {
  print("Connection is not Sucessful")
}

result<- dbGetQuery(con, "Select * from student")
print(result) #returns data from xampp apahche and MySQL server

str(result)
head(result)
tail(result)
print(result$id)
dim(result)

result<- dbGetQuery(con, "Select * from teacher")
print(result) #returns data from xampp apahche and MySQL server

insert_query<-dbExecute(con,"Insert into student(id, name) values(2, 'Dipson')")
print(insert_query)

update_query<-dbExecute(con,"Update student set name ='Jamuna' wherlle id=2")
print(update_query)

delete_query<-dbExecute(con, "Delete from student where id=2")
print(delete_query)