square.em<-function(x) {
  ans<-x*x
  return(ans)
}

square.em(4)

v<-1:5
square.em(v)

m<-matrix(seq(2,20,2),10,2)
square.em(m)

mat<-matrix(1:4,4,3)
chr<-c("m","c","ds")
L<-list(mat,chr)


square.em2<-function(x) {
  ans<-x*x
  return(list(x_name=x, answer=ans))
}
square.em2(5)
out<-square.em2(5)
out
out$x_name


power<-function(x, power) {
  ans<-x^power
  return(list(x=x, answer=ans))
}

power(3,4)
power(1:5,2)
power(x=2, power=1:5)
power(power=3, x=4)



CtoF <- function(x) {
  ans<-round(((x*9/5)+32),2)
  return(temperature_F=ans)
}


library(readr)
library(lubridate)
data<-read.csv("data_temp.csv")
head(data)
updated_temp=CtoF(data$maxTemp)
updated_data=data.frame(
  Date=mdy(data$date),
  temperature_F=updated_temp,
  temperature_C=data$maxTemp)
colnames(updated_data)<-c("Date","Temperature(F)", "Temperature(C)")
head(updated_data, 10)

student_labs_assignment<-function(n) {
ids<-1:n
ids

labs<-c(rep("A",n/2),rep("B",n/2))
labs

assignment<-sample(labs, replace = FALSE)
student_labs<- data.frame(
  Stu_ID=ids, 
  Lab=assignment)
return(student_labs)
}

student_labs_assignment(80)
