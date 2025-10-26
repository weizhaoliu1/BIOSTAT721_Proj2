print("hi")
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
head(airquality)
hist(airquality$Temp)

hist(airquality$Temp, breaks = 14)
boxplot(airquality$Ozone)

#scatter plot
plot(x=airquality$Temp, y=airquality$Ozone)
abline(fit,2)

ggplot(airquality, aes(x=Ozone))+geom_histogram()
install.packages('tictoc')
library(tictoc)
tic("code in intial statement")
ggplot(airquality, aes(x=Temp, y=Ozone))+geom_point(shape=20, size=2)+
  xlab("Temp")+
  ylab("Ozone")+
  ggtitle("daily temp and ozone")
toc()
class(airquality$Month)
airquality2= airquality

airquality2$Month=as_factor(airquality$Month)
airquality2$Day=as_factor(airquality$Day)
ggplot(airquality2, aes(x=Temp, y=Ozone))+
  geom_point(aes(color=Month),shape=16, size=2)+
  xlab("Temp")+
  ylab("Ozone")+
  ggtitle("daily temp and ozone")


head(diamonds)

ggplot(diamonds, aes(x=carat, y=price))+
  geom_point(aes(color=cut), alpha=0.3, size=2)+
  ylab("Price$")+
  xlab("Carat")+
  ggtitle("Price and Carat of Diamonds, by Cut")+
  theme_minimal()
               