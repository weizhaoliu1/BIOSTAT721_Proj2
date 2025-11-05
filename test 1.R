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
  ggtitle("daily temp and ozone")+
  scale_color_brewer(palette = "Dark2")

install.packages("viridis")
library(viridis)
airquality2$Month=as_factor(airquality$Month)
airquality2$Day=as_factor(airquality$Day)
ggplot(airquality2, aes(x=Temp, y=Ozone))+
  geom_point(aes(color=Month),shape=16, size=2)+
  xlab("Temp")+
  ylab("Ozone")+
  ggtitle("daily temp and ozone")+
  scale_color_viridis(discrete= TRUE, direction=1)



head(diamonds)

ggplot(diamonds, aes(x=carat, y=price))+
  geom_point(aes(color=cut), alpha = 0.25, size=2)+
  scale_colour_viridis_d(direction=-1, name="Cut")+
  ylab("Price$")+
  xlab("Carat")+
  ggtitle("Price and Carat of Diamonds, by Cut")+
  theme_minimal()+
  ggsave("randomplot.jpg")
               


install.packages("wesanderson")
library(wesanderson)
col<-wes_palette("Darjeeling1", 5, type=c("discrete"))

install.packages("devtools")
devtools::install_github("katie")

ggplot(diamonds, aes(x=carat, y=price))+
  geom_point(aes(color=cut), alpha = 0.25, size=2)+
  scale_colour_viridis_d(direction=-1, name="Cut")+
  ylab("Price$")+
  xlab("Carat")+
  ggtitle("Price and Carat of Diamonds, by Cut")+
  theme_minimal()+
  ggsave("randomplot.jpg", width = 5, height = 5)

install.packages("arsenal")
library(arsenal)
library(tidyverse)
aq2<-na.omit(airquality)
aq3<- aq2%>%
  group_by(Month)%>%
  summarize(temp=mean(Temp), sd=sd(Temp), count=n())%>%
  mutate(LB=temp-1.96*sd/sqrt(count), UB=temp+1.96*sd/sqrt(count))

aq3_alt<-aq2|>
  group_by(Month)|>
summarize(mean_temp=mean(Temp, na.rm=TRUE),
          LB= mean_cl_normal(Temp)$ymin,
          UB=mean_cl_normal(Temp)$ymax)

aq3
aq3_alt

aq3$Month <- ifelse(aq3$Month == 5, "May",
                    ifelse(aq3$Month == 6, "June",
                           ifelse(aq3$Month == 7, "July",
                                  ifelse(aq3$Month == 8, "August",
                                         ifelse(aq3$Month == 9, "September", NA)))))

                    
                    
                    
p1<- ggplot(aq3, aes(x=Month, y=temp, color=month ))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LB, ymax=UB))+
  ylab("Temp")+
  xlab("Month")+
  theme_classic()
p1


p2<- ggplot(aq3, aes(x=factor(Month), y=temp, color=month ))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LB, ymax=UB))+
  ylab("Temp")+
  xlab("Month")+
  theme_classic()



P3+scale_color_discrete(name="Month")+theme(
  legend.title = element_text(colour = "blue", size = 16, face="bold"),
  legend.text = element_text(size=14, face="italic"))



head(diamonds)
dia1<-diamonds|>
  group_by(color)|>
  summarize(diamond_price=mean(price), sd1=sd(price, na.rm = T), count=n())|>
  mutate(LB=diamond_price-1.96*sd1/sqrt(count), UB=diamond_price+1.96*sd1/sqrt(count))

dia_alt<-dia1


p <- ggplot(dia_alt, aes(x = color, y = diamond_price, color = color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.1) +
  scale_colour_viridis_d(direction = -1, name = "Color") +
  ylab("Price ($)") +
  xlab("Diamond Color Level") +
  theme_minimal(base_size = 14)+
  theme(legend.position = "none") 

p


dia_alt <- diamonds |>
  group_by(clarity, color) |>
  summarize(
    diamond_price = mean(price, na.rm = TRUE),
    sd1 = sd(price, na.rm = TRUE),
    count = n()
  ) |>
  mutate(
    LB = diamond_price - 1.96 * sd1 / sqrt(count),
    UB = diamond_price + 1.96 * sd1 / sqrt(count)
  )

ggplot(dia_alt, aes(x = color, y = diamond_price, color = color)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.1) +
  scale_colour_viridis_d(direction = -1, guide = "none") +
  facet_wrap(~ clarity) +   # 👈 每种 cut 一个小图
  ylab("Price ($)") +
  xlab("Diamond Color Level") +
  theme_minimal(base_size = 14)