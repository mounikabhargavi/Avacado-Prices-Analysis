#Module 3 R Practice
print("Module 3 R Practice")

#Installing packages
install.packages("ggpubr")
install.packages("zip")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

#Setting working directory & Importing csv file into R script
setwd("C:/Users/admin/Downloads")
Avacado<- read.csv("avocado.csv",header=TRUE, sep=",")
options(max.print=999999)

#To understand the various attributes of the data and to view the data set
names(Avacado)
View(Avacado)


#removing columns X
Avacado = subset(Avacado, select=c(-X))


#changing column names for better understanding
colnames(Avacado) <- c("Date","AveragePrice","TotalVolume","PLU_4046","PLU_4225","PLU_4770",
                       "Total_bags","Small_bags","Large_bags","XLarge_bags","type","year","region")

# Statistical summary of Average price of avocado
summary(Avacado$AveragePrice)


#creating new table with avocado average price vs type
Avacado1 <- data.frame("AvacadoPrice" = Avacado$AveragePrice, "Type" = Avacado$type)

#plotting a box plot for avocado average price vs type
p <- ggplot(Avacado1, aes(x = Type, y = AvacadoPrice, fill=Type))+ 
  geom_boxplot()+scale_fill_brewer(palette="Dark2")
p

#creating a subset for conventional type avocado
Avacado_conventional <- subset(Avacado1[c("AvacadoPrice", "Type")], Avacado1$Type == 'conventional')
Avacado_conventional
options(max.print=999999)
Avacado_conventional

#one sample t-test for conventional type avocado
T_mean_conventional <- mean(Avacado_conventional$AvacadoPrice)
T_mean_conventional
t_test1 <- t.test(Avacado_conventional$AvacadoPrice, mu = 1.15)
t_test1

#Hypothesis testing for P Value for conventional type avocado

sdf <- sd(Avacado_conventional$AvacadoPrice)
sdf

#n=9126
deg_freedom = n - 1
deg_freedom

str(deg_freedom)

t <- (mean(Avacado_conventional$AvacadoPrice)-1.158)/(sd(Avacado_conventional$AvacadoPrice)
                                                      /sqrt(length(Avacado_conventional$AvacadoPrice)))
t

p_value<-2*pt(-abs(t),deg_freedom)
p_value


#creating a subset for organic type avocado
Avacado_organic <- subset(Avacado1[c("AvacadoPrice", "Type")], Avacado1$Type == 'organic')
Avacado_organic

#one sample t-test for organic type avocado
T_mean_organic <- mean(Avacado_organic$AvacadoPrice)
T_mean_organic
t_test2 <- t.test(Avacado_organic$AvacadoPrice, mu = 1.65)
t_test2

#Hypothesis testing for P Value for organic type avocado

sdf <- sd(Avacado_organic$AvacadoPrice)
sdf

#n=9123
deg_freedom = n - 1
deg_freedom

str(deg_freedom)

t <- (mean(Avacado_organic$AvacadoPrice)-1.653)/(sd(Avacado_organic$AvacadoPrice)
                                                      /sqrt(length(Avacado_organic$AvacadoPrice)))
t

p_value<-2*pt(-abs(t),deg_freedom)
p_value


##Two sample t test 
t.test(Avacado_conventional$AvacadoPrice,Avacado_organic$AvacadoPrice,var.equal = TRUE)


#main table t test
mean <- mean(Avacado$AveragePrice)
mean
t_test3 <- t.test(Avacado$AveragePrice, mu = 1.405978)
t_test3

#Hypothesis testing for P Value 

sdf <- sd(Avacado$AveragePrice)
sdf


#n=18249
deg_freedom = n - 1
deg_freedom

str(deg_freedom)

t <- (mean(Avacado$AveragePrice)-1.406)/(sd(Avacado$AveragePrice)/sqrt(length(Avacado$AveragePrice)))
t

p_value<-2*pt(-abs(t),deg_freedom)
p_value


