
### Data Visualization Practice Script####

##Sanjiv Jaggia's Book Business Analytics: Chapter-3 and Chapter-4 Examples ####

#install.packages("readxl")
library("readxl")
setwd("F:/UCR/Academics/Fall2022/MGT256_Business_Analytics/Homeworks/Lab/Lab1")


#### Exercise 3.1 #######
 #a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Growth_Value")

 #b)
mean(myData$Growth)
median(myData$Growth)

 #c) 
summary(myData$Growth)


#### Exercise 3.2 #######
 #a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Online")

#b)
tapply(myData$Clothing, myData$Sex, mean)

### Exercise 3.3 ####
#a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Growth_Value")

#b)
summary(myData)

quantile(myData$Growth, 0.30)


#### Excercise 3.4 ####
#a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Growth_Value")

#b)
max(myData$Growth)- min(myData$Growth)

#c)
quantile(myData$Growth, 0.75)- quantile(myData$Growth,0.25)

#d)
mean(abs(myData$Growth-mean(myData$Growth)))

#e)
var(myData$Growth)
sd(myData$Growth)


### Excercise 3.8###
#a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Growth_Value")

#b)
cor(myData)

### Excercise 3.9###
#a)
myData=read_excel("jaggia_ba_2e_ch03_data.xlsx", sheet = "Growth_Value")

#b)
boxplot(myData$Growth,myData$Value, main= "Boxplots for Growth and Value", xlab="Annual Returns, 1984-2019(in percent)", names= c("Growth", "Value"), horizontal= TRUE, col= "gold")

#c)
outliersGrowth = boxplot(myData$Growth)$out; outliersGrowth
outliersValue = boxplot(myData$Value)$out; outliersValue

#d)
myData$newValue = ifelse(myData$Value %in% outliersValue, NA, myData$Value)

#e)
summary(myData)



### Excercise 4.1###

#a)

myData=read_excel("jaggia_ba_2e_ch04_data.xlsx", sheet = "Transactions")

#b)
Repeat_Frequency= table(myData$Repeat)
Repeat_Frequency
Sex_Frequency= table(myData$Sex)
Sex_Frequency
View(Sex_Frequency)

#c)
Repeat_Proportion= prop.table(Repeat_Frequency)
Repeat_Proportion
View(Repeat_Proportion)
Sex_Proportion= prop.table(Repeat_Frequency)
Sex_Proportion
View(Sex_Proportion)

#d)
barplot(Repeat_Frequency, main="Bar chart for the Repeat variable", ylab= "Number of Customers", col= "blue")
abline(h=0)

barplot(Sex_Frequency, main="Bar chart for the Sex variable", ylab= "Number of Customers", col= "blue")
abline(h=0)



### Excercise 4.2###
#a)
myData=read_excel("jaggia_ba_2e_ch04_data.xlsx", sheet = "Transactions")

#b)
intervals = seq(0, 250, by=50)

#c)
Income.cut= cut(myData$Income, intervals, left= FALSE, right= TRUE)

#d)
Income.frequency=table(Income.cut)
Income.frequency
View(Income.frequency)

#e)
Income.prop= prop.table(Income.frequency)
Income.prop
View(Income.prop)

#f)
hist(myData$Income, breaks=intervals, right=TRUE, main= "Histogram for the Income variable", xlab="Annual Income (in $1,000s)", col= "blue")




### Excercise 4.3###
#a)
myData=read_excel("jaggia_ba_2e_ch04_data.xlsx", sheet = "Promotion")

#b)
myTable= table(myData$Location, myData$Purchase)
myTable
prop.table(myTable)

#c)
myNewTable= table(myData$Purchase, myData$Location)

#d)
barplot(myNewTable, col=c("blue","red"), legend= rownames(myTable), xlab="Location", ylab= "count", ylim=c(0,200))



### Excercise 4.4###
#a)
myData=read_excel("jaggia_ba_2e_ch04_data.xlsx", sheet = "Transactions")

#b)
plot(Purchase~Income, data=myData, main= "Scatterplot of Purchase against Income", 
     xlab= "Annual Income(in $1000s)", ylab= "Purchase(in $)", col= "chocolate", pch=16)






### Excercise 4.5###
#a)
myData=read_excel("jaggia_ba_2e_ch04_data.xlsx", sheet = "Birth_Life")

#b)
plot(myData$Birth_Rate~myData$Life_Exp, main="Scatterplot of Birth Rate against Life Expectancy", 
     xlab= "Life Expectancy (in years)", ylab= 'Birth Rate (in %)', pch=16, 
     col=ifelse(myData$Development== "Developing", 20, 26))











