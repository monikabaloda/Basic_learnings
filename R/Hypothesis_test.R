

##Hypothesis Testing ##

library("readxl")
myData= read_excel("prime.xlsx", sheet="Prime")   ####Reading the file#######

#confidence intervals 
lower= mean(myData$Expenditures) -qt(0.975, 99, lower.tail= TRUE)*sd(myData$Expenditures)/sqrt(100);lower  #lower confidence interval
upper= mean(myData$Expenditures)+qt(0.975, 99, lower.tail= TRUE)*sd(myData$Expenditures)/sqrt(100);upper   #upper confidence interval
       #With 95% confidence, We can say that the average annual expenditure of all Prime customers fall between $1240.24 and 1373.64





##Hypothesis Testings ##

myData= read_excel("prime.xlsx", sheet="Study_Hours")
t.test(myData$Hours, alternative= "two.sided", mu=14) #We do two sided t-test. 
                               #Conclusion: do not reject the null hypothesis because p-value is greater then 0.05.


#one-side t-tests

#Question-1
df1=read_excel("earnings.xlsx")
pvalue=t.test(df1$earnings, alternative="greater", mu=50000) 
pvalue

##Conclusion: We cannot reject the null hypothesis because p-value is greater than 0.1
#    We observe that p-value is greater than alpha (=0.1), therefore we cannot reject the null hypothesis that sample mean is equal to $50000.			
#   Therefore we conclude that the observed sample mean is not significantly different from 50000 at 95% confidence level			



#Question2 
df2= data.frame(University=c("Arizona", "Auburn", "Baylor", "Boston", "Bowling"),
                Rsearch=c(237.52, 146.98, 304.22, 343.22,10.71), 
                Duration=c(23,20,25,32,7) )
n=nrow(df2)
lower=mean(df2$Rsearch) -  qt(0.90, (n-1), lower.tail=TRUE)*sd(df2$Rsearch)/sqrt(n)   #(n-1) is degree of freedom
upper=mean(df2$Rsearch) +  qt(0.90, (n-1), lower.tail=TRUE)*sd(df2$Rsearch)/sqrt(n)   #

##Conclusion:  We observe that the upper and lower limit of the confidence interval is 299.92 and 117.14 respectively. 	
##Interpretation: With more than 90% confidence, we can say that the mean of the population will lie between 117.14 and 299.92	


##Question-3
df3=read_excel("Lottery.xlsx")
pvalue=t.test(df3$Lottery, alternative="less", mu=900) 
pvalue

#At the 1% significance level, the data support the researcher’s claim. Because we can reject the null hypotheis 	
#It is because the p-value is less than 0.01, which means with more than 99% probability we can say that People in Massachusates spends less than $900 on Lottery	



##Question-4
df4=read.csv("MPG1.csv")
pvalue=t.test(df4$MPG, alternative="two.sided", mu=50) 
pvalue

##Conclusion: Yes, at alpha=0.05, we can conclude that MPG is significantly differs from 50.				
#It is because p-value is less than 0.05				



### Regression ####

#Question-1

#a)
myData= read_excel("jaggia_ba_2e_ch07_data.xlsx", sheet="College")   ####Reading the file#######

#b)
options(scipen=999)  ##This is to turn off the scientific notations. To turn is on back write options(scipen=0)

#c)
Model=lm(Earnings~Cost+Grad+Debt+City, data=myData)    #This is running a basic linear regression. 
summary(Model)                                         #This prints the summary i.e. coefficients, p-values and R-square etc. 

#d) 
predict(Model, data.frame(Cost=25000, Grad=60, Debt=80, City=1))   #Predicting 'Earnings' for a particular value 





##Question-1
df5=read_excel("Education.xlsx")
model=lm(Salary ~ Education, data=df5)
summary(model)

####Conlusions and Answers of Q5 ###
# i)  β1 is statistically significant at 95% confidence level or 0.05 significance level. Further we can say that it is significance at 0.01 level of significance because p-value is far less than 0.01												
# ii) If Education increases by 1 year, on average, salary increases between 8.29 units to 13.41 units with 95% confidence. 												
#iii) Predictied salary of someone who completed seven years of Education is equal to = 21.95 + 10.85x7 = 97.90												
#iv) Yes, the model is significant because p-value associated with F-test is less than 0.05, which means with more than 95% confidence we can reject the claim that model is insignificant 												
