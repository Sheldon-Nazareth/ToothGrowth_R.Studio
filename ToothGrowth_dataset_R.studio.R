#Q1::#Part 1
#In the first portion of the project, we're going to analyze the ToothGrowth data in the R datasets package.

#1 - Load the ToothGrowth data and perform some basic exploratory data analyses
#2 - Provide a basic summary of the data.
#3 - Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose.
#4 - State your conclusions and the assumptions needed for your conclusions.

data(ToothGrowth)
? ToothGrowth 
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth)
library(ggplot2)
#Exploratory data analysis and visualization

#for comparison between OJ and VC for their tooth lengths
plotly::ggplotly(ggplot(ToothGrowth, aes(y = len, x = supp)) + 
                   geom_boxplot())
#OJ admitted suppliments have a higher mean value, however the suppliments administered through VC have extreme values of length.

#to compare the does in terms of length we can use/
plotly::ggplotly(ggplot(ToothGrowth, aes(y = len, x = dose)) + 
                   geom_boxplot())
##The higher the does the greater the toth length turns out to be.

#To visualize the relationship between dose and length distinguishing the does in terms of their suppliment type.
p = ggplot(ToothGrowth, aes(x = dose, y = len))
p = p + geom_point(aes(color = supp, shape = supp), size = 5)
p
# As the value of the dose increases there's a significant increase in the tooth length for both forms of suppliment administration.

#Let's prove this through hypothesys testing!!!
#we have numeric data dependent on categorical variables so we use p and t tests
#To compare toothgrowth by supp:::::::::
#Null hypithesys = Ho: There is no difference between OJ and VC: mu -0
#The Alternatite hypothesys = Ha: OJ>VC

#Organizing dataset for t.test 
# split data set
o_j = ToothGrowth$len[ToothGrowth$supp == 'OJ']
v_c = ToothGrowth$len[ToothGrowth$supp == 'VC']
o_j
v_c
#with a confidence interval of 95% 
t.test(o_j, v_c, alternative = "greater")
# p-value:3% < 5% Thus we reject the null hypothesis. 
#OJ has a more significant affect on the length of tooth. 

#To compare toothgrowth by dose:::::::::
#Null hypithesys = Ho: half_dose = one_dose = two_dose = mu =0
#The Alternatite hypothesys = Ha: As the dosage increases the tooth growth increases half_dose < one_dose < two_dose
half_dose = ToothGrowth$len[ToothGrowth$dose == 0.5]
one_dose = ToothGrowth$len[ToothGrowth$dose == 1]
two_dose = ToothGrowth$len[ToothGrowth$dose == 2]
#with a confidence interval of 95% 
t.test(half_dose, one_dose, alternative = "less")
#Since the p-values is extremly small so we reject the null hypothesis for the first case. 
t.test(one_dose,two_dose, alternative = "less")
#again the null hypothesis is rejected 
#We xan conclude with the low p-values that the amount of dose has different affects on the lent of toothgrowth the larger the
#amount the greater the toothgrowth
#As proved with the visualizations above !!!



###########################################################################################################
#Q2:Part 2 ### The fish-diet dataset ###
#Medical researchers followed 6272 Swedish men for 30 years to see whether there was any association between the amount of fish in
#their diet and prostate cancer. This is their data. Conduct the appropriate tests and report your findings using the appropriate 
#language.

fish_diet <- read.csv("C:\\Users\\ND.COM\\Downloads\\fish-diet.csv")
View(fish_diet)
unique(fish_diet$fish_in_diet)
unique(fish_diet$cancer)
str(fish_diet)

#for some visualization
barplot(table(fish_diet$cancer, fish_diet$fish_in_diet),legend = unique(fish_diet$cancer) , beside = TRUE,
        xlab = "Amount of fish in diet", ylab = "Magnitude of test applicants")


?chisq.test
# Create a contingency table/crosstab with the needed categorical variables. 
#To determine if they are independent of each other or not .
table(fish_diet$fish_in_diet, fish_diet$cancer)

# Perform the Chi-Square test.
chisq.test(table(fish_diet$fish_in_diet, fish_diet$cancer))
#The p-values in this case in 29% > 5% this large value of p means that there's no specific relationship between cancer and 
#including fish in your diet.
