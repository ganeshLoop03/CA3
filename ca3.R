# reading BMI data file 
# data set consists of height and weight of about 10000 men and female in Ireland 
# during the year 2018 were considered to find the solution of research question 

Height_Weight_Data <- read.csv("test1.csv")
Height_Weight_Data
str(Height_Weight_Data)


Height_Weight_Data
# research question 
# What is the percentage of difference in between women's and men's BMI
# (Body Mass Index) rates in Ireland during year 2018?


# function to calclate BMI using Height and weight
BMI = function(Height, Weight){
  return(0.45455*Weight/(.0254*Height)^2)
}


# testimg bmi function
BMI(71,180)

# adding bmi column in dataframe
Height_Weight_Data$bmi = BMI(Height_Weight_Data$Height,Height_Weight_Data$Weight)

# creating subset to get desired columns
Overweight_obesity <- subset (Height_Weight_Data ,select=c(Gender, bmi))
str(Overweight_obesity)


# exploring data set 
# plotting Female and Male obesity rate
library(ggplot2)
ggplot(Overweight_obesity, aes(x=Gender, y= bmi))+
  geom_boxplot()+
  ggtitle("Figure 1: Female and Male BMI rate")
str(Overweight_obesity)


# Hypothesis 

# H0: There is no significant difference in the Obesity rate among male and Female population during period 2018 or mens BMI < womens BMI
# Ha: There is a significant difference in the Obesity rate among male and Female male and Female population during period 2018

# creating subsets for two different groups
Male <- subset(Overweight_obesity,  Gender == "Male", select=c(Gender, bmi))
Male
str(Male)

Female <- subset(Overweight_obesity,  Gender == "Female", select=c(Gender, bmi))
Female
str(Female)

#########################################################
# to check if data is normally distributed or not

library("lattice")

# the histogram uses a 1 sided formulae so we dont specify
# anything on left side of ~ and on the righ we secify which variable is in 
head(Overweight_obesity)

histogram(~bmi | 
            Gender, data = Overweight_obesity)

with(Overweight_obesity,
     qqplot(bmi[Gender == "Male"],
            bmi[Gender == "Female"], 
            main = "Comparing 2 samples", 
            xlab = "x",
            ylab = "y"))


# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution
with(Overweight_obesity, {
  qqnorm(bmi[Gender == "Male"], 
         main = "bmi$gender")
})

# We can add normailty line 
# to the plot to evaluate normality
# for group men
with(Overweight_obesity, {
  qqnorm(bmi[Gender == "Male"], 
         main = "BMI rate male ")
  qqline(bmi[Gender == "Male"])
})


with(Overweight_obesity, {
  qqnorm(bmi[Gender == "Female"], 
         main = "BMI rate Female")
  qqline(bmi[Gender == "Female"])
})



# We can check the normality in each variable

# using the tapply() function
with(Overweight_obesity, tapply(bmi, Gender, shapiro.test))


# in this example , P-value is clearly less than 0.05
# so data it is not normally distributed 

###################################################################


# to check the  sample size, effect size, desired significance level, and desired power


n = nrow(Overweight_obesity)            # The number of observations in Overweight_obesity
n

# standard deviation for group together
SD <- sd(Overweight_obesity$bmi)
SD

# standard deviation for individual groups
SD_M <- sd(Male$bmi) # Formula for the standard Deviation
SD_M

SD_F <-sd(Female$bmi)
SD_F

# to find mean of group 1 and 2
FemaleMean <- mean(filter(Overweight_obesity, Gender=="Female")$bmi)
FemaleMean
MaleMean <- mean(filter(Overweight_obesity, Gender=="Male")$bmi)
MaleMean

# to find value of d
d <- (MaleMean - FemaleMean)/SD
d


# to test power
power.t.test(n=5000,delta=0.05,sd=SD, type = c("two.sample"))

# power = 0.89

# to determine sample size
pwrTest <- power.t.test(delta=0.05, sd=SD, power=0.89)
pwrTest
plot(pwrTest)


####################################################

# to find sample effect size
# h = 0.20: “small effect size”.
# h = 0.50: “medium effect size”.
# h = 0.80: “large effect size”.

# m1 = mean of group1
# m2 = mean of group2
# sd1= standard deviation of group1
# sd2= standard deviation of group2
# n1= total observation in group1
# n2= total observation in group2



# Calculating effect size 


install.packages("powerAnalysis")
library(powerAnalysis)
effect_size <- ES.t.two(m1 = MaleMean, m2 = FemaleMean, sd1 = SD_M, sd2 = SD_F, n1 = 5000,
         n2 = 5000, alternative = c("two.sided"))
effect_size


#######################

# sample size is 4973 from both group

Male <- subset(Overweight_obesity,  Gender == "Male", select=c(bmi))
Male
str(Male)

Male_Sample <- Male[1:4973,]

Male_Sample

Female <- subset(Overweight_obesity,  Gender == "Female", select=c(bmi))
str(Female)
Female_Sample <- Female[1:4973,]


Female_Sample

# wilcox test


res <- wilcox.test(Male_Sample, Female_Sample)
res


#The p-value of the test is 2.2e-16, which is lesser than the significance level alpha = 0.05.
# hence we can reject null hypothesis 
# There is a significant difference in the Obesity rate among male and Female population during 2018 p-value = 2.2e-16

# wilcox test 


################################################################
# DATA ANALYSIS AND STATISTICAL INFERENCE
# Nonparametric bootstrap to find the standard error of the difference of the mean
# difference between mean bmi is represented as Xbar
# and Ybar
library(dplyr)

Ybar <- mean(filter(Overweight_obesity, Gender=="Female")$bmi)
Ybar
Xbar <- mean(filter(Overweight_obesity, Gender=="Male")$bmi)
Xbar
diff <- Ybar - Xbar
diff
# The plug in estimator for the difference between the two are calculated as diff=Ybar-Xbar -4.116154


# to find 95% confidence interval using 
install.packages("bootstrap")
library(bootstrap)
psigma <- diff
Mmean <- bootstrap(x=filter(Overweight_obesity, Gender=="Female")$bmi,23,mean)
Fmean <- bootstrap(x=filter(Overweight_obesity, Gender=="Male")$bmi,27,mean)
Fmean
Mmean

# bootstap standard error is 0.0422
se.boot <- sqrt(var(Fmean$thetastar-Mmean$thetastar))
se.boot
CI.boot <-c(psigma-2*se.boot, psigma+2*se.boot)
CI.boot

sigmahat <- Ybar-Xbar
Fe <- filter(Overweight_obesity, Gender=="Female")$bmi
M <- filter(Overweight_obesity, Gender=="Male")$bmi
S.F <- sum((Fe-Ybar)^2/nrow(filter(Overweight_obesity, Gender=="Female")))
S.M <- sum((M-Xbar)^2/nrow(filter(Overweight_obesity, Gender=="Male")))
sehat <- sqrt(S.F/nrow(filter(Overweight_obesity, Gender=="Female"))+S.M/nrow(filter(Overweight_obesity, Gender=="Male")))
CI.normal <- c(sigmahat-2*sehat, sigmahat+2*sehat)


# Wald test of hypothesis
sigmahat <- Ybar -Xbar
W <- sigmahat/sehat
pvalue <- 2*pnorm(-abs(W))
pvalue

#The p-value of the test is 0, which is lesser than the significance level alpha = 0.05.
# hence we can reject null hypothesis 
# There is a significant difference in the Obesity rate among male and Female population during 2018.
