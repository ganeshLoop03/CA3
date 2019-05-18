
# demo used the data set women two varibales H and W
str(women)
head(women)

simple_linear_model <- lm(weight ~ height, data = women)

simple_linear_model

# width = -87.52 + 3.45 * height
plot (women$height, women$weight,
      xlab = "Height",
      ylab = "weight",
      main = "scatter plot showing regression line from width to height")

abline(simple_linear_model)

summary(simple_linear_model)

# residuals provide quick view of the disstribution of the residuels 
# which bythe defination have a mean 0
# therfore the median should not be far from zero 
# and the min and max should be roughly equal to absolute value. 
# residuka standard error or the RSE, R2, F statistics are metrics
# used to check hoe the models fits the data

# the standard erro SE defines the accuracy of the beta coffeecients 
# for the given beta cofffeciets, the SE reflects how the coeffecients 
# varies under repeat sampling. 
# it can used to compute CI and t-statistics.


# the corelation coeffecient measure the level of association between two variables and 
# ranges from -1 (perfecet negative corelation) to +1 (perfect positive correlation )
# A value close to 0 = weak relationship 
# a low correlation (-0.2 <x <0.2) indicateds that a lot of the variation 
# of the outcome y against predictor x is unexplained and 
# we should then look for better predictors values

cor(women$height, women$weight)



