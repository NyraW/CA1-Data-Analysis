# Import the World-happiness data frame
world_happiness_data <- read.csv("World-happiness.csv", na = "")

# Check for missing variables and examine missing data
# List rows with missing values
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

# Remove all missing data using this code
# Remove any rows that contain NA using listwise deletion
world_happiness_data <- na.omit(world_happiness_data)
world_happiness_data

# Visualise the data for missing vars
# Check to see if the missing data has been removed by using the VIM package
# install packages("VIM")
library(VIM)
incomplete_data <- aggr(world_happiness_data, prop = FALSE, numbers = TRUE)
summary(incomplete_data)

# Look at the structure of the data frame
str(world_happiness_data)

# Research Question 1 Hypothesis Test
# H0: Positive affect is not affected by freedom to make life choices
# H1: Positive affect is affected by freedom to make life choices

# Positive affect is a numerical variable
# Freedom to make life choices is a numerical variable

# Install pysch library to look at charts and correlations between variables
install.packages("psych")
library(psych)

# Using the default plot() option first to examine correlations between variables and to check linearity
# Evaluate the strength of the relationship
# Determine if the relationship is negative or positive
attach(world_happiness_data)
plot(Positive.affect, 
     Freedom.to.make.life.choices, 
     pch = 19, 
     col = "blue",
     main = "Correlation of Positive Affect with Freedom to Make Life Choices", 
     xlab = "Positive Affect", 
     ylab = "Freedom to Make Life Choices")

# We can use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Positive.affect, Freedom.to.make.life.choices,
            main = "Comparing Positive Affect and Freedom to Make Life Choices",
            xlab = "Positive Affect",
            ylab = "Freedom to Make Life Choices"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Positive.affect, col = "blue", main = "Distribution of Positive Affect")
hist(Freedom.to.make.life.choices, col = "blue", main = "Disribution of Freedom to Make Life Choices")
par = opar

# Create a normal QQ plot of Positive affect and Freedom to make life choices variables
# Add the normality line to evaluate normality
qqnorm(world_happiness_data$Positive.affect,
       main = "Normal QQ plot of Positive Affect data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Positive.affect, col = 'red')

qqnorm(world_happiness_data$Freedom.to.make.life.choices,
       main = "Normal QQ plot of Freedom to Make 
       Life Choices data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Freedom.to.make.life.choices, col = 'red')

# Check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the data is distributed normally
# Compare the quantiles of both samples 

# possible delete
attach(world_happiness_data)
qqnorm(Positive.affect)

# This code creates a line that represents normal distribution
qqline(Positive.affect, col = "red")

# Possible delete
# Using a QQ plot to check for normality
# qqnorm function plots your sample against a normal distribution
with(world_happiness_data, {
  qqnorm(Positive.affect[Freedom.to.make.life.choices], 
         main = "Inactive")
})

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable positive affect

normality_test <- shapiro.test(world_happiness_data$Positive.affect)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 1.338313e-17
# The p-value is lower than 0.05 so the positive affect variable is not normally distributed

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable freedom to make life choices

normality_test <- shapiro.test(world_happiness_data$Freedom.to.make.life.choices)
normality_test$p.value

# p-value = 2.168792e-20
# The p-value is lower than 0.05 so the freedom to make life choices variable is not normally distributed

# The p-value for the dependent variable "freedom to make life choices" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (freedom to make life choices) and an independent continuous variable (positive affect)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Freedom to Make Life
# independent variable = Positive Affect

corr <- cor.test(world_happiness_data$Positive.affect, world_happiness_data$Freedom.to.make.life.choices, method = 'spearman')
corr

# Spearman correlation = 0.6139752
# p-value < 2.2e-16
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Positive affect is affected by freedom to make life choices (p = 0.614)
# r2 = rho


# Research Question 2 Hypothesis Test
# H0: Negative affect is not affected by freedom to make life choices
# H1: Negative affect is affected by freedom to make life choices


