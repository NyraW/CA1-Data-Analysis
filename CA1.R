# Import the World-happiness data frame
world_happiness_data <- read.csv("World-happiness.csv", na = "")

# Check for missing vars and examine missing data
# List rows with missing values
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

# Remove any rows that contain NA using listwise deletion
world_happiness_data <- na.omit(world_happiness_data)
world_happiness_data

# Visualise the data for missing variables
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

# Use the psych library to look at charts and correlations between variables
install.packages("psych")
library(psych)

# Use the default plot() option to examine correlations between variables and to check linearity
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

# We can also use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Positive.affect, Freedom.to.make.life.choices,
            main = "Correlation of Positive Affect with Freedom to Make Life Choices",
            xlab = "Positive Affect",
            ylab = "Freedom to Make Life Choices"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Positive.affect, col = "blue", main = "Distribution of Positive Affect")
hist(Freedom.to.make.life.choices, col = "blue", main = "Disribution of Freedom to Make Life Choices")
par = opar

# Check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the data is distributed normally
# Create a normal QQ plot of Positive affect and Freedom to make life choices variables
# Add the normality line to evaluate normality
# Compare the quantiles of both samples
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

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable positive affect
normality_test <- shapiro.test(world_happiness_data$Positive.affect)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 1.338313e-17
# The p-value is lower than 0.05 so the positive affect variable is not normally distributed

# Normality test for the variable freedom to make life choices
normality_test <- shapiro.test(world_happiness_data$Freedom.to.make.life.choices)
normality_test$p.value

# p-value = 2.168792e-20
# The p-value is lower than 0.05 so the freedom to make life choices variable is not normally distributed

# The p-value for the dependent variable "freedom to make life choices" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (freedom to make life choices) and an independent continuous variable (positive affect)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Freedom to Make Life Choices
# independent variable = Positive Affect

corr <- cor.test(world_happiness_data$Positive.affect, world_happiness_data$Freedom.to.make.life.choices, method = 'spearman')
corr

# Spearman correlation = 0.6139752
# p-value < 2.2e-16
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Positive affect is affected by freedom to make life choices (p < 2.2e-16)


# Research Question 2 Hypothesis Test
# H0: Healthy life expectancy at birth is not affected by Log GDP per capita
# H1: Healthy life expectancy at birth is affected by Log GDP per capita

# Healthy life expectancy is a numerical variable
# Log GDP per capita is a numerical variable

# Use the pysch library to look at charts and correlations between variables
# Use the default plot() option to examine correlations between variables and to check linearity
# Evaluate the strength of the relationship
attach(world_happiness_data)
plot(Healthy.life.expectancy.at.birth, 
     Log.GDP.per.capita, 
     pch = 19, 
     col = "blue",
     main = "Correlation of Healthy Life Expectancy at Birth with Log GDP per capita", 
     xlab = "Healthy Life Expectancy at Birth", 
     ylab = "Log GDP per capita")

# We can also use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Healthy.life.expectancy.at.birth, Log.GDP.per.capita,
            main = "Correlation of Healthy Life Expectancy at Birth with Log GDP per capita",
            xlab = "Healthy Life Expectancy at Birth",
            ylab = "Log GDP per capita"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Healthy.life.expectancy.at.birth, col = "blue", main = "Distribution of Healthy Life Expectancy at Birth")
hist(Log.GDP.per.capita, col = "blue", main = "Disribution of Log GDP per capita")
par = opar

# Check whether the data is normally distributed or not
# QQ plots allows us to check if the data is distributed normally
# Create a normal QQ plot of Healthy Life Expectancy at Birth and Log GDP per capita variables
# Add the normality line to evaluate normality
qqnorm(world_happiness_data$Healthy.life.expectancy.at.birth,
       main = "Normal QQ plot of Healthy Life Expectacy 
       at Birth data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Healthy.life.expectancy.at.birth, col = 'red')

qqnorm(world_happiness_data$Log.GDP.per.capita,
       main = "Normal QQ plot of Log GDP per capita data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Log.GDP.per.capita, col = 'red')

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable Healthy Life Expectancy at birth
normality_test <- shapiro.test(world_happiness_data$Healthy.life.expectancy.at.birth)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 3.403829e-24
# The p-value is lower than 0.05 so the Healthy Life Expectancy at birth variable is not normally distributed

# Normality test for the variable Log GDP per capita
normality_test <- shapiro.test(world_happiness_data$Log.GDP.per.capita)
normality_test$p.value

# p-value = 3.411721e-19
# The p-value is lower than 0.05 so the Log GDP per capita variable is not normally distributed

# The p-value for the dependent variable "Log GDP per capita" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (Log GDP per capita) and an independent continuous variable (Healthy Life Expectancy at Birth)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Log GDP per capita
# independent variable = Healthy Life Expectancy at Birth

corr <- cor.test(world_happiness_data$Healthy.life.expectancy.at.birth, world_happiness_data$Log.GDP.per.capita, method = 'spearman')
corr

# Spearman correlation = 0.878184
# p-value < 2.2e-16
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Healthy Life Expectancy at Birth is affected by Log GDP per capita (p < 2.2e-16)


# Research Question 3 Hypothesis Test
# H0: Generosity is not affected by social support
# H1: Generosity is affected by social support

# Generosity is a numerical variable
# Social support is a numerical variable

# Use pysch library to look at charts and correlations between variables
# Use the default plot() option first to examine correlations between variables and to check linearity
# Evaluate the strength of the relationship
attach(world_happiness_data)
plot(Generosity, 
     Social.support, 
     pch = 19, 
     col = "blue",
     main = "Correlation of Generosity with Social Support", 
     xlab = "Generosity", 
     ylab = "Social Support")

# We can also use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Generosity, Social.support,
            main = "Correlation of Generosity with Social Support",
            xlab = "Generosity",
            ylab = "Social Support"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Generosity, col = "blue", main = "Distribution of Generosity")
hist(Social.support, col = "blue", main = "Disribution of Social Support")
par = opar

# Check whether the data is normally distributed or not
# QQ plots allows us to check if the data is distributed normally
# Create a normal QQ plot of Generosity and Social Support variables
# Add the normality line to evaluate normality
qqnorm(world_happiness_data$Generosity,
       main = "Normal QQ plot of Generosity data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Generosity, col = 'red')

qqnorm(world_happiness_data$Social.support,
       main = "Normal QQ plot of Social Support",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Social.support, col = 'red')

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable generosity
normality_test <- shapiro.test(world_happiness_data$Generosity)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 2.506541e-20
# The p-value is lower than 0.05 so the generosity variable is not normally distributed

# Normality test for the variable social support
normality_test <- shapiro.test(world_happiness_data$Social.support)
normality_test$p.value

# p-value = 5.53173e-30
# The p-value is lower than 0.05 so the social support variable is not normally distributed

# The p-value for the dependent variable "social support" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (social support) and an independent continuous variable (generosity)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Social Support
# independent variable = Generosity

corr <- cor.test(world_happiness_data$Generosity, world_happiness_data$Social.support, method = 'spearman')
corr

# Spearman correlation = 0.07872036 
# p-value = 0.00113
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Generosity is affected by social support (p = 0.00113)


# Research Question 4 Hypothesis Test
# H0: Negative affect is not affected by perceptions of corruption
# H1: Negative affect is affected by perceptions of corruption

# Negative affect is a numerical variable
# Perceptions of corruption is a numerical variable

# Use pysch library to look at charts and correlations between variables
# Using the default plot() option first to examine correlations between variables and to check linearity
# Evaluate the strength of the relationship
attach(world_happiness_data)
plot(Negative.affect, 
     Perceptions.of.corruption, 
     pch = 19, 
     col = "blue",
     main = "Correlation of Negative Affect with Perceptions of Corruption", 
     xlab = "Negative Affect", 
     ylab = "Perceptions of Corruption")

# We can also use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Negative.affect, Perceptions.of.corruption,
            main = "Correlation of Negative Affect with Perceptions of Corruption",
            xlab = "Negative Affect",
            ylab = "Perceptions of Corruption"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Negative.affect, col = "blue", main = "Distribution of Negative Affect")
hist(Perceptions.of.corruption, col = "blue", main = "Disribution of Perceptions of Corruption")
par = opar

# Check whether the data is normally distributed or not
# QQ plots allows us to check if the data is distributed normally
# Create a normal QQ plot of Negative affect and Perceptions of corruption variables
# Add the normality line to evaluate normality
qqnorm(world_happiness_data$Negative.affect,
       main = "Normal QQ plot of Negative Affect data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Negative.affect, col = 'red')

qqnorm(world_happiness_data$Perceptions.of.corruption,
       main = "Normal QQ plot of Perceptions of 
       Corruption data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Perceptions.of.corruption, col = 'red')

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable negative affect
normality_test <- shapiro.test(world_happiness_data$Negative.affect)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 3.536878e-20
# The p-value is lower than 0.05 so the negative affect variable is not normally distributed

# Normality test for the variable perceptions of corruption
normality_test <- shapiro.test(world_happiness_data$Perceptions.of.corruption)
normality_test$p.value

# p-value = 1.132364e-38
# The p-value is lower than 0.05 so the perceptions of corruption variable is not normally distributed

# The p-value for the dependent variable "perceptions of corruption" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (perceptions of corruption) and an independent continuous variable (negative affect)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Perceptions of Corruption
# independent variable = Negative Affect

corr <- cor.test(world_happiness_data$Negative.affect, world_happiness_data$Perceptions.of.corruption, method = 'spearman')
corr

# Spearman correlation = 0.218592
# p-value < 2.2e-16
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Negative affect is affected by perceptions of corruption (p < 2.2e-16)


# Research Question 5 Hypothesis Test
# H0: Life Ladder is not affected by Log GDP per capita
# H1: Life Ladder is affected by Log GDP per capita

# Life Ladder is a numerical variable
# Log GDP per capita is a numerical variable

# use pysch library to look at charts and correlations between variables
# Using the default plot() option first to examine correlations between variables and to check linearity
# Evaluate the strength of the relationship
attach(world_happiness_data)
plot(Life.Ladder, 
     Log.GDP.per.capita, 
     pch = 19, 
     col = "blue",
     main = "Correlation of Life Ladder with Log GDP per capita", 
     xlab = "Life Ladder", 
     ylab = "Log GDP per capita")

# We can also use a QQ plot to show the correlation between variables
with(world_happiness_data,
     qqplot(Life.Ladder, Log.GDP.per.capita,
            main = "Correlation of Life Ladder with Log GDP per capita",
            xlab = "Life Ladder",
            ylab = "Log GDP per capita"))

# Visualise the normality of the variables
opar = par(no.readonly = TRUE)
# Arrange the plots in 1 row and 2 columns
par(mfrow = c(1,2))
hist(Life.Ladder, col = "blue", main = "Distribution of Life Ladder")
hist(Log.GDP.per.capita, col = "blue", main = "Disribution of Log GDP per capita")
par = opar

# Check whether the data is normally distributed or not
# QQ plots allows us to check if the data is distributed normally
# Create a normal QQ plot of Life Ladder and Log GDP per capita variables
# Add the normality line to evaluate normality
qqnorm(world_happiness_data$Life.Ladder,
       main = "Normal QQ plot of Life Ladder data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Life.Ladder, col = 'red')

qqnorm(world_happiness_data$Log.GDP.per.capita,
       main = "Normal QQ plot of Log GDP 
       per capita data",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(world_happiness_data$Log.GDP.per.capita, col = 'red')

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable life ladder
normality_test <- shapiro.test(world_happiness_data$Life.Ladder)
normality_test$p.value

# p-value tells us the chances that the sample comes from a normal distribution
# p-value = 7.415742e-13
# The p-value is lower than 0.05 so the life ladder variable is not normally distributed

# Normality test for the variable Log GDP per capita
normality_test <- shapiro.test(world_happiness_data$Log.GDP.per.capita)
normality_test$p.value

# p-value = 3.411721e-19
# The p-value is lower than 0.05 so the Log GDP per capita variable is not normally distributed

# The p-value for the dependent variable "Log GDP per capita" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (Log GDP per capita) and an independent continuous variable (Life ladder)
# So I need to use the Spearman’s Correlation Coefficient test

# dependent variable = Log GDP per capita
# independent variable = Life Ladder

corr <- cor.test(world_happiness_data$Life.Ladder, world_happiness_data$Log.GDP.per.capita, method = 'spearman')
corr

# Spearman correlation = 0.8076649 
# p-value < 2.2e-16
# cut-off = 0.05

# The p-value is < 0.05 so we reject H0 and conclude that 
# Life Ladder is affected by Log GDP per capita (p < 2.2e-16)