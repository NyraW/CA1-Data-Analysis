# Import the World-happiness data frame
world_happiness_data <- read.csv("World-happiness.csv", na = "")

# Check for missing variables and examine missing data
# List rows with missing values
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

# Remove all missing data using this code
# Remove any rows that contain NA - listwise deletion
world_happiness_data <- na.omit(world_happiness_data)
world_happiness_data

# Check to see if the missing data has been removed
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

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

# Using the default plot() option first to examine correlations between variables
# Evaluate the strength of the relationship
# Determine if the relationship is negative or positive
attach(world_happiness_data)
plot(Freedom.to.make.life.choices, Positive.affect, pch = 19, col = "red")

pairs(world_happiness_data, labels = colnames(world_happiness_data), main = "World Happiness dataset correlation plot")

pairs.panels(world_happiness_data, 
             smooth = FALSE, # If TRUE, draws loess smooths  
             scale = FALSE, # If TRUE, scales the correlation text font  
             density = TRUE, # If TRUE, adds density plots and histograms  
             ellipses = FALSE, # If TRUE, draws ellipses   
             method = "spearman",# Correlation method (also "pearson" or "kendall") 
             pch = 21, # pch symbol   
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered  
             factor = 2, # Jittering factor  
             hist.col = 4, # Histograms color   
             stars = TRUE,
             ci = TRUE) # If TRUE, adds confidence intervals 

# Check linearity 
attach(world_happiness_data)
plot(Positive.affect,
     Freedom.to.make.life.choices,
     pch = "19",
     col = "red", 
     main = "Comparison of Positive Affect and Freedom to Make Life Choices",
     xlab = " Positive Affect", ylab = "Freedom to Make Life Choices")

# Select the appropriate test
# Check whether the data is normally distributed or not

# Quantile-quantile plot allows us to check if the data is distributed normally
# Compare the quantiles of both samples 

attach(world_happiness_data)
qqnorm(Positive.affect)

# This code creates a line that represents normal distribution
qqline(Positive.affect, col = "red")

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
# The p-value is lower than 0.05 so the positive affect variable is not normally distributed

# Use this code to test for normality using the Shapiro-Wilks test
# Normality test for the variable freedom to make life choices

normality_test <- shapiro.test(world_happiness_data$Freedom.to.make.life.choices)
normality_test$p.value

# The p-value is lower than 0.05 so the freedom to make life choices variable is not normally distributed

# The p-value for the dependent variable "positive affect" indicates that the data is not normally distributed so we need to use a non-parametric test
# After looking at the plots and the p-value, I am examining a dependent continuous variable (positive affect) and an independent continuous variable (freedom to make life choices)
# So I used the Spearman’s Correlation Coefficient test

