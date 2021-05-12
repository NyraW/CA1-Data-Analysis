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

# Look at the correlation between both variables 
# Evaluate the strength of the relationship
# Determine if the relationship is negative or positive
attach(world_happiness_data)
plot(Positive.affect, Freedom.to.make.life.choices, pch = 19, col = "red")


