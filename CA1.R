# Import the World-happiness data frame
world_happiness_data <- read.csv("World-happiness.csv", na = "")

# Check for missing varaibles. Delete if there are any
# Examine missing data
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

# Look at the structure of the dataset
str(world_happiness_data)

# Research Question 1 Hypothesis Test
# H0: Positive affect is not affected by freedom to make life choices
# H1: Positive affect is affected by freedom to make life choices

# Positive affect is a numerical variable
# Freedom to make life choices is a numerical variable

# Install pysch library to look at charts and correlations between variables
install.packages("psych")
library(psych)

pairs.panels(world_happiness_data, 
             smooth = TRUE, # If TRUE, draws loess smooths  
             scale = FALSE, # If TRUE, scales the correlation text font  
             density = TRUE, # If TRUE, adds density plots and histograms  
             ellipses = TRUE, # If TRUE, draws ellipses   
             method = "spearman",# Correlation method (also "pearson" or "kendall") 
             pch = 21, # pch symbol   
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered  
             factor = 2, # Jittering factor  
             hist.col = 4, # Histograms color   
             stars = TRUE,
             ci = TRUE) # If TRUE, adds confidence intervals

# Use the default plot() option to examine correlations between the variables
pairs(world_happiness_data, labels = colnames(world_happiness_data), main = "World Happiness dataset correlation plot")
