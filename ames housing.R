install.packages("data.table")   #installing package data.table library
install.packages("plyr")   #installing package plyr library
install.packages("FSA")   #installing package FSA library
install.packages("FSAdata")   #installing package FSA data library
install.packages("magrittr")   #installing package magrittr library
install.packages("dplyr")   #installing package dplyr library
install.packages("plotrix")   #installing package plotrix library
install.packages("ggplot2")   #installing package ggplot2 library
install.packages("moments")   #installing package moments library
install.packages("lifecycle")  # Install the lifecycle package
install.packages("tidyverse")
install.packages("corrplot")
install.packages("car")
install.packages("leaps")
library(leaps)
library(car)
library(corrplot)
library(tidyverse)
library(lifecycle)# Load the lifecycle packagel
library(dplyr) # Load the dplyr package
library(plyr)  # importing plyr library
library(data.table) #importing data.table library
library(FSA)  # import FSA library
library(FSAdata)  # import FSAdata library
library(magrittr)  # import magrittr library
library(dplyr)  # import dplyr library
library(plotrix)  # import plotrix library
library(ggplot2)  # import ggplot2 library
library(moments)  # import moments library(package


#q1
ameshousing<-read.csv("C:\\Users\\heman\\Downloads\\AmesHousing.csv")

#q2
summary(ameshousing)

# Create a bar plot for Overall Quality
barplot(table(ameshousing$Overall.Qual),
        main = "Distribution of Overall Quality",
        xlab = "Overall Quality",
        ylab = "Count",
        col = "skyblue")


# Create a bar plot for Overall Condition
barplot(table(ameshousing$Overall.Cond),
        main = "Distribution of Overall Condition",
        xlab = "Overall Condition",
        ylab = "Count",
        col = "salmon")

#q3
na_count <- sapply(ameshousing, function(ames) sum(length(which(is.na(ameshousing)))))
na_count

clean_ameshousing <- ameshousing
for(i in 1:ncol(clean_ameshousing)) {
  clean_ameshousing[, i][is.na(clean_ameshousing[, i])] <- mean(clean_ameshousing[, i], na.rm = TRUE)
}
clean_ameshousing

#q4
final_ameshousing1 <- clean_ameshousing %>%
  mutate_if(is.character, as.numeric)

final_ameshousing2 <- final_ameshousing1 %>%
  mutate_if(is.integer, as.numeric) %>%
  select(where(~ !all(is.na(.))))
final_ameshousing2
cor_data <- cor(final_ameshousing2, use = 'pairwise', method = 'pearson')
cor_data

#q5
corrplot(cor_data, type = 'upper')

#q6
# Scatter plot for highest Correlation with SalesPrice
plot(final_ameshousing2$Overall.Qual, final_ameshousing2$SalePrice,
     main = "Scatter plot for highest Correlation with SalesPrice",
     xlab = "overall quality of the house ",
     ylab = "Sales Price",
     col = "brown")

# Scatter plot for lowest Correlation with SalesPrice
plot(final_ameshousing2$BsmtFin.SF.2, final_ameshousing2$SalePrice,
     main = "Scatter plot for lowest Correlation with SalesPrice",
     xlab = "Basement finished sqrfeet 2 ",
     ylab = "Sales Price",
     col = "cyan")

# Scatter plot for 0.5 Correlation with SalesPrice
plot(final_ameshousing2$Mas.Vnr.Area, final_ameshousing2$SalePrice,
     main = "Scatter plot for 0.5 Correlation with SalesPrice",
     xlab = "Masonry veneer area",
     ylab = "Sales Price",
     col = "pink")

#q7
fitmodel <- lm(formula = final_ameshousing2$SalePrice ~ final_ameshousing2$Gr.Liv.Area + 
                 final_ameshousing2$X1st.Flr.SF + final_ameshousing2$X2nd.Flr.SF)
fitmodel

#q8
summary(fitmodel)

#q9
plot(fitmodel)

#q10
vif(fitmodel)

#q11
boxplot(final_ameshousing2$Gr.Liv.Area, final_ameshousing2$X1st.Flr.SF, final_ameshousing2$X2nd.Flr.SF,
        main = "Boxplot of the model variables",
        col = "light green")

#q12
# Defining and fit a linear regression model
model <- lm(SalePrice ~ Overall.Qual + Total.Bsmt.SF + Gr.Liv.Area, data = final_ameshousing2)

# Summarize the model
summary(model)


#q13
# Fit the all subsets regression model
all_subsets_model <- regsubsets(SalePrice ~ ., data = final_ameshousing2, nvmax = ncol(final_ameshousing2))

# Summary of the all subsets model
summary(all_subsets_model)

#Adjusted R2 plot
plot(all_subsets_model, scale = "adjr2", main = "Adjusted R^2")






