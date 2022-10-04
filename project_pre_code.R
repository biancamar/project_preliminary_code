## Preliminary Report

## By Erika Kirkpatrick and Bianca Rodriguez
## Using R version 4.1.2

## Libraries

library(dplyr)

## Question options:
## 1) Can the height, weight, and breed of a dog affect intelligence? 
## 2) Is there a correlation between dog height/weight and intelligence? 

## Read in the data
dog_data <- read.csv("C:/Users/arizo/Documents/Math 363/Math Project/Dog Intelligence.csv")

## Cleaning the data

dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "A0tolian Sheepdog", "Australian Sheepdog")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Fox Terrier ???лв Smooth", "Smooth Fox Terrier")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Fox Terrier ???лв Wirehair", "Wirehair Fox Terrier")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Saint Ber0rd", "Saint Bernard")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Giant Sch0uzer", "Giant Schnauzer")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Ca0an Dog", "Canaan Dog")
dog_data$Breed <- replace(dog_data$Breed, dog_data$Breed == "Standard Sch0uzer", "Standard Schnauzer")

## Removing the NA Values

dog_data_no_na <- na.omit(dog_data)

## Removing the 0 Values in height and weight

dog_data_no_na <- dog_data_no_na[!(dog_data_no_na$Breed=="Alaskan Malamute" | dog_data_no_na$Breed=="Coton de Tulear"),]

## Creating Columns to Hold the Averages of Weight, Height and Reps

## Average Height
dog_data_no_na <- mutate(dog_data_no_na, average_height = (height_low_inches + height_high_inches)/2)

## Average Weight
dog_data_no_na <- mutate(dog_data_no_na, average_weight = (weight_low_lbs + weight_high_lbs)/2)

## Average Reps
dog_data_no_na <- mutate(dog_data_no_na, average_reps = (reps_lower + reps_upper)/2)

## Creating Plots for Question 1 / Question 2

## Scatterplot of Average Height and Average Reps

plot(dog_data_no_na$average_height, dog_data_no_na$average_reps, xlab = "Average Height (Inches)", ylab="Average Repetitions", main= "Scatterplot of Average Height and Average Repetitions", sub = "Coren, S. (2022, August 10). Dog Intelligence Comparison Based on Size.")
## Scatterplot of Average Weight and Average Reps

plot(dog_data_no_na$average_weight, dog_data_no_na$average_reps, xlab = "Average Weight (lbs)", ylab="Average Repetitions", main= "Scatterplot of Average Weight and Average Repetitions", sub = "Coren, S. (2022, August 10). Dog Intelligence Comparison Based on Size.")


## Multiple Linear Relationship between Average Height/Weight and Average Repetitions
dog_data_lm <- lm(average_reps~average_height+average_weight, data=dog_data_no_na)

summary(dog_data_lm)
summary(dog_data_no_na)

## Correlation Between Average Height and Average Weight
cor(dog_data_no_na$average_height, dog_data_no_na$average_weight)

## Correlations between Average Height/Weight and Average Reps
cor(dog_data_no_na$average_height, dog_data_no_na$average_reps)
cor(dog_data_no_na$average_weight, dog_data_no_na$average_reps)
