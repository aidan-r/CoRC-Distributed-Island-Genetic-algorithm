library(tidyverse)

#cannot import with fewer column names
#standard_ga <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_300_20.txt")

#have to import data manually
#wondering how to set when the value gets recorded in COPASI


#Remove the parentheses columns from the data frame
Standard_GA_300_20 = subset(Standard_GA_300_20, select = -c(V3,V11))

ggplot(data = Standard_GA_300_20,mapping = aes(x = Standard_GA_300_20[,1],y = Standard_GA_300_20[,2])) +
  geom_point()


pop_one <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/output_one.csv")
pop_two <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/output_two.csv")

ggplot(data = pop_one,mapping = aes(x = pop_one[,1], y = pop_one[,3])) + 
  geom_point()
ggplot(data = pop_two,mapping = aes(x = pop_two[,1], y = pop_two[,3])) + 
  geom_point()