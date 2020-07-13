rowSds

#cannot import with fewer column names
#Standard_ga <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_300_20.txt")

#have to import data manually
#wondering how to set when the value gets recorded in COPASI


#Remove the parentheses columns from the data frame
#Standard_GA_300_20 = subset(Standard_GA_300_20, select = -c(V3,V11))

#ggplot(data = Standard_GA_300_20,mapping = aes(x = Standard_GA_300_20[,1],y = Standard_GA_300_20[,2])) +
#  geom_point()

#initialize a data frame that is the size needed
island_one <- data.frame(matrix(ncol = 12, nrow = 300))
island_two <- data.frame(matrix(ncol = 12, nrow = 300))

#have a question about how to remove the first column
#Load all of the experimental data
pop_1_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_1_a.csv")
pop_1_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_1_b.csv")
#extract the fitness value columns
fit_1_a <- pop_1_a[,3,drop = FALSE]
fit_1_b <- pop_1_b[,3,drop = FALSE]
#assign the fitness values into the cumulative data frame
island_one[,1] <- fit_1_a
island_two[,1] <- fit_1_b

pop_2_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_2_a.csv")
pop_2_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_2_b.csv")
fit_2_a <- pop_2_a[,3,drop = FALSE]
fit_2_b <- pop_2_b[,3,drop = FALSE]
island_one[,2] <- fit_2_a
island_two[,2] <- fit_2_b

pop_3_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_3_a.csv")
pop_3_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_3_b.csv")
fit_3_a <- pop_3_a[,3,drop = FALSE]
fit_3_b <- pop_3_b[,3,drop = FALSE]
island_one[,3] <- fit_3_a
island_two[,3] <- fit_3_b


pop_4_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_4_a.csv")
pop_4_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_4_b.csv")
fit_4_a <- pop_4_a[,3,drop = FALSE]
fit_4_b <- pop_4_b[,3,drop = FALSE]
island_one[,4] <- fit_4_a
island_two[,4] <- fit_4_b

pop_5_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_5_a.csv")
pop_5_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_5_b.csv")
fit_5_a <- pop_5_a[,3,drop = FALSE]
fit_5_b <- pop_5_b[,3,drop = FALSE]
island_one[,5] <- fit_5_a
island_two[,5] <- fit_5_b

pop_6_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_6_a.csv")
pop_6_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_6_b.csv")
fit_6_a <- pop_6_a[,3,drop = FALSE]
fit_6_b <- pop_6_b[,3,drop = FALSE]
island_one[,6] <- fit_6_a
island_two[,6] <- fit_6_b

pop_7_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_7_a.csv")
pop_7_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_7_b.csv")
fit_7_a <- pop_7_a[,3,drop = FALSE]
fit_7_b <- pop_7_b[,3,drop = FALSE]
island_one[,7] <- fit_7_a
island_two[,7] <- fit_7_b

pop_8_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_8_a.csv")
pop_8_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_8_b.csv")
fit_8_a <- pop_8_a[,3,drop = FALSE]
fit_8_b <- pop_8_b[,3,drop = FALSE]
island_one[,8] <- fit_8_a
island_two[,8] <- fit_8_b

pop_9_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_9_a.csv")
pop_9_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_9_b.csv")
fit_9_a <- pop_9_a[,3,drop = FALSE]
fit_9_b <- pop_9_b[,3,drop = FALSE]
island_one[,9] <- fit_9_a
island_two[,9] <- fit_9_b

pop_10_a <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_10_a.csv")
pop_10_b <- read.csv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/output_10_b.csv")
fit_10_a <- pop_10_a[,3,drop = FALSE]
fit_10_b <- pop_10_b[,3,drop = FALSE]
island_one[,10] <- fit_10_a
island_two[,10] <- fit_10_b

#extract just the data from the island data frame
stat_a <- island_one[,1:10]
stat_b <- island_two[,1:10]

#calculate the mean and st deviation on the data
island_one[,11] <- rowMeans(stat_a)
island_two[,11] <- rowMeans(stat_b)
island_one[,12] <- rowSds(stat_a)
island_two[,12] <- rowSds(stat_b)


ggplot(data = island_one,mapping = aes(x = pop_1_a[,2], y = island_one[,11])) + 
  geom_point()+
  xlim(0,2000)+
  ylim(0,400)
ggplot(data = island_two,mapping = aes(x = pop_1_a[,2], y = island_two[,11])) + 
  geom_point()+
  xlim(0,2000)+
  ylim(0,400)

