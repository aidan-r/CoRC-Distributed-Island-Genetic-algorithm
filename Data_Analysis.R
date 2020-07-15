library(tidyverse)

#cannot import with fewer column names
Standard_ga <- read.tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_300_20.txt")

#have to import data manually
#wondering how to set when the value gets recorded in COPASI


#Remove the parentheses columns from the data frame
#Standard_GA_300_20 = subset(Standard_GA_300_20, select = -c(V3,V11))

ggplot(data = Standard_GA_300_20,mapping = aes(x = Standard_GA_300_20[,1],y = Standard_GA_300_20[,2])) +
  geom_point()

#retrieving the data from the island script
df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-10.tsv")


#separating the objective values of the two islands across ten trials to recreate the COPASI plot
#need to be able to plot which currently cannot
isle_1<- df_1%>%filter(island==1)%>%select(evals,fitness)
isle_2<- df_1 %>%filter(island==2)%>% select(evals,fitness)



df_1 %>%
  group_by(X1)%>%
  filter(X2 == 1)%>%
  summarise(avg = mean(X4))

df_1 %>%
  group_by(X1)%>%
  filter(X2 == 2)%>%
  summarise(avg = mean(X4))

#creates a new df where the columns represent the fitness values of a specific iteration, calculates the mean for plotting
island_1_avg <- df_1 %>% 
  filter(island ==1,iteration == 1)%>%
  select(evals,fitness)%>%
  mutate(iteration_2 = df_1%>%filter(island ==1,iteration == 2)%>%select(fitness),
         iteration_3 = df_1%>%filter(island ==1,iteration == 3)%>%select(fitness),
         iteration_4 = df_1%>%filter(island ==1,iteration == 4)%>%select(fitness),
         iteration_5 = df_1%>%filter(island ==1,iteration == 5)%>%select(fitness),
         iteration_6 = df_1%>%filter(island ==1,iteration == 6)%>%select(fitness),
         iteration_7 = df_1%>%filter(island ==1,iteration == 7)%>%select(fitness),
         iteration_8 = df_1%>%filter(island ==1,iteration == 8)%>%select(fitness),
         iteration_9 = df_1%>%filter(island ==1,iteration == 9)%>%select(fitness),
         iteration_10 = df_1%>%filter(island ==1,iteration == 10)%>%select(fitness))
island_1_avg <- island_1_avg%>%mutate(mean = rowMeans(island_1_avg%>%select(!evals)))

island_2_avg <- df_1 %>% 
  filter(island ==2,iteration == 1)%>%
  select(evals,fitness)%>%
  mutate(iteration_2 = df_1%>%filter(island ==2,iteration == 2)%>%select(fitness),
         iteration_3 = df_1%>%filter(island ==2,iteration == 3)%>%select(fitness),
         iteration_4 = df_1%>%filter(island ==2,iteration == 4)%>%select(fitness),
         iteration_5 = df_1%>%filter(island ==2,iteration == 5)%>%select(fitness),
         iteration_6 = df_1%>%filter(island ==2,iteration == 6)%>%select(fitness),
         iteration_7 = df_1%>%filter(island ==2,iteration == 7)%>%select(fitness),
         iteration_8 = df_1%>%filter(island ==2,iteration == 8)%>%select(fitness),
         iteration_9 = df_1%>%filter(island ==2,iteration == 9)%>%select(fitness),
         iteration_10 = df_1%>%filter(island ==2,iteration == 10)%>%select(fitness))
island_2_avg <- island_2_avg%>%mutate(mean = rowMeans(island_2_avg%>%select(!evals)))




#create the graph for island one from COPASI  
ggplot(data = isle_1,mapping = aes(x = evals,y = fitness)) + 
  geom_point()+
  scale_y_log10()

#create the graph for island two from COPASI  
ggplot(data = isle_2,mapping = aes(x = evals,y = fitness)) + 
  geom_point()+
  scale_y_log10()

#plot the average progression from island 1
ggplot(data = island_1_avg,mapping = aes(x = evals,y=mean))+
  geom_point()+
  scale_y_log10()

#plot the average progression from island 2
ggplot(data=island_2_avg,mapping = aes(x = evals,y=mean))+
  geom_point()+
  scale_y_log10()


