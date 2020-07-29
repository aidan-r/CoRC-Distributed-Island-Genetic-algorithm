library(tidyverse)
library(matrixStats)

#cannot import with fewer column names
#Standard_ga <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems.txt",col_names = FALSE)
#Standard_ga <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems_2.txt",col_names = FALSE) 
Standard_ga <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems_3.txt",col_names = FALSE)

#retrieving the island data
df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-1.tsv")
#df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-2.tsv")
#df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-3.tsv")

#Variable for the number of iterations
iter <- df_1%>%
  select(iteration)%>%
  summarise(max = max(iteration))
iter <- as.double(iter)


#remove the bracket columns from the Standard GA df

for(j in 1:dim(Standard_ga)[2]){
  if(j > dim(Standard_ga)[2]){
    break}
  
  else if(Standard_ga[1,j] == "("|Standard_ga[1,j] == ")"){
    Standard_ga <- Standard_ga%>%select(-j)
  }
  }




#add a column of all zeros for mutation

Standard_ga <-Standard_ga %>%
    mutate(iteration = 0)


#Separate the iterations of the Standard Search

val=1
for(i in 1:dim(Standard_ga)[1]){
  if(i == 1){
    Standard_ga[i,dim(Standard_ga)[2]] <- val
  }
  else if(Standard_ga[i,2] <= Standard_ga[i-1,2]){
    Standard_ga[i,dim(Standard_ga)[2]] <- val
  }
  else{
    val <- val+1
    Standard_ga[i,dim(Standard_ga)[2]] <- val
  }
}


#Select the iteration and fitness of the standard search
fitness_standard <- Standard_ga%>%
  select(iteration,X2,X1)
colnames(fitness_standard)<-c("iteration","value","evals")


#initialize the usable dataframe
Standard_Avg <- data.frame(matrix(ncol = iter,nrow = 0))


row_count = 1

#Convert the standard search data into a usable data frame for row-wise statistics
for(i in 1:dim(fitness_standard)[1]){
  col <- as.double(fitness_standard[i,1])
  if(i == 1){
    Standard_Avg[row_count,col] <- fitness_standard[row_count,2]
    row_count<- row_count + 1
  }
  else if (fitness_standard[i,2] < fitness_standard[i-1,2]){
    Standard_Avg[row_count,col] <- fitness_standard[i,2]
    row_count <- row_count + 1
  }
  else if (fitness_standard[i,2] > fitness_standard[i-1,2]){
    row_count <- 1
    Standard_Avg[row_count,col] <- fitness_standard[i,2]
  }
}



#calculate the number of convergences in the standard ga
converge_vector_standard_1 <-fitness_standard%>%
  group_by(iteration)%>%
  summarise(min = min(value))
converge_count_standard <- as.double(colSums(converge_vector_standard_1[,2] < 4))

bar_matrix <- data.frame(matrix(nrow = 3,ncol = 2))
colnames(bar_matrix) <- c("Standard","Island")
bar_matrix[1,1] <- 88
bar_matrix[2,1] <- 86
bar_matrix[3,1] <- 91



Standard_Convergence_Avg <- mean(converge_count_standard,converge_count_standard_2,converge_count_standard_3)
Standard_Convergence_Stdev <- sd(converge_count_standard,converge_count_standard_2,converge_count_standard_3)


# Standard_ga <- Standard_ga%>%
#   group_by(iteration)%>%
#   mutate(row = row_number())%>%
#   pivot_wider(names_from = iteration, values_from = X2)%>%
#   select(-row)


#separating the objective values of the two islands across ten trials to recreate the COPASI plot
isle_1<- df_1 %>%filter(island==1)%>%select(evals,fitness)
isle_2<- df_1 %>%filter(island==2)%>%select(evals,fitness)

best_from_islands <-df_1 %>%
  filter(island==1)%>% 
  select(evals,iteration,fitness)%>%
  mutate(second = df_1%>%filter(island==2)%>%select(fitness))

best_from_islands <- best_from_islands %>%mutate(best = rowMins(as.matrix(best_from_islands%>%select(-evals,-iteration))))

#organize the data into columns by iteration number
island_1_avg <- df_1%>%filter(island ==1)%>%select(iteration,fitness)
island_1_avg <- island_1_avg %>%
  group_by(iteration) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = iteration, values_from = fitness) %>%
  select(-row)
island_1_avg <-island_1_avg%>%
  mutate(avg = rowMeans(island_1_avg))

#Add the column for the st dev and CV
island_1_avg <- island_1_avg%>%
  mutate(StDev = rowSds(as.matrix(select(island_1_avg,-avg))),
         cv = StDev / avg)

#Repeat for Island 2
island_2_avg <- df_1%>%filter(island == 2)%>%select(iteration,fitness)
island_2_avg <- island_2_avg%>%
  group_by(iteration)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = iteration,values_from = fitness)%>%
  select(-row)

island_2_avg <-island_2_avg%>%
  mutate(avg = rowMeans(island_2_avg))

island_2_avg <- island_2_avg%>%
  mutate(StDev = rowSds(as.matrix(select(island_2_avg,-avg))),
         cv = StDev / avg)



best_avg <- best_from_islands %>%
  select(iteration,best)%>%
  group_by(iteration) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = iteration, values_from = best) %>%
  select(-row)
best_avg <-best_avg%>%
  mutate(avg = rowMeans(best_avg))
best_avg <- best_avg%>%
  mutate(StDev = rowSds(as.matrix(select(best_avg,-avg))),
         cv = StDev / avg)



#create an x axis vector
axis <- isle_1 %>%
  select(evals)%>%
  filter(evals <= 6040)

#Add the axis vector to the islands
island_1_avg <- island_1_avg%>%
  mutate(axis)
island_2_avg <- island_2_avg%>%
  mutate(axis)
best_avg <- best_avg%>%
  mutate(axis)

#count the number of converging trials
converge_count_1 <- rowSums(slice_tail(island_1_avg%>%select(-cv,-avg,-StDev)) < 4)
converge_count_2 <- rowSums(slice_tail(island_2_avg%>%select(-cv,-avg,-StDev)) < 4)

bar_matrix[1,2] <- converge_count_1
bar_matrix[2,2] <- 98
bar_matrix[3,2] <- 97
#calculate convergence for the standard GA


#create the graph for island one from COPASI  
ggplot(data = isle_1,mapping = aes(x = evals,y = fitness)) + 
  geom_point()+
  scale_y_log10()

#create the graph for island two from COPASI  
ggplot(data = isle_2,mapping = aes(x = evals,y = fitness)) + 
  geom_point()+
  scale_y_log10()

#plot the average progression from island 1 and 2 together
figure_avgs <- ggplot()+
  geom_point(data = island_1_avg,mapping = aes(x = evals,y = avg),color = 'blue')+
  geom_point(data = island_2_avg,mapping = aes(x = evals,y = avg),color = 'red')+
  scale_y_log10()

print(figure_avgs)

figure_best_avg <- ggplot()+
  geom_point(data = best_avg,mapping = aes(x = evals,y = avg),color = 'green')+
  scale_y_log10()
print(figure_best_avg)

#plot progression of both islands together
figure_islands <- ggplot()+
  geom_point(data = isle_1,mapping = aes(x = evals,y=fitness),color = 'blue')+
  geom_point(data = isle_2,mapping = aes(x = evals,y = fitness,color = 'red'))+
  scale_y_log10()
print(figure_islands)

figure_best_progress <- ggplot()+
  geom_point(data = best_from_islands,mapping = aes(x = evals,y=best),color = 'green')+
  scale_y_log10()
print(figure_best_progress)

  
#Plot the progress of the standard GA
ggplot(data = Standard_ga,mapping = aes(x = X1,y =X2))+
  geom_point()+
  scale_y_log10()

#trying to alter the standard GA data frame
bar_matrix <- t(bar_matrix)
bar_matrix%>%mutate(avg = rowMeans(bar_matrix),
                    StDev = rowSds(bar_matrix))
                           
#C:\Program Files\copasi.org\COPASI 4.28.226\bin\CopasiSE C:\Users\aidan\Desktop\HRP\CoRC-Distributed-Island-Genetic-algorithm\KinMMFit2.cps


bar_matrix <- data.frame(matrix(ncol = 2,nrow = 6))
bar_matrix[,1] <- c("Standard","Standard","Standard","Island","Island","Island")
bar_matrix[,2] <- c(88,86,91,95,98,97)
colnames(bar_matrix) <-c("Version","Convergence_Count")

ggplot(data = bar_matrix,mapping = aes(x = Version,y = Convergence_Count,fill = Version)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="bottom")