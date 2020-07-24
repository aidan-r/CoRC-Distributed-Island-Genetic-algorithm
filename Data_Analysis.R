library(tidyverse)
library(matrixStats)

#cannot import with fewer column names
Standard_ga <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems.txt",col_names = FALSE)
Standard_ga_2 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems_2.txt",col_names = FALSE) 
Standard_ga_3 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_100_gens_40_mems_3.txt",col_names = FALSE)

#retrieving the island data
df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-1.tsv")
df_2 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-2.tsv")
df_3 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-100-3.tsv")

#Variable for the number of iterations
iter <- df_1%>%
  select(iteration)%>%
  summarise(max = max(iteration))
iter <- as.double(iter)


#remove the bracket columns from the Standard GA df
remove_brackets <- function(df){
  for(j in 1:dim(df)[2]){
    if(j > dim(df)[2]){
      break}
  
    else if(df[1,j] == "("|df[1,j] == ")"){
      df <- df%>%select(-j)
      print("removing a column")
  }
  }
}

remove_brackets(Standard_ga)
remove_brackets(Standard_ga_2)
remove_brackets(Standard_ga_3)

#add a column of all zeros for mutation
add_zero <- function(df){
df <-df %>%
    mutate(iteration = 0)}

add_zero(Standard_ga)
add_zero(Standard_ga_2)
add_zero(Standard_ga_3)

#Separate the iterations of the Standard Search
iterize_standard <- function(df){
val=1
for(i in 1:dim(df)[1]){
  if(i == 1){
    df[i,dim(df)[2]] <- val
  }
  else if(df[i,2] <= df[i-1,2]){
    df[i,dim(df)[2]] <- val
  }
  else{
    val <- val+1
    df[i,dim(df)[2]] <- val
  }
}}
iterize_standard(Standard_ga)
iterize_standard(Standard_ga_2)
iterize_standard(Standard_ga_3)


#Select the iteration and fitness of the standard search
fitness_standard_1 <- Standard_ga%>%
  select(iteration,X2,X1)
colnames(fitness_standard)<-c("iteration","value","evals")

fitness_standard_2 <- Standard_ga_2%>%
  select(iteration,X2,X1)
colnames(fitness_standard_2)<-c("iteration","value","evals")

fitness_standard_3 <- Standard_ga_3%>%
  select(iteration,X2,X1)
colnames(fitness_standard_3)<-c("iteration","value","evals")

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

converge_vector_standard_2 <-fitness_standard_2%>%
  group_by(iteration)%>%
  summarise(min = min(value))
converge_count_standard_2 <- as.double(colSums(converge_vector_standard_2[,2] < 4))

converge_vector_standard_3 <-fitness_standard_3%>%
  group_by(iteration)%>%
  summarise(min = min(value))
converge_count_standard_3 <- as.double(colSums(converge_vector_standard_3[,2] < 4))

Standard_Convergence_Avg <- mean(converge_count_standard,converge_count_standard_2,converge_count_standard_3)
Standard_Convergence_Stdev <- sd(converge_count_standard,converge_count_standard_2,converge_count_standard_3)


# Standard_ga <- Standard_ga%>%
#   group_by(iteration)%>%
#   mutate(row = row_number())%>%
#   pivot_wider(names_from = iteration, values_from = X2)%>%
#   select(-row)


#separating the objective values of the two islands across ten trials to recreate the COPASI plot
isle_1<- df_1 %>%filter(island==1)%>% select(evals,fitness)
isle_2<- df_1 %>%filter(island==2)%>% select(evals,fitness)

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

#create an x axis vector
axis <- isle_1 %>%
  select(evals)%>%
  filter(evals <= 6040)

#Add the axis vector to the islands
island_1_avg <- island_1_avg%>%
  mutate(axis)
island_2_avg <- island_2_avg%>%
  mutate(axis)  

#count the number of converging trials
converge_count_1 <- rowSums(slice_tail(island_1_avg%>%select(-cv,-avg,-StDev)) < 4)
converge_count_2 <- rowSums(slice_tail(island_2_avg%>%select(-cv,-avg,-StDev)) < 4)

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

#plot progression of both islands together
figure_islands <- ggplot()+
  geom_point(data = isle_1,mapping = aes(x = evals,y=fitness),color = 'blue')+
  geom_point(data = isle_2,mapping = aes(x = evals,y = fitness,color = 'red'))+
  scale_y_log10()
print(figure_islands)

  
#Plot the progress of the standard GA
ggplot(data = Standard_ga,mapping = aes(x = X1,y =X2))+
  geom_point()+
  scale_y_log10()

#trying to alter the standard GA data frame
test <- Standard_ga%>%
  group_by(iteration)%>%
  summarise(min = min(X1),max = max(X1))

row_s <- dim(island_1_avg)[1]
col_s <- iter
standard_test <-data.frame(matrix(nrow = row_s,ncol = col_s +1))
current_eval <- 20
for(i in 1:row_s){
  standard_test[i,1] <- current_eval
  current_eval <- current_eval+20
}
                           
C:\Program Files\copasi.org\COPASI 4.28.226\bin\CopasiSE C:\Users\aidan\Desktop\HRP\CoRC-Distributed-Island-Genetic-algorithm\KinMMFit2.cps


