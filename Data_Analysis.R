library(tidyverse)
library(matrixStats)

#cannot import with fewer column names
Standard_ga <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/GA Performance Data/Standard_GA_10_gens.txt",col_names = FALSE)

#retrieving the island data
df_1 <- read_tsv("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-algorithm/KinMMFit-10.tsv")

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

#Select the iteration and fitness
fitness_standard <- Standard_ga%>%
  select(iteration,X2)
#initialize the usable dataframe
Standard_Avg <- data.frame(matrix(ncol = iter,nrow = 0))
row_count = 1

#Convert the data into a usable data frame for row-wise statistics
for(i in 1:dim(fitness_standard)[1]){
  col <- as.double(fitness_standard[i,1])
  if(i == 1){
    Standard_Avg[row_count,col] <- fitness_standard[row_count,2]
    row_count<- row_count + 1
  }
  else if (fitness_standard[i,2] < fitness_standard[i-1,2]){
    row_count <- row_count + 1
    Standard_Avg[row_count,col] <- fitness_standard[i,2]
  }
  else if (fitness_standard[i,2] > fitness_standard[i-1,2]){
    row_count <- 1
    Standard_Avg[row_count,col] <- fitness_standard[i,2]
  }
}




#separating the objective values of the two islands across ten trials to recreate the COPASI plot
isle_1<- df_1%>%filter(island==1)%>%select(evals,fitness)
isle_2<- df_1 %>%filter(island==2)%>% select(evals,fitness)



#initialize a data frame for comparison of trials
island_1_avg <- data.frame(matrix(ncol = iter,nrow = (dim(isle_1)[1]/iter)))
fitness_1 <- df_1%>%
  filter(island == 1)%>%
  select(iteration,fitness)
row_count = 1

#Transfer the Fitness values of every iteration into a usable DataFrame
for(i in 1:dim(fitness_1)[1]){
  col <- as.double(fitness_1[i,1])
  if(row_count == as.double(dim(fitness_1)[1]/ iter)){
    island_1_avg[row_count,col] <- as.double(fitness_1[i,2])
    row_count = 1
    }
  else{
    island_1_avg[row_count,col] <- as.double(fitness_1[i,2])
    row_count <- row_count + 1}
}

#Add the column for the mean
island_1_avg <-island_1_avg%>%
  mutate(avg = rowMeans(island_1_avg))

#Add the column for the st dev and CV
island_1_avg <- island_1_avg%>%
  mutate(StDev = rowSds(as.matrix(select(island_1_avg,-avg))),
         cv = StDev / avg)

#Repeat for Island 2
island_2_avg <- data.frame(matrix(ncol = iter,nrow = (dim(isle_2)[1]/iter)))
fitness_2 <- df_1%>%
  filter(island == 2)%>%
  select(iteration,fitness)
  row_count = 1
for(i in 1:dim(fitness_2)[1]){
  col <- as.double(fitness_2[i,1])
  if(row_count == as.double(dim(fitness_2)[1]/ iter)){
    island_2_avg[row_count,col] <- as.double(fitness_2[i,2])
    row_count = 1
  }
  else{
    island_2_avg[row_count,col] <- as.double(fitness_2[i,2])
    row_count <- row_count + 1}
}

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


