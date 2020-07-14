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
isle_1<- df_1 %>%filter(X2==1)%>% select(X4)
isle_2<- df_1 %>%filter(X2==2)%>% select(X4)

df_1 %>%
  group_by(X1)%>%
  filter(X2 == 1)%>%
  summarise(avg = mean(X4))

df_1 %>%
  group_by(X1)%>%
  filter(X2 == 2)%>%
  summarise(avg = mean(X4))

  
ggplot(data = isle_1,mapping = aes(x = y = isle_1)) + 
  geom_point()+
  xlim(0,2000)+
  ylim(0,400)


