#Load the necessary libraries
library(CoRC)
library(tidyverse)
getCopasi(quiet = TRUE)

#load the model of choice
model <- loadSBML("http://www.ebi.ac.uk/biomodels-main/download?mid=BIOMD0000000010")

#get the time series data before estimation
data_before <- runTimeCourse(1000,1)$result

#read the experimental data
data_experimental <-
  read_tsv("MAPKdata.txt") %>%
  rename(Time = time, `Mos-P` = "MAPKKK-P", `Erk2-P` = "MAPK-P")

#define the experiments for COPASI
fit_experiments <- defineExperiments(
  data = data_experimental
  type = c("time","dependent","dependent"),
  mapping = c(NA,"{[Mos-P]}","{[Erk2-P]}"),
  weight_method = "mean_square"
)

#define the parameters for COPASI
fit_parameters <- 