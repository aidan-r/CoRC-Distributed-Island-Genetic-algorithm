#This GA is Utilizing 4 islands

library(tidyverse)
### Defining all functions we are going to use 
##mGenerations

# converts a copasi vector to c()
vec_to_column <- function(vec)
{
  tmp <- c()
  for (j in 0:(vec$size()-1))
  {
    tmp <- c(tmp, vec$get(j))
  }
  return(tmp)
}

# converts a copasi vector of cvectors to a matrix
vec_to_matrix <- function(c_population)
{
  
  tmp <- c()
  for (i in 0:(c_population$size()-1))
  {
    current <- c_population$"__getitem__"(i)
    tmp <- c(tmp, vec_to_column(current))
  }
  result <- matrix(tmp, nrow=c_population$size(), byrow=TRUE)
  return(result)
}

# returns the population as matrix 
get_population <- function(method)
{
  c_population <- method$getPopulation()
  population <- vec_to_matrix(c_population)
  return (population)
}

# returns objective values
get_objective_values <- function(method)
{
  vec <- method$getObjectiveValues();
  result <- vec_to_column(vec)
  return (result)
}

# hacks start here ... creating a new vector in R proved highly
# problematic ... but i figured out a way by going bare metal
# creating a vector
new_vec = function()
{
  # warning ... bare pointer operations ... might crash runtime
  # needs checking for null ptr, input types, ... 
  res <-  .Call('R_swig_new_FloatStdVector__SWIG_0', PACKAGE='COPASI');
  return(res)
  
  # todo add a delete method that frees the memory
  
}

# getting its size
vec_size<-function(vec)
{
  # warning ... bare pointer operations ... might crash runtime
  # needs checking for null ptr, input types, ... 
  if (inherits(vec, "ExternalReference")) vec <- vec@ref
  res <- .Call('R_swig_FloatStdVector_size', vec, FALSE, PACKAGE = 'COPASI')
  return(res)
}

# adding to it
vec_append<-function(vec, x)
{
  # warning ... bare pointer operations ... might crash runtime
  # needs checking for null ptr, input types, ... 
  # do more testing like so 
  if (inherits(vec, "ExternalReference")) vec <- vec@ref
  .Call('R_swig_FloatStdVector_append', vec, x, PACKAGE='COPASI');
}

# need to go bare metal for setting the data as well, as the 
# swig generated one did not work with our vectors above
method_set_nth_population <-function(method, index, data)
{
  # warning ... bare pointer operations ... might crash runtime
  # do more testing like so 
  if (inherits(method, "ExternalReference")) method <- method@ref
  
  index = as.integer(index);
  
  if(length(index) > 1) {
    warning("using only the first element of index");
  };
  
  ;.Call('R_swig_COptPopulationMethod_setNthPopulation', method, index, data, FALSE, PACKAGE='COPASI');
}

# this converts a R c() into one of the vectors we need for copasi
column_to_vec <- function(col)
{
  # this function just allocates a new vector, and fills it with 
  # the elements from col
  vec <- new_vec()
  for (item in col){
    vec_append(vec,item)
  }
  return(vec)
}

# finally we can set the nth individual 
set_nth_individual <- function(method, index, ind)
{
  # warning the c function we are calling is 0 based
  # the vector we are taking is 1 based. So ensure that the
  # index given is 0 based!
  #
  # i.e set_nth_individual(method, 9, population[1,]) will 
  # set the 10th element of the method (with the first 
  # individual from get_population ()
  #
  vec <- column_to_vec(ind)
  method_set_nth_population(method, index, vec)
}

#COMMENT: I think this function should be declared outside the loop... 
exchange <- function(met_a,met_b,met_c,met_d){
  #initializing populations from method object
  pop_a <- get_population(met_a) 
  pop_b <- get_population(met_b)
  pop_c <- get_population(met_c)
  pop_d <- get_population(met_d)
  
  #generating the random index
  idx <- sample(1:(dim(pop_a)[1]),1,replace = TRUE) 
  #retrieving the randomly selected individuals
  random_indiv_b <- pop_b[idx,]
  random_indiv_a <- pop_a[idx,]
  random_indiv_c <- pop_c[idx,]
  random_indiv_d <- pop_d[idx,]
  
  #actually completing the exchange
  set_nth_individual(met_a,idx-1,random_indiv_b)#b goes to a 
  set_nth_individual(met_b,idx-1,random_indiv_c)#c goes to b
  set_nth_individual(met_c,idx-1,random_indiv_d)#d goes to c
  set_nth_individual(met_d,idx-1,random_indiv_a)#a goes to d
  
}


library(CoRC)

# we create a base name, so that all filenames can be derived from it
basename <- "C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-Algorithm/KinMMFit_4_islands"

# filename of model file to load; this just appends ".cps" to the basename
modelfile <- sprintf("%s.cps",basename)

c_datamodel_one <- loadModel(modelfile)
c_datamodel_two <- loadModel(modelfile)
c_datamodel_three <- loadModel(modelfile)
c_datamodel_four <- loadModel(modelfile)

task_one <- as(c_datamodel_one$getTask("Parameter Estimation"), "_p_CFitTask")

task_two <- as(c_datamodel_two$getTask("Parameter Estimation"), "_p_CFitTask")

task_three <- as(c_datamodel_three$getTask("Parameter Estimation"), "_p_CFitTask")

task_four <- as(c_datamodel_four$getTask("Parameter Estimation"), "_p_CFitTask")

problem_one <- as(task_one$getProblem(), "_p_CFitProblem")
problem_two <- as(task_two$getProblem(), "_p_CFitProblem")
problem_three <- as(task_three$getProblem(),"_p_CFitProblem")
problem_four <- as(task_four$getProblem(),"_p_CFitProblem")

task_one$initialize(TRUE)
task_two$initialize(TRUE)
task_three$initialize(TRUE)
task_four$initialize(TRUE)

method_one <- as(task_one$getMethod(), "_p_COptMethodGA")
print(paste("instance on uses method ", method_one$getObjectName()))

method_two <- as(task_two$getMethod(), "_p_COptMethodGA")
print(paste("instance two uses method ", method_two$getObjectName()))

method_three <- as(task_three$getMethod(), "_p_COptMethodGA")
print(paste("instance three uses method ", method_three$getObjectName()))

method_four <- as(task_four$getMethod(), "_p_COptMethodGA")
print(paste("instance four uses method ", method_four$getObjectName()))


# need to manually do some steps that would have been done by Task::process
problem_one$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_one$rememberStartValues()  # this stores the start values, so they can be reset later
problem_one$resetEvaluations()     # resets the counter of function evaluations
task_one$output('BEFORE') # this will cause the report settings in header to be 

problem_two$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_two$rememberStartValues()  # this stores the start values, so they can be reset later
problem_two$resetEvaluations()     # resets the counter of function evaluations
task_two$output('BEFORE') # this will cause the report settings in header to be 

problem_three$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_three$rememberStartValues()  # this stores the start values, so they can be reset later
problem_three$resetEvaluations()     # resets the counter of function evaluations
task_three$output('BEFORE') # this will cause the report settings in header to be 

problem_four$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_four$rememberStartValues()  # this stores the start values, so they can be reset later
problem_four$resetEvaluations()     # resets the counter of function evaluations
task_four$output('BEFORE') # this will cause the report settings in header to be 

if (method_one$getObjectName() != "Genetic Algorithm")
{
  stop("This code only works with Genetic Algorithm")
}

if (method_two$getObjectName() != "Genetic Algorithm")
{
  stop("This code only works with Genetic Algorithm")
}

#extract the number of generations
method_one$optimise_start() 
generations <- method_one$getNumGenerations()
initial_pop_one <- get_population(method_one)
col <- 5 + dim(initial_pop_one)[2]
method_one$optimise_finish();
# repeats is the number of times we want to repeat the whole process
# later, this could be read from the command line instead of being hard-coded
repeats <- 1

#need a column for the iteration, island, gen number, the objective value, and each parameter that was estimated
#start off with 0 rows and successively add at each step
# create a data frame for all the results
output <- data.frame(matrix(ncol = col,nrow = generations*repeats*4)) #dataframe, adjusted to fit four islands



# this is the outer loop that will repeat the parameter estimation many times
for(r in 1:repeats){  
  
  method_one$optimise_start() # initialize populations
  method_two$optimise_start()
  method_three$optimise_start()
  method_four$optimise_start()
  
  print(paste("population size of instance one ", method_one$getPopulationSize()))
  print(paste("generation number of instance one", method_one$getNumGenerations()))
  
  print(paste("population size of instance two ", method_two$getPopulationSize()))
  print(paste("generation number of instance two", method_two$getNumGenerations()))
  
  print(paste("population size of instance three ", method_three$getPopulationSize()))
  print(paste("generation number of instance three", method_three$getNumGenerations()))
  
  print(paste("population size of instance four ", method_four$getPopulationSize()))
  print(paste("generation number of instance four", method_four$getNumGenerations()))
  
  #retrieve the correct number of generations for the loop to iterate through from the COPASI file
  generations <- method_one$getNumGenerations()
  #set the number of steps before exchanging population individuals
  steps <- 10
  
  print("Initial population One")
  initial_pop_one <- get_population(method_one)
  print (initial_pop_one)
  
  print("Initial population Two")
  initial_pop_two <- get_population(method_two)
  print (initial_pop_two)
  
  print("Initial population Three")
  initial_pop_three <- get_population(method_three)
  print (initial_pop_three)
  
  print("Initial population Four")
  initial_pop_four <- get_population(method_four)
  print (initial_pop_four)
  
  
  #main loop that will run both populations through the ga while exchanging random values
  for(i in 1:generations){
    row_n <- (r-1)*4*generations + (4*i)
    if(i %% steps == 0){
      #exchange the values between the two populations
      exchange(method_one,method_two,method_three,method_four)
      
      #step population 1 and save the data
      method_one$optimise_step()
      obj_values_one <- get_objective_values(method_one) #retrieve the objective values
      population_one <- get_population(method_one) #retrieve the current population
      best_index_one <- method_one$getBestIndex() #retrieve the index of the best member of the population
      obj_values_one[best_index_one+1]
      population_one[best_index_one+1,]
      fun_calls_1 <- problem_one$getFunctionEvaluations()
      output[row_n-3,] <- c(r,1,i,fun_calls_1,obj_values_one[best_index_one+1],population_one[best_index_one+1,]) #assign the current row of the data frame to the best value
      
      #step population 2 and save the data
      method_two$optimise_step()
      obj_values_two <- get_objective_values(method_two)
      population_two <- get_population(method_two)
      best_index_two = method_two$getBestIndex()
      obj_values_two[best_index_two+1]
      population_two[best_index_two+1,]
      fun_calls_2 <- problem_two$getFunctionEvaluations()
      output[row_n-2,] <- c(r,2,i,fun_calls_2,obj_values_two[best_index_two+1],population_two[best_index_two+1,])
      
      method_three$optimise_step()
      obj_values_three <- get_objective_values(method_three) #retrieve the objective values
      population_three <- get_population(method_three) #retrieve the current population
      best_index_three <- method_three$getBestIndex() #retrieve the index of the best member of the population
      obj_values_one[best_index_three+1]
      population_one[best_index_three+1,]
      fun_calls_3 <- problem_three$getFunctionEvaluations()
      output[row_n-1,] <- c(r,3,i,fun_calls_3,obj_values_three[best_index_three+1],population_three[best_index_three+1,])
      
      method_four$optimise_step()
      obj_values_four <- get_objective_values(method_four) #retrieve the objective values
      population_four <- get_population(method_four) #retrieve the current population
      best_index_four <- method_four$getBestIndex() #retrieve the index of the best member of the population
      obj_values_four[best_index_four+1]
      population_four[best_index_four+1,]
      fun_calls_4 <- problem_four$getFunctionEvaluations()
      output[row_n,] <- c(r,4,i,fun_calls_4,obj_values_four[best_index_four+1],population_four[best_index_four+1,])
    }
    else{
      method_one$optimise_step()
      obj_values_one <- get_objective_values(method_one)
      population_one <- get_population(method_one)
      best_index_one = method_one$getBestIndex()
      obj_values_one[best_index_one+1]
      population_one[best_index_one+1,]
      fun_calls_1 <- problem_one$getFunctionEvaluations()
      output[row_n-3,] <- c(r,1,i,fun_calls_1,obj_values_one[best_index_one+1],population_one[best_index_one+1,])
      
      method_two$optimise_step()
      obj_values_two <- get_objective_values(method_two)
      population_two <- get_population(method_two)
      best_index_two = method_two$getBestIndex()
      obj_values_two[best_index_two+1]
      population_two[best_index_two+1,]
      fun_calls_2 <- problem_two$getFunctionEvaluations()
      output[row_n-2,] <- c(r,2,i,fun_calls_2,obj_values_two[best_index_two+1],population_two[best_index_two+1,])
      
      method_three$optimise_step()
      obj_values_three <- get_objective_values(method_three) #retrieve the objective values
      population_three <- get_population(method_three) #retrieve the current population
      best_index_three <- method_three$getBestIndex() #retrieve the index of the best member of the population
      obj_values_one[best_index_three+1]
      population_one[best_index_three+1,]
      fun_calls_3 <- problem_three$getFunctionEvaluations()
      output[row_n-1,] <- c(r,3,i,fun_calls_3,obj_values_three[best_index_three+1],population_three[best_index_three+1,])
      
      method_four$optimise_step()
      obj_values_four <- get_objective_values(method_four) #retrieve the objective values
      population_four <- get_population(method_four) #retrieve the current population
      best_index_four <- method_four$getBestIndex() #retrieve the index of the best member of the population
      obj_values_four[best_index_four+1]
      population_four[best_index_four+1,]
      fun_calls_4 <- problem_four$getFunctionEvaluations()
      output[row_n,] <- c(r,4,i,fun_calls_4,obj_values_four[best_index_four+1],population_four[best_index_four+1,])
    }
  }
  
  # population is no longer available after calling finish
  method_one$optimise_finish();
  method_two$optimise_finish()
  method_three$optimise_finish()
  method_four$optimise_finish()
  colnames(output) <- c("iteration","island","generation","evals","fitness","param 1","param 2","param 3","param 4","param 5","param 6","param 7")
  view(output)
} # end of the outer loop

# now we need to save the entire data frame!
version <-1
outfilename <- sprintf("%s-%d-%d.tsv",basename,repeats,version)
write_tsv(x = output, path = outfilename, col_names = TRUE)
