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

library(CoRC)

c_datamodel_one <- loadModel("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-Algorithm/KinMMFit.cps")
c_datamodel_two <- loadModel("C:/Users/aidan/Desktop/HRP/CoRC-Distributed-Island-Genetic-Algorithm/KinMMFit.cps")

task_one <- as(c_datamodel_one$getTask("Parameter Estimation"), "_p_CFitTask")


task_two <- as(c_datamodel_two$getTask("Parameter Estimation"), "_p_CFitTask")

problem_one <- as(task_one$getProblem(), "_p_CFitProblem")
problem_two <- as(task_two$getProblem(), "_p_CFitProblem")

task_one$initialize(TRUE)
task_two$initialize(TRUE)


method_one <- as(task_one$getMethod(), "_p_COptMethodGA")
print(paste("instance on uses method ", method_one$getObjectName()))

method_two <- as(task_two$getMethod(), "_p_COptMethodGA")
print(paste("instance two uses method ", method_two$getObjectName()))


# need to manually do some steps that would have been done by Task::process
problem_one$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_one$rememberStartValues()  # this stores the start values, so they can be reset later
problem_one$resetEvaluations()     # resets the counter of function evaluations
task_one$output('BEFORE') # this will cause the report settings in header to be 

problem_two$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem_two$rememberStartValues()  # this stores the start values, so they can be reset later
problem_two$resetEvaluations()     # resets the counter of function evaluations
task_two$output('BEFORE') # this will cause the report settings in header to be 

if (method_one$getObjectName() != "Genetic Algorithm")
{
  stop("This code only works with Genetic Algorithm")
}

if (method_two$getObjectName() != "Genetic Algorithm")
{
  stop("This code only works with Genetic Algorithm")
}

method_one$optimise_start() # initialize populations
method_two$optimise_start()

print(paste("population size of instance one ", method_one$getPopulationSize()))
print(paste("generation number of instance one", method_one$getNumGenerations()))

print(paste("population size of instance two ", method_two$getPopulationSize()))
print(paste("generation number of instance two", method_two$getNumGenerations()))

gens <- method_one$getNumGenerations()
steps <- 10


print("Initial population One")
initial_pop_one <- get_population(method_one)
print (initial_pop_one)


print("Initial population Two")
initial_pop_two <- get_population(method_two)
print (initial_pop_two)




exchange <- function(met_a,met_b){
  #initializing populations from method object
  pop_a <- get_population(met_a) 
  pop_b <- get_population(met_b)
  
  #generating the random index
  idx <- sample(1:(dim(pop_a)[1]),1,replace = TRUE) 
  #retrieving the randomly selected individuals
  random_indiv_b <- pop_b[idx,]
  random_indiv_a <- pop_a[idx,]
  
  #actually completing the exchange
  set_nth_individual(met_a,idx-1,random_indiv_b)#replacing a random individual in population one with the best from population 
  #I am pretty sure this is where the "-1" needs to be placed to function correctly
  set_nth_individual(met_b,idx-1,random_indiv_a)#replacing a random individual in population one with the best from population 
  
  
}

#need a column for the gen number, the objective value, and each parameter that was estimated
#start off with 0 rows and successively add at each step
rows <- 0
col <- 2 + dim(initial_pop_one)[2]

output_one <- data.frame(matrix(ncol = col,nrow = rows)) #dataframe for the first pop
output_two <- data.frame(matrix(ncol = col,nrow = rows)) #dataframe for the second pop

#encountering some type of error with the function correctly appending to the dataframe


main_loop <- function(generations,step_size,met_a,met_b,output_a,output_b){
  for(i in 1:generations){
    if(i %% step_size == 0){
      #exchange the values between the two populations
      exchange(met_a,met_b)
      
      #step population 1 and save the data
      met_a$optimise_step()
      obj_values_one <- get_objective_values(met_a)
      population_one <- get_population(met_a)
      best_index_one = met_a$getBestIndex()
      obj_values_one[best_index_one+1]
      population_one[best_index_one+1,]
      output_a <- rbind(output_a,c(best_index_one+1,obj_values_one[best_index_one+1],population_one[best_index_one+1,]))
      
      #step population 2 and save the data
      met_b$optimise_step()
      obj_values_two <- get_objective_values(met_b)
      population_two <- get_population(met_b)
      best_index_two = met_b$getBestIndex()
      obj_values_two[best_index_two+1]
      population_two[best_index_two+1,]
      output_b <- rbind(output_b,c(best_index_two+1,obj_values_two[best_index_two+1],population_two[best_index_two+1,]))
      }
      else{
        met_a$optimise_step()
        obj_values_one <- get_objective_values(met_a)
        population_one <- get_population(met_a)
        best_index_one = met_a$getBestIndex()
        obj_values_one[best_index_one+1]
        population_one[best_index_one+1,]
        output_a <- rbind(output_a,c(best_index_one+1,obj_values_one[best_index_one+1],population_one[best_index_one+1,]))
        
        met_b$optimise_step()
        obj_values_two <- get_objective_values(met_b)
        population_two <- get_population(met_b)
        best_index_two = met_b$getBestIndex()
        obj_values_two[best_index_two+1]
        population_two[best_index_two+1,]
        output_b <- rbind(output_b,c(best_index_two+1,obj_values_two[best_index_two+1],population_two[best_index_two+1,]))
        
    }
  }
}

main_loop(gens,steps,method_one,method_two,output_one,output_two)



# population is no longer available after calling finish
method_one$optimise_finish();
method_two$optimise_finish()
print("output one is")
view(output_one)
print("output two is")
view(output_two)