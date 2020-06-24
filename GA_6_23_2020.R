
### Defining all functions we are going to use 

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
loadModel("C:/Users/aidan/Desktop/HRP/models/KinMMFit.cps")
c_datamodel <- getCurrentModel()

task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
problem <- as(task$getProblem(), "_p_CFitProblem")

task$initialize(TRUE)


method <- as(task$getMethod(), "_p_COptMethodGA")
print(paste("uses method ", method$getObjectName()))


# need to manually do some steps that would have been done by Task::process
problem$randomizeStartValues() # this will only randomize start values if the flag is checked in the model
problem$rememberStartValues()  # this stores the start values, so they can be reset later
problem$resetEvaluations()     # resets the counter of function evaluations
task$output('BEFORE') # this will cause the report settings in header to be 

method$optimise_start() # initialize populations
#This is where I am generating the node stack overflow

print(paste("population size ", method$getPopulationSize()))


print("Initial population")
initial_pop <- get_population(method)
print (initial_pop)
print("with objective values")
print(get_objective_values(method))
#This is where I get a fatal error that aborts the current R session

# do some steps
for (i in 1:10){
  method$optimise_step() # running a step 
} 

# remember indices in copasi are 0 based, in r 1 based: 

first_steps <- get_population(method)
print("obj values")
obj_values <- get_objective_values(method)
print(obj_values)
best_index = method$getBestIndex()
print (paste("best one: ", best_index, 
             " with value ", obj_values[best_index+1], " was: "))
print(first_steps[best_index+1,])


#  replace an indidual (here the first one):
set_nth_individual(method, 0, first_steps[best_index+1])

# do some steps
for (i in 1:10){
  method$optimise_step() # running a step 
} 

# result: 
population <- get_population(method)
print("obj values")
obj_values <- get_objective_values(method)
print(obj_values)
best_index = method$getBestIndex()
print (paste("best one: ", best_index, 
             " with value ", obj_values[best_index+1], " was: "))
print(population[best_index+1,])


# population is no longer available after calling finish
method$optimise_finish();

# once done we might need some extra steps
problem$calculateStatistics() # calculate solution statistic (if option is checked)
task$output("AFTER") # output footer information