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
  map(
    parameter_strict(regex(c("V1$", "V2$", "V5$", "V6$", "V9$", "V10$"))),
    ~ {
      val <- getParameters(.x)$value
      defineParameterEstimationParameter(parameter(.x, "Value"), start_value = val, lower_bound = val * 0.1, upper_bound = val * 1.9)
    }
  )

result <- 
  runParameterEstimation(
    parameters = fit_parameters,
    experiments = fit_experiments,
    method = list(method = "LevenbergMarquardt",
                  log_verbosity = 2),
    update_model = TRUE
  )

data_after <- runTimeCourse(1000,1)$result

plots <- list(
  `Erk2-P` =
    ggplot(mapping = aes(x = Time, y = `Erk2-P`)) +
    geom_point(data = data_experimental, aes(color = "experimental")) +
    geom_line(data = data_before, aes(color = "before")) +
    geom_line(data = data_after, aes(color = "after")) +
    scale_color_manual(values = c(before = "red", after = "green", experimental = "black")) +
    labs(
      x = paste0("Time (", getTimeUnit(), ")"),
      y = paste0("Erk2-P (", getQuantityUnit(), ")"),
      color = "Series"
    ),
  `Mos-P` =
    ggplot(mapping = aes(x = Time, y = `Mos-P`)) +
    geom_point(data = data_experimental, aes(color = "experimental")) +
    geom_line(data = data_before, aes(color = "before")) +
    geom_line(data = data_after, aes(color = "after")) +
    scale_color_manual(values = c(before = "red", after = "green", experimental = "black")) +
    labs(
      x = paste0("Time (", getTimeUnit(), ")"),
      y = paste0("Mos-P (", getQuantityUnit(), ")"),
      color = "Series"
    )
)

unloadModel()

result$fitted_values
