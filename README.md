# CoRC Distributed Island Genetic Algorithm
This repository contains two versions of the Island Genetic Algorithm for Parameter Estimation in COPASI

The standard version utilizes two islands of solutions, with random exchange at each generation. The four island version institutes a random process to select a donating and receiving island to ensure that the same two islands are not exchanging individuals at each generation.

The algorithms were built using CoRC, the R connector for the COPASI software which can be found here:
https://github.com/jpahle/CoRC

In order to run the files, the kinmmfit.cps model will need to be downloaded and stored in the same folder as the scripts for the algorithms
It is essential to update the code to reflect the path to the model on your machine
