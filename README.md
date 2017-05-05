## Code for reproducing simulations and analyses in "Test of Matrix Structure for Construct Validation" by Brian D. Segal, Thomas Braun, Michael Elliott, and Hui Jiang. Submitted.

The accompanying R package is at [https://github.com/bdsegal/matrixTest](https://github.com/bdsegal/matrixTest).

### Contents:

1. `simulations`: Code for reproducing simulations
    1. To run the simulations, submit the scripts beginning with `simulation`: `simulation_white_noise.R`, `simulation_not_full_block.R`, `simulation_constant.R`, `simulation_alternative.R`
    2. To make plots and analyze results, run the scripts beginning with `results`: `results_white_noise.R`, `results_not_full_block.R`, `results_constant.R`, `results_alternative.R`
2. `application`: Code for the analysis with the Health and Retirement Study Big Five Questionnaire. Before running the following scripts, download the 2010 core data from [HRS](http://hrsonline.isr.umich.edu/), and alter the paths at the top of the scripts in this folder to point to the data.
    1. `HRS_plots.R`: Plot correlation matrix
    2. `HRS_analysis.R`: Reproduce all analyses of the HRS data
    3. `HRS_eigen.R`: Plots demonstrating the low rank structure from an eigen-decomposition
