## Code for reproducing simulations and analyses in Segal, et al. (2019).

The accompanying R package is available on the [CRAN](https://cran.r-project.org/web/packages/matrixStrucTest/index.html} and at [https://github.com/bdsegal/matrixStrucTest](https://github.com/bdsegal/matrixStrucTest).

### Contents:

1. `simulations`: Code for reproducing simulations
    1. To conduct the simulations, run the scripts beginning with `simulation`.
    2. To make plots and analyze results, run the scripts beginning with `results` after running the corresponding simulation scripts.
2. `application`: Code for the analysis with the Health and Retirement Study Big Five Questionnaire. Before running the following scripts, download the 2010 core data from [HRS](http://hrsonline.isr.umich.edu/), and alter the paths at the top of the scripts to point to the data.
    1. `HRS_plots.R`: Plot correlation matrix
    2. `HRS_analysis.R`: Reproduce all analyses of the HRS data
    3. `HRS_eigen.R`: Plots demonstrating the low rank structure from an eigen-decomposition

## References

Segal, B. D., Braun, T., Gonzalez, R., and Elliott, M. R. (2019). Tests of matrix structure for construct validation. Psychometrika, 84(1), 65-83. [doi.org/10.1007/s11336-018-9647-4](https://doi.org/10.1007/s11336-018-9647-4). (open-access [view-only version](https://rdcu.be/bb49z)).
