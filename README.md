# *NeuroRehabilitation Case Study*
*Analytical case study examining how to scale up high-dose, high-intensity neurorehabilitation*

### Directories
  * `code` - R scripts required for project
    + `analysis_code.R` - main R analysis script
    + `figure_code.R` - R script for making report figures
    + `functions.R` - functions sourced and used in main script(s)
    + `packages.R` - list of packages sourced and used in main script(s)
  * `data` - data required for project
    + `therapies.csv` - list of therapies considered in analysis
    + `costs.csv` - list of costs associated with each therapy in analysis
    + `nd_counties.csv` - list of counties and population (2020 counts) for North Dakota (for estimating travel costs)
    + `nd_stroke_hospitals.csv` - geolocations for 6 stroke hospitals in North Dakota (for estimating travel costs)
    + `therapies_colours.csv` - plotting colours associated with each therapy
    + `results_group_probabilities.csv` - simulation results: probabilities that an individual in each therapy will improve by some clinically meaningful amount 
    + `results_individual_probabilities.csv` - simulation results: probabilities that a group of patients will improve by some clinically meaningful amount given therapy costs
  * `figures` - figures included in the report

### Files
  * `.gitignore` - tells git what files and folders *not* to track or upload to GitHub
  * `README.md` - this page
  * `r-project.Proj` - R project
  
## How to use

To use this code to re-run the analysis in the analytical case study report, download the repository. The code is saved as an R project, and therefore, opening the R project should provide the complete workspace to re-run analyses and re-create figures.