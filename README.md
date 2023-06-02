# *NeuroRehabilitation Case Study*
*Analytical case study examining how to scale up high-dose, high-intensity neurorehabilitation*

This case study examines the potential for high-dose, high-intensity rehabilitation therapies to address the growing burden of stroke. In this exercise, I examined 7 different rehabilitation therapies based on their prescribed therapy hours, therapist-to-patient ratio, location of therapy (in-clinic or remote), and clinical outcomes measured by the Fugl-Meyer Upper Extremity (FM-UE) assessment. 

therapy_id | paper | location | patients | $\Delta$ FM-UE (sd) | hours | sessions
--- | --- | --- | --- | --- | --- | ---
Dodakian_2017 | [2017](https://doi.org/10.1177/1545968317733818) | remote | 12 | 4.80 (3.80) | 56 | 28 
Cramer_TR_2019 | [2019](https://doi.org/10.1001/jamaneurol.2019.1604) | remote | 62 | 7.86 (6.68) | 42 | 36 
Cramer_TR_2021 | [2021](https://doi.org/10.3389/fneur.2020.611453) | remote | 12 | 6.75 (2.89) | 72 | 72 
Cramer_IC_2019 | [2019](https://doi.org/10.1001/jamaneurol.2019.1604) | hybrid | 60 | 8.36 (7.04) | 42 | 36 
McCabe_2015 | [2015](https://doi.org/10.1016/j.apmr.2014.10.022) | in-clinic | 12 | 7.78 (1.89) | 300 | 60
Daly_2019 | [2019](https://doi.org/10.1177/1545968319846120) | in-clinic | 31 | 9.40 (7.12) | 300 | 60
Ward_2019 | [2019](http://dx.doi.org/10.1136/jnnp-2018-319954) | in-clinic | 224 | 9.00 (1.33) | 90 | 15

### Analysis (in brief)

The FM-UE scores were used as the primary outcome measure. As a first step, I estimated the proportion of therapy success for individual patients by: 1) simulating hypothetical patients for each therapy based on the reported FM-UE scores; and 2) assessing the proportion of patients achieving a clinically significant improvement. This was done 100 times for each therapy, and the results were averaged across simulation batches per each therapy.

Assuming the estimated proportions (which are empirical) are good estimates of the probabilities of achieving a range of score changes on the FM-UE per therapy, one can then estimate the proportion of individuals in a larger group of potential patients that would achieve successful outcomes. Therefore, to consider how each neurorehabilitation can be scaled-up in a cost-effective manner, I estimated the proportion of patients that would achieve successful outcomes (a given change in FM-UE for groups of varying sizes) given a fixed budget. This required monetary cost estimates for each therapy, for which I considered labor, equipment, and travel expenses. 

Results and interpretations can be found in the report: `arbuckle_report.pdf`

***

### Directories
  * `code` - R scripts required for project
    + `analysis_code.R` - main R analysis script
    + `geospatial_cost_code.R` - supplementary R script to estimate average roundtrip travel costs (in North Dakota)
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
  * `arbuckle_report.pdf` - analytical case study report
  
***  
  
### How to use

To use this code to re-run the analysis in the analytical case study report, download the repository. The code is saved as an R project, and therefore, opening the R project should provide the complete workspace to re-run analyses and re-create figures.
  * `analysis_code.R` - will re-do the primarly analyses
  * `figure_code.R` - will make the figures



