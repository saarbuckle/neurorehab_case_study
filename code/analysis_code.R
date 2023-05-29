##########################################################
# Project: NeuroRehab Case Study 
# This file: analysis cases to for report
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

#### Add libraries and functions ####
# add necessary packages:
source("code/packages.R")
# add some functions:
source("code/functions.R")

#### Load needed data ####
df_params    <- set_analysis_params()
df_therapies <- read.csv(here("data/therapies.csv")) # the potential therapies
df_costs     <- read.csv(here("data/costs.csv")) # estimated costs for each therapy

#### PART 1: linear regressions ####
# Run several simple models to see if # therapy hours, the therapist-to-patient
# ratio, of remoteness of therapies are systematically related to clinical
# improvements (operationalized as the change in the fugl-meyer (fm) score)
# linear models:
lm_therapy_hours <- lm(fm_change_mean ~ therapy_hours, df_therapies)
lm_ratio         <- lm(fm_change_mean ~ therapist_patient_ratio, df_therapies)
lm_remoteness    <- lm(fm_change_mean ~ proportion_treatment_remote, df_therapies)


#### PART 2: probability of clinical improvement for an individual patient  ####
# Here we estimate the probablity of clinical improvement for a patient under 
# each of the therapies.
# To do this, we simulate patient data for each potential therapy.
# Each therapy is simulated a number of times (default is 100). 
# To simulate individual patient data for each therapy, we draw values 
# (patient change in the FM score)from a normal distribution with the same mean 
# and standard deviation as the change in the Fugl-Meyer score before and after 
# (usually measured at follow up) completing the therapy, as reported in each paper.
# For each therapy, we simulate data for the same number of patients that completed the therapy. 
# Using each simulated dataset, we compute the proportion of "successful" therapies
# under varying thresholds of the Fugl-Meyer (fm) (i.e., how many patients would met the 
# minimally-clinical important difference threshold?).

set.seed(df_params$rand_seed) # for reproducability
df_sim <- data.frame(mcid_thres = numeric(),
                     therapy_id = character(),
                     prop_success = numeric())
for (sim_n in seq(1, df_params$num_sim, 1)) { # each simulation
  for (ii in 1:nrow(df_therapies)) { # loop through each of the therapies
    therapy <- df_therapies[ii, ] # get the info for this therapy
    # simulate patients' changes in fm as result of this specific therapy:
    fm_change <- rnorm(n = therapy$num_patients, mean = therapy$fm_change_mean, sd = therapy$fm_change_sd)
    # compute proportion of therapy "successes" for for each level of the Fugl-Meyer
    for (mcid in seq(df_params$mcid_thres_start, df_params$mcid_thres_end, df_params$mcid_thres_step)) {
      df_sim <- rbind(df_sim,
                      data.frame(mcid_thres = mcid,
                                 therapy_id = therapy$therapy_id,
                                 prop_success = sum(fm_change > mcid) / therapy$num_patients
                      )
      ) # rbind
    } # for mcid
  } # for therapy_id
} # for each simulation

# summarize simulations into mean and CIs per therapy option:
df_sim <- df_sim %>% 
  group_by(therapy_id, mcid_thres) %>% 
  summarize(mean_prop_success = mean(prop_success), 
            lower_bound = quantile(prop_success, 0.025), 
            upper_bound = quantile(prop_success, 0.975)
  )

# save results:
write.csv(df_sim, "data/results_individual_probabilities.csv", row.names=FALSE)


#### PART 3: perform % treated analysis ####
# Here, we use the estimated therapy success rates for each therapy to estimate, 
# with a given budget (that covers labour, travel, and equipment), how many patients 
# can be treated.
#
# To understand how each therapy deals with more patients, we vary the # of patients
# while holding the overall budget constant. We expect that, as more patients need
# treatment, the proportion of total patients who can be successfully treated
# (i.e., meet some FM threshold after treatment) will decrease. What we are intersted in
# is how the % of successfully treated patients decrease with respect to other therapies. 
#
# We are hoping to find which therapy is the best choice given some knowledge of 
# how many patients to treat and the budget available.
df_treatment <- data.frame(therapy_id = character(), # what therapy is this?
                           budget = numeric(), # what is total therapy budget available?
                           mcid_thres = numeric(), # what is cutoff for therapy success?
                           num_patients_total = numeric(), # how many patients?
                           num_patients_treated = numeric(), # how many patients can be treated?
                           num_patients_success = numeric(), # how many patients were successfully treated?
                           therapy_cost_total = numeric(),   # cost of all treated patients?
                           prop_patients_success = numeric() # proportion of num_patients_total that are successfully treated
)
for (num_patients in seq(100, df_params$max_patients, 100)){ # for groups of varying patient sizes
  for (therapy_id in unique(df_costs$therapy_id)){ # loop through available therapies
    # get info about how much this therapy costs
    therapy_cost         <- sum(df_costs$cost_usd_2023[df_costs$therapy_id==therapy_id], na.rm=TRUE)
    # what is this therapy's estimated success rate (at each fm mcid threshold)
    therapy_sucess_rate  <- df_sim$mean_prop_success[df_sim$therapy_id==therapy_id]
    num_mcid_thres       <- length(therapy_sucess_rate)
    # what is the max # of patients we can treat with this therapy given its costs?
    max_patients_covered <- floor(df_params$budget / therapy_cost)
    # can all patients can be covered given the budget?
    if (max_patients_covered >= num_patients){ 
      num_patients_treated <- rep(num_patients, num_mcid_thres) 
    } else { # if not all patients can be covered, find out how many can be
      num_patients_treated <- rep(max_patients_covered, num_mcid_thres) # how many patients were treated?
    }
    # calculate remaining fields:
    therapy_cost_total    <- num_patients_treated * therapy_cost # how much did it cost to treat these patients?
    num_patients_success  <- floor(num_patients_treated * therapy_sucess_rate) # how many treated patients are "successful"?
    prop_patients_success <- num_patients_success / num_patients # convert # successful patients to a proportion of ALL patients (not just those treated, if we cannot afford to treat all)
    num_patients_total    <- rep(num_patients, num_mcid_thres) # how many patients NEEDED treatment?
    # add to output dataframe:
    df_treatment <- rbind(
      df_treatment, 
      data.frame(therapy_id = rep(therapy_id, num_mcid_thres),           # what therapy is this?
                 budget = rep(df_params$budget, num_mcid_thres),                   # what is total therapy budget available?
                 mcid_thres = df_sim$mcid_thres[df_sim$therapy_id==therapy_id], # what is cutoff for therapy success?
                 num_patients_total = rep(num_patients, num_mcid_thres), # how many patients total?
                 num_patients_treated = num_patients_treated,            # how many patients can be treated?
                 num_patients_success = num_patients_success,            # how many patients were successfully treated?
                 therapy_cost_total = therapy_cost_total,                # cost of all treated patients
                 prop_patients_success = prop_patients_success
      )
    )
  } # for each therapy
} # for each # of patients

# save results:
write.csv(df_treatment, "data/results_group_probabilities.csv", row.names=FALSE)
