##########################################################
# Project: NeuroRehab Case Study 
# This file: functions needed for analysis_code.R
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

# define some functions:
# helper function to set analysis parameters
set_analysis_params <- function(){
  return(data.frame(num_sim = 1000, # how many simulated groups of patients per therapy?
                    rand_seed = 16, # seed for random number generator (for reproducability)
                    mcid_thres_start = 4.2, # what is the starting threshold for meaningful fugel-meyer improvement?
                    mcid_thres_end = 7.2, # what is the upper bound for one unit of meaningful FM improvement?
                    mcid_thres_step = 0.2, # what is the step size between the start and end mcid for FM?
                    budget = 1000000, # how much money is available for treatment of all patients?
                    max_patients = 10000) # max number of potential patients
  )
}
# function to check simulation accuracy:
check_simulation_accuracy <- function(df_therapies, df_params, plot_it){
  # this function will check that the mean and standard deviations of the simulated "patients" converge
  # on the mean/sd reported in each paper for each therapy
  set.seed(df_params$rand_seed) # for reproducability, reset the random seed to be the same as what was used in analysis
  df_sim_test <- data.frame(therapy_id = character(),
                            fm_therapy_mean = numeric(),
                            fm_therapy_sd = numeric(),
                            fm_sim_mean = numeric(),
                            fm_sd_mean = numeric(),
                            sim_num = integer())
  for (sim_n in seq(1, df_params$num_sim, 1)) { # each simulation
    for (ii in 1:nrow(df_therapies)) { # loop through each of the therapies
      therapy <- df_therapies[ii, ]
      # simulate patients' changes in fm as result of this specific therapy:
      fm_change <- rnorm(n = therapy$num_patients, mean = therapy$fm_change_mean, sd = therapy$fm_change_sd)
      # check the simulated data is accurate:
      df_sim_test <- rbind(df_sim_test,
                           data.frame(therapy_id = therapy$therapy_id,
                                      fm_therapy_mean = therapy$fm_change_mean,
                                      fm_therapy_sd = therapy$fm_change_sd,
                                      fm_sim_mean = mean(fm_change),
                                      fm_sim_sd = sd(fm_change),
                                      sm_num = sim_n
                           )
      ) # rbind
    } # for therapy_id
  } # for each simulation
  
  # compute deviations
  df_sim_test$fm_mean_diff <- df_sim_test$fm_therapy_mean - df_sim_test$fm_sim_mean
  df_sim_test$fm_sd_diff <- df_sim_test$fm_therapy_sd - df_sim_test$fm_sim_sd
  
  # do we plot the results?
  if (plot_it){
    # plot mean deviations
    p1 <- ggplot(data=df_sim_test, aes(therapy_id, fm_mean_diff)) +
      geom_hline(yintercept = 0) +
      geom_boxplot(varwidth=T, fill="grey") + 
      guides(x =  guide_axis(angle = 45)) +
      theme_classic() +
      scale_y_continuous(breaks = seq(-6,6,2)) +
      ylim(-6,6) +
      labs(title="Check: FM mean change", 
           subtitle="Difference of the mean FM change in each study vs. simulated data",
           x="therapy id",
           y="difference from paper mean")
    # plot sd deviations
    p2 <- ggplot(data=df_sim_test, aes(therapy_id, fm_sd_diff)) +
      geom_hline(yintercept = 0) +
      geom_boxplot(varwidth=T, fill="grey") + 
      guides(x =  guide_axis(angle = 45)) +
      scale_y_continuous(breaks = seq(-6,6,2)) +
      theme_classic() +
      ylim(-6,6) +
      labs(title="Check: FM sd change", 
           subtitle="Difference of the standard deviation of the FM change in each study vs. simulated data",
           x="therapy id",
           y="difference from paper standard deviation") 
    # arrange into one figure space
    grid.arrange(p1, p2, nrow = 1)
  }
  return(df_sim_test)
}