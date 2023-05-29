##########################################################
# Project: NeuroRehab Case Study 
# This file: analysis cases to for report
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################

#### Preamble ####
# add necessary packages:
source("code/packages.R")

# define some functions:
# helper function to set analysis parameters
set_analysis_params <- function(){
  return(data.frame(num_sim = 1000, # how many simulated groups of patients per therapy?
                    rand_seed = 16, # seed for random number generator (for reproducability)
                    mcid_thres_start = 4.2, # what is the starting threshold for meaningful fugel-meyer improvement?
                    mcid_thres_end = 7.2, # what is the upper bound for one unit of meaningful FM improvement?
                    mcid_thres_step = 0.1, # what is the step size between the start and end mcid for FM?
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


#### PART 1: perform % successful therapy analysis ####
df_params <- set_analysis_params()
set.seed(df_params$rand_seed)
# define the potential therapies:
df_therapies <- read.csv(here("data/therapies.csv"))

# simulate patient data for each potential therapy and compute the
# proportion of successes under varying minimal clinically important difference
# (mcid) for fugel-meyer (fm):
df_sim <- data.frame(mcid_thres = numeric(),
                     therapy_id = character(),
                     prop_success = numeric())
for (sim_n in seq(1, df_params$num_sim, 1)) { # each simulation
  for (ii in 1:nrow(df_therapies)) { # loop through each of the therapies
    therapy <- df_therapies[ii, ]
    # simulate patients' changes in fm as result of this specific therapy:
    fm_change <- rnorm(n = therapy$num_patients, mean = therapy$fm_change_mean, sd = therapy$fm_change_sd)
    # compute proportion of therapy "successes" for varying fm mcid threshold:
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


#### PART 1: plot ####
plot_one = TRUE
if (plot_one){
  # plot simulated success rates + confidence intervals (currently commented out)
  df_sim_plot <- df_sim
  therapies <- as.factor(df_sim_plot$therapy_id)
  p1 <-ggplot(df_sim) + 
    #geom_ribbon(aes(x=mcid_thres, ymin=lower_bound, ymax=upper_bound, group=therapy_id), fill="grey", alpha=0.2) +
    geom_line(aes(x=mcid_thres, y=mean_prop_success, group=therapy_id, color=therapies)) +
    ylim(0, 1) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(min(df_sim$mcid_thres), max(df_sim$mcid_thres)), expand = c(0,0)) +
    labs(title="Treatment success at given FM level", x ="Fugl-Meyer assessment for upper extremity", y = "% of study patients with successful treatment") +
    theme_classic()
  ggplotly(p1)
}


#### PART 2: perform % treated analysis ####
# define the cost per patient for each therapy (assuming therapy delivered exactly as explained in paper)
df_costs <- read.csv(here("data/costs.csv"))
# now scale up therapy 
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
    therapy_cost         <- sum(df_costs$cost_usd_2023[df_costs$therapy_id==therapy_id], na.rm=TRUE)
    therapy_sucess_rate  <- df_sim$mean_prop_success[df_sim$therapy_id==therapy_id]
    num_mcid_thres       <- length(therapy_sucess_rate)
    max_patients_covered <- floor(df_params$budget / therapy_cost)
    # can all patients can be covered given the budget?
    if (max_patients_covered >= num_patients){ 
      num_patients_treated <- rep(num_patients, num_mcid_thres)
    } else { # if not all patients can be covered, find out how many can be
      num_patients_treated <- rep(max_patients_covered, num_mcid_thres)
    }
    # calculate remaining fields:
    therapy_cost_total    <- num_patients_treated * therapy_cost
    num_patients_success  <- floor(num_patients_treated * therapy_sucess_rate) # how many patients are successful?
    prop_patients_success <- num_patients_success / num_patients
    num_patients_total    <- rep(num_patients, num_mcid_thres)
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


#### PART 2: plot ####
plot_two = FALSE
if (plot_two){
  # plot # patients vs. prop success (given a budget and one mcid threshold)
  df_treatment_plot <- subset(df_treatment, mcid_thres==5.2)
  therapies <- as.factor(df_treatment_plot$therapy_id)
  p2 <- ggplot(df_treatment_plot) + 
    geom_line(aes(x=num_patients_total, y=prop_patients_success, group=therapy_id, color=therapies)) +
    ylim(0, 1) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, max(df_treatment_plot$num_patients_total)), expand = c(0,0)) +
    labs(title="Treatment success given budget", x ="# of patients", y = "% of patients with successful treatment") +
    theme_classic()
  ggplotly(p2)
}