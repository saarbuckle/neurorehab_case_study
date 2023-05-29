
library(dplyr)
library(plotly)
library(ggplot2)
library(ggthemes)

set.seed(16) # for reproducability 


# define the potential therapies:
df_therapies <- data.frame(
  therapy_id = c("Cramer_IC_2019", "Cramer_TR_2019", "Ward_2019", "Daly_2019", "McCabe_2015"),
  num_patients = c(60, 62, 224, 31, 12),
  fm_change_mean = c(8.36, 7.86, 9, 9.4, 7.775),
  fm_change_sd = c(7.04, 6.68, 1.333, 7.1267, 1.8875)
)


# simulate patient data for each potential therapy and compute the
# proportion of successes under varying minimal clinically important difference
# (mcid) for fugel-meyer (fm):

df_sim <- data.frame(mcid_thres = numeric(),
                     therapy_id = character(),
                     prop_success = numeric())
num_sim <- 100
for (sim_n in seq(1,num_sim,1)) { # each simulation
  for (ii in 1:nrow(df_therapies)) { # loop through each of the therapies
    therapy <- df_therapies[ii, ]
    # simulate patients' changes in fm as result of this specific therapy:
    fm_change <- rnorm(n = therapy$num_patients, mean = therapy$fm_change_mean, sd = therapy$fm_change_sd)
    # compute proportion of therapy "successes" for varying fm mcid threshold:
    for (mcid in seq(4.2,7.3,0.1)) {
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

# plot simulated success rates + confidence intervals
#therapies <- as.factor(df_sim_plot$therapy_id)
#ggplot(df_sim_plot) + 
#  geom_ribbon(aes(x=mcid_thres, ymin=lower_bound, ymax=upper_bound, group=therapy), fill="grey", alpha=0.25) +
#  geom_line(aes(x=mcid_thres, y=mean_prop_success, group=therapy, color=therapies)) +
#  theme_classic()

# define the cost per patient for each therapy (assuming same hours)
df_cost <- data.frame(therapy_id = c("Cramer_IC_2019", "Cramer_TR_2019", "Ward_2019", "Daly_2019", "McCabe_2015"),
                      cost_type = c(1,1,1,1,1),
                      cost = c(878.62, 361.81, 3681.90, 4416.00, 4092.00)
                      )
# cost for complete IC therapy as described in cramer et al. (42.12 hours)
# cost for complete TR therapy as described in cramer et al. (42.12 hours)
# cost for ward et al. therapy (90 hours with 1:1 therapist ratio)
# cost for Daly therapy (300 hours with 1/3 OT per hour)

# define the budget
budget <- 1000000 # $1 million USD
max_patients <- 10000 # max number of potential patients

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


for (num_patients in seq(100, max_patients, 100)){ # for groups of varying patient sizes
  for (therapy_id in unique(df_cost$therapy_id)){ # loop through available therapies
    therapy_cost         <- sum(df_cost$cost[df_cost$therapy_id==therapy_id])
    therapy_sucess_rate  <- df_sim$mean_prop_success[df_sim$therapy_id==therapy_id]
    num_mcid_thres       <- length(therapy_sucess_rate)
    max_patients_covered <- floor(budget / therapy_cost)
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
                 budget = rep(budget, num_mcid_thres),                   # what is total therapy budget available?
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

# plot # patients vs. prop success (given a budget and one mcid threshold)
df_treatment_plot <- subset(df_treatment, mcid_thres==4.2)
therapies <- as.factor(df_treatment_plot$therapy_id)
p <- ggplot(df_treatment_plot) + 
  geom_line(aes(x=num_patients_total, y=prop_patients_success, group=therapy_id, color=therapies)) +
  ylim(0, 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, max(df_treatment_plot$num_patients_total)), expand = c(0,0)) +
  labs(title="Treatment success given budget", x ="# of patients", y = "% of patients with successful treatment") +
  theme_classic()
ggplotly(p)