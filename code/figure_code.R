##########################################################
# Project: NeuroRehab Case Study 
# This file: code to produce figures for report
# 05/2023
# Spencer Arbuckle
# R version 4.2.2
##########################################################
# This code is written such that each figure is written as one function.
# It assumes the analysis is already run and results files are saved in the 
# `data` folder.

#### Add some other libraries and functions ####
# add necessary packages:
source("code/packages.R")
# add some functions:
source("code/functions.R")
text_size <<- 7 # assign text size for figures
df_colours <<- read.csv(here("data/therapies_colours.csv")) # perscribed plotting colours for therapies

#### Plots ####
fig1 <- function(){
  # make therapy info plots (FIGURE 1)
  
  # get needed data:
  df_therapies <- read.csv(here("data/therapies.csv")) # the potential therapies
  df_costs     <- read.csv(here("data/costs.csv")) # estimated costs for each therapy
  
  # arrange data frames for plotting
  df_therapies_sorted <- df_therapies %>%
    mutate(therapy_id = fct_reorder(therapy_id, therapy_hours, .desc = TRUE)) %>% # sort therapies for plotting by # treatment hours
    merge(., df_colours) # add perscribed plotting colours for therapies
  df_cost_total <- df_costs %>% # average the costs across the three components (labour, equipment, travel) per therapy 
    group_by(therapy_id) %>% 
    summarize(cost_per_patient = sum(cost_usd_2023, na.rm=TRUE))
  # add travel costs to plotting data frame
  df_therapies_sorted <- merge(df_therapies_sorted, df_cost_total) 
  
  # MAKE THE PLOTS
  p1 <- ggplot(df_therapies_sorted, # plot therapy hours (x) per therapy
               aes(x = therapy_hours,
                   xend = 0,
                   y = therapy_id,
                   yend = therapy_id,
                   colour = therapy_id)) +
    geom_segment(show.legend=FALSE) +
    geom_point(show.legend=FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(breaks = c(df_therapies_sorted$therapy_id),
                        values = c(df_therapies_sorted$colour)) +
    labs(x = "reported rehab hours",
         y = "",
         title = "",
         caption = "") +
    theme_classic() + coord_cartesian(clip = "off") +
    theme(text = element_text(size=text_size))
  # plot therapist-patient ratio (x) per therapy
  p2 <- ggplot(df_therapies_sorted, 
               aes(x = therapist_patient_ratio,
                   xend = 0,
                   y = therapy_id,
                   yend = therapy_id,
                   colour = therapy_id)) +
    geom_segment(show.legend=FALSE) +
    geom_point(show.legend=FALSE) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(breaks = c(df_therapies_sorted$therapy_id),
                        values = c(df_therapies_sorted$colour)) +
    labs(x = "therapist-to-patient ratio",
         y = "",
         title = "",
         caption = "") +
    theme_classic() + coord_cartesian(clip = "off") +
    theme(text = element_text(size=text_size))
  # plot therapy remoteness (x) per therpay
  p3 <- ggplot(df_therapies_sorted, 
               aes(x = proportion_treatment_remote,
                   xend = 0,
                   y = therapy_id,
                   yend = therapy_id,
                   colour = therapy_id)) +
    geom_segment(show.legend=FALSE) +
    geom_point(show.legend=FALSE) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(breaks = c(df_therapies_sorted$therapy_id),
                        values = c(df_therapies_sorted$colour)) +
    labs(x = "proportion of rehab\ndone remotely",
         y = "",
         title = "",
         caption = "") +
    theme_classic() + coord_cartesian(clip = "off") +
    theme(text = element_text(size=text_size))
  # plot therapy cost (x) per therapy
  p4 <- ggplot(df_therapies_sorted, 
               aes(x = cost_per_patient,
                   xend = 0,
                   y = therapy_id,
                   yend = therapy_id,
                   colour = therapy_id)) +
    geom_segment(show.legend=FALSE) +
    geom_point(show.legend=FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(breaks = c(df_therapies_sorted$therapy_id),
                        values = c(df_therapies_sorted$colour)) +
    labs(x = "estimated cost per patient ($USD)",
         y = "",
         title = "",
         caption = "") +
    theme_classic() + coord_cartesian(clip = "off") +
    theme(text = element_text(size=text_size))
  # plot the subplots together
  gp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
  # save figure
  ggsave(filename = "figures/fig1.png", gp , width = 15, height = 10, dpi = 600, units = "cm", device='png')
#  fig <- subplot(ggplotly(p1), 
#                 ggplotly(p2), 
#                 ggplotly(p3),
#                 ggplotly(p4),
#                 nrows = 2)
#  fig$x$data[[1]]$showlegend <- FALSE
#  fig$x$data[[2]]$showlegend <- FALSE
#  fig$x$data[[3]]$showlegend <- FALSE
#  fig$x$data[[4]]$showlegend <- FALSE
#  fig
}
fig2 <- function(){
  # make therapy component dot plots (FIGURE 2)
  
  # get needed data:
  df_therapies <- read.csv(here("data/therapies.csv")) # the potential therapies
  df_therapies_plot <- merge(df_therapies, df_colours) # add perscribed plotting colours for therapies
  
  # MAKE PLOTS
  # plot therapy_hours (x) vs. fm-ue change (y)
  p1 <- ggplot(data = df_therapies_plot) +
    geom_point(aes(x=therapy_hours, fm_change_mean, color=therapy_id)) +
    ylim(0, 20) +
    scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
    labs(title="", x ="total rehab hours", y = "mean FM-UE increase") +
    scale_colour_manual(breaks = c(df_therapies_plot$therapy_id),
                        values = c(df_therapies_plot$colour)) +
    theme_classic() + theme(text = element_text(size=text_size))
  # plot therapist-patient ratio (x) vs. fm-ue change (y)
  p2 <- ggplot(data = df_therapies_plot) +
    geom_point(aes(x=therapist_patient_ratio, fm_change_mean, color=therapy_id)) +
    ylim(0, 1) +
    scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
    coord_cartesian(clip = "off") + 
    labs(title="", x ="therapist-to-patient ratio", y = "") +
    scale_colour_manual(breaks = c(df_therapies_plot$therapy_id),
                        values = c(df_therapies_plot$colour)) +
    theme_classic() + theme(text = element_text(size=text_size))
  # plot therapy remoteness (x) vs. fm-ue change (y)
  p3 <- ggplot(data = df_therapies_plot) +
    geom_point(aes(x=proportion_treatment_remote, fm_change_mean, color=therapy_id)) +
    ylim(0, 1) +
    scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
    scale_x_continuous(limits = c(-0.05, 1), expand = c(0,0)) +
    coord_cartesian(clip = "off") +
    labs(title="", x ="proportion of rehab\noffered remotely", y = "") +
    scale_colour_manual(breaks = c(df_therapies_plot$therapy_id),
                        values = c(df_therapies_plot$colour)) +
    theme_classic() + theme(text = element_text(size=text_size))
  # plot the subplots together
  gp <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  # save figure
  ggsave(filename = "figures/fig2.png", gp , width = 12, height = 7, dpi = 600, units = "cm", device='png')
}
fig3 <- function(){
  # make probability treatment success plots (FIGURE 1)
  
  # get needed data:
  df_therapies <- read.csv(here("data/therapies.csv")) # the potential therapies
  df_plot_1 <- read.csv(here("data/results_individual_probabilities.csv")) %>% # probability successful treatment for an individual patient
    merge(., df_colours) # add perscribed plotting colours for therapies
  df_plot_2 <- read.csv(here("data/results_group_probabilities.csv")) %>% # probability successful treatment for patient in a larger group
    merge(., df_colours) # add perscribed plotting colours for therapies
  df_plot_2 <- subset(df_plot_2, df_plot_2$mcid_thres>6.5 & df_plot_2$mcid_thres<6.7) # restrict second plot to probabilities for MCID of 6.6
  
  # calculate probability of patient in larger group having successful treatment in remote vs. in-person rehab
  df_plot_3 <- df_plot_2 %>% 
    merge(., subset(df_therapies, select=c(therapy_id,proportion_treatment_remote)))
  df_plot_3$proportion_treatment_remote <- df_plot_3$proportion_treatment_remote>0 # which therapies are remote-ish?
  
  df_plot_3 <- df_plot_3 %>% 
    group_by(num_patients_total, proportion_treatment_remote) %>% 
    summarize(mean_success = mean(prop_patients_success))
  
  df_plot_3_inperson <- df_plot_3[df_plot_3$proportion_treatment_remote==FALSE,]
  df_plot_3_remote <- df_plot_3[df_plot_3$proportion_treatment_remote==TRUE,]
  df_plot_3_remote$mean_diff <- df_plot_3_remote$mean_success - df_plot_3_inperson$mean_success
  
  
  # MAKE PLOTS
  p1 <-ggplot(df_plot_1) + 
    #geom_ribbon(aes(x=mcid_thres, ymin=lower_bound, ymax=upper_bound, group=therapy_id), fill="grey", alpha=0.2) +
    geom_vline(xintercept = 6.6, linetype="dotted", color="grey") + 
    geom_line(aes(x=mcid_thres, y=mean_prop_success, color=therapy_id, linetype=therapy_id)) +
    ylim(0, 1) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(min(df_plot_1$mcid_thres), max(df_plot_1$mcid_thres)), expand = c(0,0)) +
    labs(title="Treatment success", x ="FM-UE thresholds", y = "probability of rehab success") +
    theme_classic() + theme(text = element_text(size=text_size)) +
    scale_colour_manual(breaks = c(df_plot_1$therapy_id),
                        values = c(df_plot_1$colour)) +
    scale_linetype_manual(values = c(rep("longdash", 3), "solid", "longdash", rep("solid", 2)))
  
  p2 <- ggplot(df_plot_2) + 
    geom_line(aes(x=num_patients_total, y=prop_patients_success, color=therapy_id, linetype=therapy_id)) +
    ylim(0, 1) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, max(df_plot_2$num_patients_total)), expand = c(0,0)) +
    labs(title="Treatment success given budget", x ="# of patients", y = "% of all patients with clinical improvement") +
    theme_classic() + theme(text = element_text(size=text_size)) +
    scale_colour_manual(breaks = c(df_plot_2$therapy_id),
                        values = c(df_plot_2$colour)) +
    scale_linetype_manual(values = c(rep("longdash", 3), "solid", "longdash", rep("solid", 2)))
  
  p3<- ggplot(data=df_plot_3_remote) + 
    geom_hline(yintercept=0, linetype="dotted") + 
    geom_line(aes(x=num_patients_total, y=mean_diff)) + 
    scale_y_continuous(labels = scales::percent_format(), limits = c(-0.4, 0.4), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, max(df_plot_3_remote$num_patients_total)), expand = c(0,0)) +
    labs(title="Remote vs. in-person scalability", x ="# of patients", y = "difference in % clinical improvement") +
    theme_classic() + theme(text = element_text(size=text_size))
  
  # plot subplots
  gp <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "right")
  # save figure
  ggsave(filename = "figures/fig3.png", gp , width = 20, height = 7, dpi = 600, units = "cm", device='png')
}
