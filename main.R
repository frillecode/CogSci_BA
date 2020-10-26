# imports
library(pacman)
pacman::p_load(brms, dplyr, boot, igraph)
source("util1.r")
source("simulation.R")
source("analysis1.r")

###       ###
### SETUP ###
###       ###
number <- 101

###            ###
### Parameters ###
###            ###

### True values
# these set the true values of the population
# putting multiple values in will cause simulations to run
# with different true values.
# The first four give the mean value of the four parameters
# that determine participant behavior. The next two allow you
# to determine the mean and sd of the normal distribution that
# that the individual variance of the simulated population 
# will be randomly drawn from for each repeat. 
b_bases <- c(0)
b_sexs <- c(0)
b_conds <- c(0)
b_sex_conds <- c(1) #0, 1, 2

var_u <- 0.5 #mean
var_sig <- 0.2 #sd

### Citation chain
# Here you can insert an edgelist. Based on the degree 
# distribution of this edgelist, a citation_chain where
# each node represents a study and the connections represent 
# citations between studies through which posteriors can
# be passed. The size of this citation_chain can be specified
# in size parameters below (number of nodes specified in 
# n_experiments_per_repeat below). 
citation_list <- read_csv("my_edgelist.csv") 
citation_list <- graph_from_data_frame(d=citation_list, directed=T)

### Size parameters
# These determine the size of the simulations for every set of
# parameter values given in the "True values" section above.
# Warning! n_participants_per_experiment and n_people need to 
# be divisible by 4!
n_repeats <- 2
n_experiments_per_repeat <- 4
n_participants_per_experiment <- 80
n_trials_per_participant <- 25
n_people <- 100000

total_simulations <- length(b_bases) * length(b_sexs) * length(b_conds) * length(b_sex_conds) * n_repeats
current_simulation <- 1

### Analysis parameters
# These allow you to choose which analyses do you want
do_pp_linear <- T
do_pp_citation <- T

### Publication bias
# This allows you to choose whether or not the analysis 
# should be run with publication bias or not. If you choose
# to run with publication bias, you can set the values for 
# the probability of being published depending on direction
# of effect and whether or not CIs include 0. This also allows
# you do decide whether you want the publication bias to be 
# symmetric or asymmetric. 
do_publication_bias <- T

pb_prob_pos <- 0.9 #prob if b above zero and b lower above zero
pb_prob_null <- 0.2 #prob if b above zero and b lower below zero OR b below zero and b upper above zero
pb_prob_neg <- 0.6 #prob if b below zero and b upper below zero

### Posterior-passing parameters
# These give you various options wrt posterior passing
# log only the final experiment from each chain:
pp_final_expt_only <- T

### Vectors to store meta-data
# This function is in util.R It creates a number of vectors that will be
# filled with data. Each set of simulations for a set of parameters
# creates a results table. At the end of each set of parameter values,
# the meta_results table is filled in with a single row that gives data
# on the results across those simulations.
prepare_meta_vectors()

###                        ###
### SIMULATIONS START HERE ###
###                        ###

### For loops to iterate through parameter values
for (i in 1:length(b_bases)) {
  for (j in 1:length(b_sexs)) {
    for (k in 1:length(b_conds)) {
      for (l in 1:length(b_sex_conds)) {

              ### Set up values for the simulation
              # this function is in util.R
              # also save the values to the meta vectors
              prepare_for_simulation()

              ### Vectors to store data
              # this function is in util.R
              # These store the results of simulations within each set
              # of parameter values
              prepare_data_vectors()
              
              ###
              ### Each repeat starts here
              ###
              
              for (rep in 1:n_repeats) {
                print(paste(">>>> Repeat", rep, "of", n_repeats, sep=" "))
                print(paste(">>>> Simulation", current_simulation, "of", total_simulations, sep=" "))
                current_simulation <- current_simulation + 1
              
                ###
                ### Create the population ###
                ###
                # This function is in simulation.R
                # The population is simply a data table with 3 columns:
                # id, sex, d_base
                # It is created with the "doppelganger quadrangle" method
                # so there are equal number of men and women and the true
                # mean and true variance of each sub population is the same
                population <- create_population()
                
                ###
                ### Create citation chain
                ###
                citation_chain <- create_citation_chain(citation_list)
                
                ###
                ### Create the datasets for each experiment ###
                ###
                # This function is in simulation.R
                # The datasets are stored in a single table with 5 columns:
                # data_set - the id of the experiment
                # participant_id - the id of the participant within that experiment
                # sex - the sex of the participant
                # condition - the condition the participant was in
                # response - the mean response of that participant, ranging from 0 to 1
                data_sets <- create_datasets()
                
                ###
                ### run analyses ###
                ###
                # this function is in analyses.R
                # Now that we have all the data_sets we perform the desired analyses over them.
                # It is in these methods that we start to create the results table. Each entry
                # in this table corresponds to a single analysis on a single data_set.
                do_analyses_1()
                
              } # end of for each repeat loop
              
              ###
              ### Create the Meta-results table
              ###
              # this function is in util.R
              # now we have all our results so we parse them as a single table and collapse
              # each repeat simulation (i.e. each run of experiments) into a single entry
              save_results_meta()
              
              ###
              ### Save results for each value of b_sex_conds
              ###
              if(exists("saved_results_final")){
                saved_results_final <- merge(saved_results, saved_results_final)
              } else {
                saved_results_final <- saved_results
              }
              }
            }
          }
} # end of parameter value for loops

#save results
meta_results <- compile_meta_results()

#save results
# saved <- paste("Results/saved_results_", number, ".csv", sep = "")          ###### ??????
# meta_a <- paste("Results/meta_analysis_results_", number, ".csv", sep = "") ###### ??????
# write.csv(saved_results_final, saved)                                       ###### ??????
# write.csv(meta_analysis_final, meta_a)                                      ###### ??????

#tidy_workspace()
