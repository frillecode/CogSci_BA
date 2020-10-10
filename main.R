# imports
library(pacman)
pacman::p_load(brms, dplyr, boot)
source("util1.r")
source("simulation.R")
source("analysis1.r")

###       ###
### SETUP ###
###       ###
number <- 101

### Citation chain
# Here you can insert an edgelist. The nodes represent each     ##### skrives om
# unique study ID from the edgelist. Running the simulation 
# creates a citation_chain for passing the posterior based 
# upon the connections in the edgelist. 
citation_list <- read_csv("my_edgelist.csv")

###            ###
### Parameters ###
###            ###

### True values
# these set the true values of the population
# putting multiple values in will cause simulations to run
# with different true values.
# The first four give the mean value of the four parameters
# that determine participant behavior. The next four give
# the variance in these parameters.
# Warning! var_sexs, var_conds and var_sex_conds do not currently
# do anything!
b_bases <- c(0)
b_sexs <- c(0)
b_conds <- c(0)
b_sex_conds <- c(1) #0, 1, 2
# var_bases <- c(0.5)
# var_sexs <- c(0)
# var_conds <- c(0)
# var_sex_conds <- c(0)                              ######### skrives ordentligt (evt slet var_bases osv.)
var_u <- 0.5
var_sig <- 0.2


### Size parameters
# These determine the size of the simulations for every set of
# parameter values given in the "True values" setction above.
# Warning! n_participants_per_experiment and n_people need to 
# be divisible by 4!
n_repeats <- 1
n_experiments_per_repeat <- 60
n_participants_per_experiment <- 80
n_trials_per_participant <- 25
n_people <- 100000

total_simulations <- length(b_bases) * length(b_sexs) * length(b_conds) * length(b_sex_conds) * length(var_bases) * length(var_sexs) * length(var_conds) * length(var_sex_conds) * n_repeats
current_simulation <- 1

### Analysis parameters
# These allow you to choose which analyses do you want

do_pp_citation <- T
do_pp_linear <- T

### Publication bias
publication_bias_sym <- F
publication_bias_asym <-  F

### Posterior-passing parameters
# These give you various options wrt posterior passing
# log only the final experiment from each chain:
pp_final_expt_only <- T

### Vectors to store meta-data
# This function is in util.R
# It creates a number of vectors that will be
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
        for (m in 1:length(var_bases)) {
          for (n in 1:length(var_sexs)) {
            for (o in 1:length(var_conds)) {
              for (p in 1:length(var_sex_conds)) {

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
              ### Do meta-analysis          ----------------- do not do this
              ###
              # This function is in analysis1.R
              # makes a meta-analysis for outputs of b-skep for each repeat
              #meta_analysis <- do_meta_analysis()
              
              ###
              ### Save results for each value of b_sex_conds
              ###
              if(exists("saved_results_final")){
                saved_results_final <- merge(saved_results, saved_results_final)
                #meta_analysis_final <- merge(meta_analysis, meta_analysis_final)
              } else {
                saved_results_final <- saved_results
                #meta_analysis_final <- meta_analysis
              }
              }
            }
          }
        }
      }
    }
  }
} # end of parameter value for loops


#save results
saved <- paste("Results/saved_results_", number, ".csv", sep = "") 
meta_a <- paste("Results/meta_analysis_results_", number, ".csv", sep = "")
write.csv(saved_results_final, saved)
write.csv(meta_analysis_final, meta_a)

#tidy_workspace()
