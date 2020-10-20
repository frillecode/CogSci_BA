prepare_meta_vectors <- function() {
  total_meta_conditions <<- length(b_bases)*length(b_sexs)*length(b_conds)*length(b_sex_conds)
  meta_n_repeats <<- rep(n_repeats, total_meta_conditions)
  meta_n_experiments_per_repeat <<- rep(n_experiments_per_repeat, total_meta_conditions)
  meta_n_participants_per_experiment <<- rep(n_participants_per_experiment, total_meta_conditions)
  meta_n_trials_per_participant <<- rep(n_trials_per_participant, total_meta_conditions)
  meta_n_people <<- rep(n_people, total_meta_conditions)
  meta_true_base <<- vector()
  meta_true_sex <<- vector()
  meta_true_cond <<- vector()
  meta_true_sex_cond <<- vector()
  
  meta_base_estimate_pp_l <<- vector()
  meta_sex_estimate_pp_l <<- vector()
  meta_cond_estimate_pp_l <<- vector()
  meta_sex_cond_estimate_pp_l <<- vector()
  
  meta_base_estimate_pp_l_pb <<- vector()
  meta_sex_estimate_pp_l_pb <<- vector()
  meta_cond_estimate_pp_l_pb <<- vector()
  meta_sex_cond_estimate_pp_l_pb <<- vector()
  
  meta_base_estimate_pp_c <<- vector()
  meta_sex_estimate_pp_c <<- vector()
  meta_cond_estimate_pp_c <<- vector()
  meta_sex_cond_estimate_pp_c <<- vector()
  
  meta_base_estimate_pp_c_pb <<- vector()
  meta_sex_estimate_pp_c_pb <<- vector()
  meta_cond_estimate_pp_c_pb <<- vector()
  meta_sex_cond_estimate_pp_c_pb <<- vector()
  
  meta_base_estimate_upper_pp_l <<- vector()
  meta_sex_estimate_upper_pp_l <<- vector()
  meta_cond_estimate_upper_pp_l <<- vector()
  meta_sex_cond_estimate_upper_pp_l <<- vector()
  
  meta_base_estimate_upper_pp_l_pb <<- vector()
  meta_sex_estimate_upper_pp_l_pb <<- vector()
  meta_cond_estimate_upper_pp_l_pb <<- vector()
  meta_sex_cond_estimate_upper_pp_l_pb <<- vector()
  
  meta_base_estimate_upper_pp_c <<- vector()
  meta_sex_estimate_upper_pp_c <<- vector()
  meta_cond_estimate_upper_pp_c <<- vector()
  meta_sex_cond_estimate_upper_pp_c <<- vector()
  
  meta_base_estimate_upper_pp_c_pb <<- vector()
  meta_sex_estimate_upper_pp_c_pb <<- vector()
  meta_cond_estimate_upper_pp_c_pb <<- vector()
  meta_sex_cond_estimate_upper_pp_c_pb <<- vector()

  meta_base_estimate_lower_pp_l <<- vector()
  meta_sex_estimate_lower_pp_l <<- vector()
  meta_cond_estimate_lower_pp_l <<- vector()
  meta_sex_cond_estimate_lower_pp_l <<- vector()
  
  meta_base_estimate_lower_pp_l_pb <<- vector()
  meta_sex_estimate_lower_pp_l_pb <<- vector()
  meta_cond_estimate_lower_pp_l_pb <<- vector()
  meta_sex_cond_estimate_lower_pp_l_pb <<- vector()
  
  meta_base_estimate_lower_pp_c <<- vector()
  meta_sex_estimate_lower_pp_c <<- vector()
  meta_cond_estimate_lower_pp_c <<- vector()
  meta_sex_cond_estimate_lower_pp_c <<- vector()
  
  meta_base_estimate_lower_pp_c_pb <<- vector()
  meta_sex_estimate_lower_pp_c_pb <<- vector()
  meta_cond_estimate_lower_pp_c_pb <<- vector()
  meta_sex_cond_estimate_lower_pp_c_pb <<- vector()
  
  meta_sex_positive_rate_pp_l <<- vector()
  meta_cond_positive_rate_pp_l <<- vector()
  meta_sex_cond_positive_rate_pp_l <<- vector()
  
  meta_sex_positive_rate_pp_l_pb <<- vector()
  meta_cond_positive_rate_pp_l_pb <<- vector()
  meta_sex_cond_positive_rate_pp_l_pb <<- vector()
  
  meta_sex_positive_rate_pp_c <<- vector()
  meta_cond_positive_rate_pp_c <<- vector()
  meta_sex_cond_positive_rate_pp_c <<- vector()
  
  meta_sex_positive_rate_pp_c_pb <<- vector()
  meta_cond_positive_rate_pp_c_pb <<- vector()
  meta_sex_cond_positive_rate_pp_c_pb <<- vector()

  meta_base_uncertainty_pp_l <<- vector()
  meta_sex_uncertainty_pp_l <<- vector()
  meta_cond_uncertainty_pp_l <<- vector()
  meta_sex_cond_uncertainty_pp_l <<- vector()
  
  meta_base_uncertainty_pp_l_pb <<- vector()
  meta_sex_uncertainty_pp_l_pb <<- vector()
  meta_cond_uncertainty_pp_l_pb <<- vector()
  meta_sex_cond_uncertainty_pp_l_pb <<- vector()
  
  meta_base_uncertainty_pp_c <<- vector()
  meta_sex_uncertainty_pp_c <<- vector()
  meta_cond_uncertainty_pp_c <<- vector()
  meta_sex_cond_uncertainty_pp_c <<- vector()
  
  meta_base_uncertainty_pp_c_pb <<- vector()
  meta_sex_uncertainty_pp_c_pb <<- vector()
  meta_cond_uncertainty_pp_c_pb <<- vector()
  meta_sex_cond_uncertainty_pp_c_pb <<- vector()
}

prepare_data_vectors <- function() {
  repeat_id <<- vector()
  expt <<- vector()
  analysis_type <<- vector()
  true_base <<- vector()
  b_base_lower <<- vector()
  b_base_med <<- vector()
  b_base_upper <<- vector()
  b_base_error <<- vector()
  true_sex <<- vector()
  b_sex_p_value <<- vector()
  b_sex_lower <<- vector()
  b_sex_med <<- vector()
  b_sex_upper <<- vector()
  b_sex_error <<- vector()
  true_cond <<- vector()
  b_cond_p_value <<- vector()
  b_cond_lower <<- vector()
  b_cond_med <<- vector()
  b_cond_upper <<- vector()
  b_cond_error <<- vector()
  true_sex_cond <<- vector()
  b_sex_cond_p_value <<- vector()
  b_sex_cond_lower <<- vector()
  b_sex_cond_med <<- vector()
  b_sex_cond_upper <<- vector()
  b_sex_cond_error <<- vector()
  pub_true <<- vector()
  var_pop <<- vector()  
}

save_analysis_results_1 <- function(type) {
  repeat_id <<- c(repeat_id, rep(rep, n_experiments_per_repeat))
  expt <<- c(expt, c(1:n_experiments_per_repeat))
  true_base <<- c(true_base, rep(b_base, n_experiments_per_repeat))
  true_sex <<- c(true_sex, rep(b_sex, n_experiments_per_repeat))
  true_cond <<- c(true_cond, rep(b_cond, n_experiments_per_repeat))
  true_sex_cond <<- c(true_sex_cond, rep(b_sex_cond, n_experiments_per_repeat))
  analysis_type <<- c(analysis_type, rep(type, n_experiments_per_repeat))
  var_pop <<- c(var_pop, rep(population$var_base[1], n_experiments_per_repeat)) 
}

save_results_meta <- function() {
  print(">>>>>>>> Saving results")
  # combine results into a data frame
  results <- data.frame(repeat_id, expt, analysis_type,
                        true_base, b_base_lower, b_base_med, b_base_upper, b_base_error,
                        true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper, b_sex_error,
                        true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, b_cond_error,
                        true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper, b_sex_cond_error, 
                        pub_true, pp_n, var_pop) 
  saved_results <<- results
  rm(this_data_set, model,
     repeat_id, expt, analysis_type,
     true_base, b_base_lower, b_base_med, b_base_upper,
     true_sex, b_sex_p_value, b_sex_lower, b_sex_med, b_sex_upper,
     true_cond, b_cond_p_value, b_cond_lower, b_cond_med, b_cond_upper, 
     true_sex_cond, b_sex_cond_p_value, b_sex_cond_lower, b_sex_cond_med, b_sex_cond_upper, var_pop,
     pos = ".GlobalEnv")
  
  for (rep in 1:n_repeats) {
    pp_l_results <- results[results$repeat_id == rep & results$analysis_type == "pp_linear",]
    pp_l_pb_results <- results[results$repeat_id == rep & results$analysis_type == "pp_linear_pb",]
    pp_c_results <- results[results$repeat_id == rep & results$analysis_type == "pp_citation",]
    pp_c_pb_results <- results[results$repeat_id == rep & results$analysis_type == "pp_citation_pb",]
    
    # estimate
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_pp_l <<- c(meta_base_estimate_pp_l, mean(pp_l_results$b_base_med))
      meta_sex_estimate_pp_l <<- c(meta_sex_estimate_pp_l, mean(pp_l_results$b_sex_med))
      meta_cond_estimate_pp_l <<- c(meta_cond_estimate_pp_l, mean(pp_l_results$b_cond_med))
      meta_sex_cond_estimate_pp_l <<- c(meta_sex_cond_estimate_pp_l, mean(pp_l_results$b_sex_cond_med))
    } else {
      meta_base_estimate_pp_l <<- c(meta_base_estimate_pp_l, mean(pp_l_results$b_base_med[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_pp_l <<- c(meta_sex_estimate_pp_l, mean(pp_l_results$b_sex_med[pp_l_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_pp_l <<- c(meta_cond_estimate_pp_l, mean(pp_l_results$b_cond_med[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_pp_l <<- c(meta_sex_cond_estimate_pp_l, mean(pp_l_results$b_sex_cond_med[pp_l_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_pp_l_pb <<- c(meta_base_estimate_pp_l_pb, mean(pp_l_pb_results$b_base_med))
      meta_sex_estimate_pp_l_pb <<- c(meta_sex_estimate_pp_l_pb, mean(pp_l_pb_results$b_sex_med))
      meta_cond_estimate_pp_l_pb <<- c(meta_cond_estimate_pp_l_pb, mean(pp_l_pb_results$b_cond_med))
      meta_sex_cond_estimate_pp_l_pb <<- c(meta_sex_cond_estimate_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_med))
    } else {
      meta_base_estimate_pp_l_pb <<- c(meta_base_estimate_pp_l_pb, mean(pp_l_pb_results$b_base_med[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_pp_l_pb <<- c(meta_sex_estimate_pp_l_pb, mean(pp_l_pb_results$b_sex_med[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_pp_l_pb <<- c(meta_cond_estimate_pp_l_pb, mean(pp_l_pb_results$b_cond_med[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_pp_l_pb <<- c(meta_sex_cond_estimate_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_med[pp_l_pb_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_pp_c <<- c(meta_base_estimate_pp_c, mean(pp_c_results$b_base_med))
      meta_sex_estimate_pp_c <<- c(meta_sex_estimate_pp_c, mean(pp_c_results$b_sex_med))
      meta_cond_estimate_pp_c <<- c(meta_cond_estimate_pp_c, mean(pp_c_results$b_cond_med))
      meta_sex_cond_estimate_pp_c <<- c(meta_sex_cond_estimate_pp_c, mean(pp_c_results$b_sex_cond_med))
    } else {
      meta_base_estimate_pp_c <<- c(meta_base_estimate_pp_c, mean(pp_c_results$b_base_med[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_pp_c <<- c(meta_sex_estimate_pp_c, mean(pp_c_results$b_sex_med[pp_c_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_pp_c <<- c(meta_cond_estimate_pp_c, mean(pp_c_results$b_cond_med[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_pp_c <<- c(meta_sex_cond_estimate_pp_c, mean(pp_c_results$b_sex_cond_med[pp_c_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_pp_c_pb <<- c(meta_base_estimate_pp_c_pb, mean(pp_c_pb_results$b_base_med))
      meta_sex_estimate_pp_c_pb <<- c(meta_sex_estimate_pp_c_pb, mean(pp_c_pb_results$b_sex_med))
      meta_cond_estimate_pp_c_pb <<- c(meta_cond_estimate_pp_c_pb, mean(pp_c_pb_results$b_cond_med))
      meta_sex_cond_estimate_pp_c_pb <<- c(meta_sex_cond_estimate_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_med))
    } else {
      meta_base_estimate_pp_c_pb <<- c(meta_base_estimate_pp_c_pb, mean(pp_c_pb_results$b_base_med[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_pp_c_pb <<- c(meta_sex_estimate_pp_c_pb, mean(pp_c_pb_results$b_sex_med[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_pp_c_pb <<- c(meta_cond_estimate_pp_c_pb, mean(pp_c_pb_results$b_cond_med[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_pp_c_pb <<- c(meta_sex_cond_estimate_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_med[pp_c_pb_results$expt == n_experiments_per_repeat]))
    }
    
    # Upper
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_upper_pp_l <<- c(meta_base_estimate_upper_pp_l, mean(pp_l_results$b_base_upper))
      meta_sex_estimate_upper_pp_l <<- c(meta_sex_estimate_upper_pp_l, mean(pp_l_results$b_sex_upper))
      meta_cond_estimate_upper_pp_l <<- c(meta_cond_estimate_upper_pp_l, mean(pp_l_results$b_cond_upper))
      meta_sex_cond_estimate_upper_pp_l <<- c(meta_sex_cond_estimate_upper_pp_l, mean(pp_l_results$b_sex_cond_upper))
    } else {
      meta_base_estimate_upper_pp_l <<- c(meta_base_estimate_upper_pp_l, mean(pp_l_results$b_base_upper[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_upper_pp_l <<- c(meta_sex_estimate_upper_pp_l, mean(pp_l_results$b_sex_upper[pp_l_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_upper_pp_l <<- c(meta_cond_estimate_upper_pp_l, mean(pp_l_results$b_cond_upper[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_upper_pp_l <<- c(meta_sex_cond_estimate_upper_pp_l, mean(pp_l_results$b_sex_cond_upper[pp_l_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_upper_pp_l_pb <<- c(meta_base_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_base_upper))
      meta_sex_estimate_upper_pp_l_pb <<- c(meta_sex_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_sex_upper))
      meta_cond_estimate_upper_pp_l_pb <<- c(meta_cond_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_cond_upper))
      meta_sex_cond_estimate_upper_pp_l_pb <<- c(meta_sex_cond_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_upper))
    } else {
      meta_base_estimate_upper_pp_l_pb <<- c(meta_base_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_base_upper[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_upper_pp_l_pb <<- c(meta_sex_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_sex_upper[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_upper_pp_l_pb <<- c(meta_cond_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_cond_upper[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_upper_pp_l_pb <<- c(meta_sex_cond_estimate_upper_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_upper[pp_l_pb_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_upper_pp_c <<- c(meta_base_estimate_upper_pp_c, mean(pp_c_results$b_base_upper))
      meta_sex_estimate_upper_pp_c <<- c(meta_sex_estimate_upper_pp_c, mean(pp_c_results$b_sex_upper))
      meta_cond_estimate_upper_pp_c <<- c(meta_cond_estimate_upper_pp_c, mean(pp_c_results$b_cond_upper))
      meta_sex_cond_estimate_upper_pp_c <<- c(meta_sex_cond_estimate_upper_pp_c, mean(pp_c_results$b_sex_cond_upper))
    } else {
      meta_base_estimate_upper_pp_c <<- c(meta_base_estimate_upper_pp_c, mean(pp_c_results$b_base_upper[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_upper_pp_c <<- c(meta_sex_estimate_upper_pp_c, mean(pp_c_results$b_sex_upper[pp_c_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_upper_pp_c <<- c(meta_cond_estimate_upper_pp_c, mean(pp_c_results$b_cond_upper[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_upper_pp_c <<- c(meta_sex_cond_estimate_upper_pp_c, mean(pp_c_results$b_sex_cond_upper[pp_c_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_upper_pp_c_pb <<- c(meta_base_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_base_upper))
      meta_sex_estimate_upper_pp_c_pb <<- c(meta_sex_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_sex_upper))
      meta_cond_estimate_upper_pp_c_pb <<- c(meta_cond_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_cond_upper))
      meta_sex_cond_estimate_upper_pp_c_pb <<- c(meta_sex_cond_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_upper))
    } else {
      meta_base_estimate_upper_pp_c_pb <<- c(meta_base_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_base_upper[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_upper_pp_c_pb <<- c(meta_sex_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_sex_upper[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_upper_pp_c_pb <<- c(meta_cond_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_cond_upper[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_upper_pp_c_pb <<- c(meta_sex_cond_estimate_upper_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_upper[pp_c_pb_results$expt == n_experiments_per_repeat]))
    }
    
    # Lower
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_lower_pp_l <<- c(meta_base_estimate_lower_pp_l, mean(pp_l_results$b_base_lower))
      meta_sex_estimate_lower_pp_l <<- c(meta_sex_estimate_lower_pp_l, mean(pp_l_results$b_sex_lower))
      meta_cond_estimate_lower_pp_l <<- c(meta_cond_estimate_lower_pp_l, mean(pp_l_results$b_cond_lower))
      meta_sex_cond_estimate_lower_pp_l <<- c(meta_sex_cond_estimate_lower_pp_l, mean(pp_l_results$b_sex_cond_lower))
    } else {
      meta_base_estimate_lower_pp_l <<- c(meta_base_estimate_lower_pp_l, mean(pp_l_results$b_base_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_lower_pp_l <<- c(meta_sex_estimate_lower_pp_l, mean(pp_l_results$b_sex_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_lower_pp_l <<- c(meta_cond_estimate_lower_pp_l, mean(pp_l_results$b_cond_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_lower_pp_l <<- c(meta_sex_cond_estimate_lower_pp_l, mean(pp_l_results$b_sex_cond_lower[pp_l_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_lower_pp_l_pb <<- c(meta_base_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_base_lower))
      meta_sex_estimate_lower_pp_l_pb <<- c(meta_sex_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_sex_lower))
      meta_cond_estimate_lower_pp_l_pb <<- c(meta_cond_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_cond_lower))
      meta_sex_cond_estimate_lower_pp_l_pb <<- c(meta_sex_cond_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_lower))
    } else {
      meta_base_estimate_lower_pp_l_pb <<- c(meta_base_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_base_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_lower_pp_l_pb <<- c(meta_sex_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_sex_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_lower_pp_l_pb <<- c(meta_cond_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_cond_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_lower_pp_l_pb <<- c(meta_sex_cond_estimate_lower_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_lower_pp_c <<- c(meta_base_estimate_lower_pp_c, mean(pp_c_results$b_base_lower))
      meta_sex_estimate_lower_pp_c <<- c(meta_sex_estimate_lower_pp_c, mean(pp_c_results$b_sex_lower))
      meta_cond_estimate_lower_pp_c <<- c(meta_cond_estimate_lower_pp_c, mean(pp_c_results$b_cond_lower))
      meta_sex_cond_estimate_lower_pp_c <<- c(meta_sex_cond_estimate_lower_pp_c, mean(pp_c_results$b_sex_cond_lower))
    } else {
      meta_base_estimate_lower_pp_c <<- c(meta_base_estimate_lower_pp_c, mean(pp_c_results$b_base_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_lower_pp_c <<- c(meta_sex_estimate_lower_pp_c, mean(pp_c_results$b_sex_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_lower_pp_c <<- c(meta_cond_estimate_lower_pp_c, mean(pp_c_results$b_cond_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_lower_pp_c <<- c(meta_sex_cond_estimate_lower_pp_c, mean(pp_c_results$b_sex_cond_lower[pp_c_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_estimate_lower_pp_c_pb <<- c(meta_base_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_base_lower))
      meta_sex_estimate_lower_pp_c_pb <<- c(meta_sex_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_sex_lower))
      meta_cond_estimate_lower_pp_c_pb <<- c(meta_cond_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_cond_lower))
      meta_sex_cond_estimate_lower_pp_c_pb <<- c(meta_sex_cond_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_lower))
    } else {
      meta_base_estimate_lower_pp_c_pb <<- c(meta_base_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_base_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_estimate_lower_pp_c_pb <<- c(meta_sex_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_sex_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_estimate_lower_pp_c_pb <<- c(meta_cond_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_cond_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_estimate_lower_pp_c_pb <<- c(meta_sex_cond_estimate_lower_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
    }
    
  
    # uncertainty
    if (pp_final_expt_only == FALSE) {
      meta_base_uncertainty_pp_l <<- c(meta_base_uncertainty_pp_l, mean(pp_l_results$b_base_upper - pp_l_results$b_base_lower))
      meta_sex_uncertainty_pp_l <<- c(meta_sex_uncertainty_pp_l, mean(pp_l_results$b_sex_upper - pp_l_results$b_sex_lower))
      meta_cond_uncertainty_pp_l <<- c(meta_cond_uncertainty_pp_l, mean(pp_l_results$b_cond_upper - pp_l_results$b_cond_lower))
      meta_sex_cond_uncertainty_pp_l <<- c(meta_sex_cond_uncertainty_pp_l, mean(pp_l_results$b_sex_cond_upper - pp_l_results$b_sex_cond_lower))
    } else {
      meta_base_uncertainty_pp_l <<- c(meta_base_uncertainty_pp_l, mean(pp_l_results$b_base_upper[pp_l_results$expt == n_experiments_per_repeat] - pp_l_results$b_base_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_uncertainty_pp_l <<- c(meta_sex_uncertainty_pp_l, mean(pp_l_results$b_sex_upper[pp_l_results$expt == n_experiments_per_repeat] - pp_l_results$b_sex_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_cond_uncertainty_pp_l <<- c(meta_cond_uncertainty_pp_l, mean(pp_l_results$b_cond_upper[pp_l_results$expt == n_experiments_per_repeat] - pp_l_results$b_cond_lower[pp_l_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_uncertainty_pp_l <<- c(meta_sex_cond_uncertainty_pp_l, mean(pp_l_results$b_sex_cond_upper[pp_l_results$expt == n_experiments_per_repeat] - pp_l_results$b_sex_cond_lower[pp_l_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_uncertainty_pp_l_pb <<- c(meta_base_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_base_upper - pp_l_pb_results$b_base_lower))
      meta_sex_uncertainty_pp_l_pb <<- c(meta_sex_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_sex_upper - pp_l_pb_results$b_sex_lower))
      meta_cond_uncertainty_pp_l_pb <<- c(meta_cond_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_cond_upper - pp_l_pb_results$b_cond_lower))
      meta_sex_cond_uncertainty_pp_l_pb <<- c(meta_sex_cond_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_upper - pp_l_pb_results$b_sex_cond_lower))
    } else {
      meta_base_uncertainty_pp_l_pb <<- c(meta_base_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_base_upper[pp_l_pb_results$expt == n_experiments_per_repeat] - pp_l_pb_results$b_base_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_uncertainty_pp_l_pb <<- c(meta_sex_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_sex_upper[pp_l_pb_results$expt == n_experiments_per_repeat] - pp_l_pb_results$b_sex_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_uncertainty_pp_l_pb <<- c(meta_cond_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_cond_upper[pp_l_pb_results$expt == n_experiments_per_repeat] - pp_l_pb_results$b_cond_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_uncertainty_pp_l_pb <<- c(meta_sex_cond_uncertainty_pp_l_pb, mean(pp_l_pb_results$b_sex_cond_upper[pp_l_pb_results$expt == n_experiments_per_repeat] - pp_l_pb_results$b_sex_cond_lower[pp_l_pb_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_uncertainty_pp_c <<- c(meta_base_uncertainty_pp_c, mean(pp_c_results$b_base_upper - pp_c_results$b_base_lower))
      meta_sex_uncertainty_pp_c <<- c(meta_sex_uncertainty_pp_c, mean(pp_c_results$b_sex_upper - pp_c_results$b_sex_lower))
      meta_cond_uncertainty_pp_c <<- c(meta_cond_uncertainty_pp_c, mean(pp_c_results$b_cond_upper - pp_c_results$b_cond_lower))
      meta_sex_cond_uncertainty_pp_c <<- c(meta_sex_cond_uncertainty_pp_c, mean(pp_c_results$b_sex_cond_upper - pp_c_results$b_sex_cond_lower))
    } else {
      meta_base_uncertainty_pp_c <<- c(meta_base_uncertainty_pp_c, mean(pp_c_results$b_base_upper[pp_c_results$expt == n_experiments_per_repeat] - pp_c_results$b_base_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_uncertainty_pp_c <<- c(meta_sex_uncertainty_pp_c, mean(pp_c_results$b_sex_upper[pp_c_results$expt == n_experiments_per_repeat] - pp_c_results$b_sex_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_cond_uncertainty_pp_c <<- c(meta_cond_uncertainty_pp_c, mean(pp_c_results$b_cond_upper[pp_c_results$expt == n_experiments_per_repeat] - pp_c_results$b_cond_lower[pp_c_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_uncertainty_pp_c <<- c(meta_sex_cond_uncertainty_pp_c, mean(pp_c_results$b_sex_cond_upper[pp_c_results$expt == n_experiments_per_repeat] - pp_c_results$b_sex_cond_lower[pp_c_results$expt == n_experiments_per_repeat]))
    }
    
    if (pp_final_expt_only == FALSE) {
      meta_base_uncertainty_pp_c_pb <<- c(meta_base_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_base_upper - pp_c_pb_results$b_base_lower))
      meta_sex_uncertainty_pp_c_pb <<- c(meta_sex_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_sex_upper - pp_c_pb_results$b_sex_lower))
      meta_cond_uncertainty_pp_c_pb <<- c(meta_cond_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_cond_upper - pp_c_pb_results$b_cond_lower))
      meta_sex_cond_uncertainty_pp_c_pb <<- c(meta_sex_cond_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_upper - pp_c_pb_results$b_sex_cond_lower))
    } else {
      meta_base_uncertainty_pp_c_pb <<- c(meta_base_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_base_upper[pp_c_pb_results$expt == n_experiments_per_repeat] - pp_c_pb_results$b_base_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_uncertainty_pp_c_pb <<- c(meta_sex_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_sex_upper[pp_c_pb_results$expt == n_experiments_per_repeat] - pp_c_pb_results$b_sex_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_cond_uncertainty_pp_c_pb <<- c(meta_cond_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_cond_upper[pp_c_pb_results$expt == n_experiments_per_repeat] - pp_c_pb_results$b_cond_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
      meta_sex_cond_uncertainty_pp_c_pb <<- c(meta_sex_cond_uncertainty_pp_c_pb, mean(pp_c_pb_results$b_sex_cond_upper[pp_c_pb_results$expt == n_experiments_per_repeat] - pp_c_pb_results$b_sex_cond_lower[pp_c_pb_results$expt == n_experiments_per_repeat]))
    }
    
  }
}

compile_meta_results <- function() {
  meta_base_estimate_pp_l_p <- exp(meta_base_estimate_pp_l)/(1+exp(meta_base_estimate_pp_l))
  meta_sex_estimate_pp_l_p <- exp(meta_sex_estimate_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_sex_estimate_pp_l+meta_base_estimate_pp_l)) - meta_base_estimate_pp_l_p
  meta_cond_estimate_pp_l_p <- exp(meta_cond_estimate_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_cond_estimate_pp_l+meta_base_estimate_pp_l)) - meta_base_estimate_pp_l_p
  meta_sex_cond_estimate_pp_l_p <- exp(meta_sex_cond_estimate_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l)/(1+exp(meta_sex_cond_estimate_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l)) - (meta_base_estimate_pp_l_p + meta_sex_estimate_pp_l_p + meta_cond_estimate_pp_l_p)
  
  meta_base_estimate_pp_l_pb_p <- exp(meta_base_estimate_pp_l_pb)/(1+exp(meta_base_estimate_pp_l_pb))
  meta_sex_estimate_pp_l_pb_p <- exp(meta_sex_estimate_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_estimate_pp_l_pb+meta_base_estimate_pp_l_pb)) - meta_base_estimate_pp_l_pb_p
  meta_cond_estimate_pp_l_pb_p <- exp(meta_cond_estimate_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_cond_estimate_pp_l_pb+meta_base_estimate_pp_l_pb)) - meta_base_estimate_pp_l_pb_p
  meta_sex_cond_estimate_pp_l_pb_p <- exp(meta_sex_cond_estimate_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb + meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_cond_estimate_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb + meta_base_estimate_pp_l_pb)) - (meta_base_estimate_pp_l_pb_p + meta_sex_estimate_pp_l_pb_p + meta_cond_estimate_pp_l_pb_p)
  
  meta_base_estimate_pp_c_p <- exp(meta_base_estimate_pp_c)/(1+exp(meta_base_estimate_pp_c))
  meta_sex_estimate_pp_c_p <- exp(meta_sex_estimate_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_sex_estimate_pp_c+meta_base_estimate_pp_c)) - meta_base_estimate_pp_c_p
  meta_cond_estimate_pp_c_p <- exp(meta_cond_estimate_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_cond_estimate_pp_c+meta_base_estimate_pp_c)) - meta_base_estimate_pp_c_p
  meta_sex_cond_estimate_pp_c_p <- exp(meta_sex_cond_estimate_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c)/(1+exp(meta_sex_cond_estimate_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c)) - (meta_base_estimate_pp_c_p + meta_sex_estimate_pp_c_p + meta_cond_estimate_pp_c_p)
  
  meta_base_estimate_pp_c_pb_p <- exp(meta_base_estimate_pp_c_pb)/(1+exp(meta_base_estimate_pp_c_pb))
  meta_sex_estimate_pp_c_pb_p <- exp(meta_sex_estimate_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_estimate_pp_c_pb+meta_base_estimate_pp_c_pb)) - meta_base_estimate_pp_c_pb_p
  meta_cond_estimate_pp_c_pb_p <- exp(meta_cond_estimate_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_cond_estimate_pp_c_pb+meta_base_estimate_pp_c_pb)) - meta_base_estimate_pp_c_pb_p
  meta_sex_cond_estimate_pp_c_pb_p <- exp(meta_sex_cond_estimate_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb + meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_cond_estimate_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb + meta_base_estimate_pp_c_pb)) - (meta_base_estimate_pp_c_pb_p + meta_sex_estimate_pp_c_pb_p + meta_cond_estimate_pp_c_pb_p)
  
  
  meta_base_uncertainty_pp_l_p <- exp(meta_base_estimate_upper_pp_l)/(1+exp(meta_base_estimate_upper_pp_l)) - exp(meta_base_estimate_lower_pp_l)/(1+exp(meta_base_estimate_lower_pp_l))
  meta_sex_uncertainty_pp_l_p <- exp(meta_sex_estimate_upper_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_sex_estimate_upper_pp_l+meta_base_estimate_pp_l)) - exp(meta_sex_estimate_lower_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_sex_estimate_lower_pp_l+meta_base_estimate_pp_l))
  meta_cond_uncertainty_pp_l_p <- exp(meta_cond_estimate_upper_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_cond_estimate_upper_pp_l+meta_base_estimate_pp_l)) - exp(meta_cond_estimate_lower_pp_l+meta_base_estimate_pp_l)/(1+exp(meta_cond_estimate_lower_pp_l+meta_base_estimate_pp_l))
  meta_sex_cond_uncertainty_pp_l_p <- exp(meta_sex_cond_estimate_upper_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l +meta_base_estimate_pp_l)/(1+exp(meta_sex_cond_estimate_upper_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l +meta_base_estimate_pp_l)) - exp(meta_sex_cond_estimate_lower_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l +meta_base_estimate_pp_l)/(1+exp(meta_sex_cond_estimate_lower_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l +meta_base_estimate_pp_l))
  
  meta_base_uncertainty_pp_l_pb_p <- exp(meta_base_estimate_upper_pp_l_pb)/(1+exp(meta_base_estimate_upper_pp_l_pb)) - exp(meta_base_estimate_lower_pp_l_pb)/(1+exp(meta_base_estimate_lower_pp_l_pb))
  meta_sex_uncertainty_pp_l_pb_p <- exp(meta_sex_estimate_upper_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_estimate_upper_pp_l_pb+meta_base_estimate_pp_l_pb)) - exp(meta_sex_estimate_lower_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_estimate_lower_pp_l_pb+meta_base_estimate_pp_l_pb))
  meta_cond_uncertainty_pp_l_pb_p <- exp(meta_cond_estimate_upper_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_cond_estimate_upper_pp_l_pb+meta_base_estimate_pp_l_pb)) - exp(meta_cond_estimate_lower_pp_l_pb+meta_base_estimate_pp_l_pb)/(1+exp(meta_cond_estimate_lower_pp_l_pb+meta_base_estimate_pp_l_pb))
  meta_sex_cond_uncertainty_pp_l_pb_p <- exp(meta_sex_cond_estimate_upper_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb +meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_cond_estimate_upper_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb +meta_base_estimate_pp_l_pb)) - exp(meta_sex_cond_estimate_lower_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb +meta_base_estimate_pp_l_pb)/(1+exp(meta_sex_cond_estimate_lower_pp_l_pb + meta_sex_estimate_pp_l_pb + meta_cond_estimate_pp_l_pb +meta_base_estimate_pp_l_pb))
  
  meta_base_uncertainty_pp_c_p <- exp(meta_base_estimate_upper_pp_c)/(1+exp(meta_base_estimate_upper_pp_c)) - exp(meta_base_estimate_lower_pp_c)/(1+exp(meta_base_estimate_lower_pp_c))
  meta_sex_uncertainty_pp_c_p <- exp(meta_sex_estimate_upper_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_sex_estimate_upper_pp_c+meta_base_estimate_pp_c)) - exp(meta_sex_estimate_lower_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_sex_estimate_lower_pp_c+meta_base_estimate_pp_c))
  meta_cond_uncertainty_pp_c_p <- exp(meta_cond_estimate_upper_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_cond_estimate_upper_pp_c+meta_base_estimate_pp_c)) - exp(meta_cond_estimate_lower_pp_c+meta_base_estimate_pp_c)/(1+exp(meta_cond_estimate_lower_pp_c+meta_base_estimate_pp_c))
  meta_sex_cond_uncertainty_pp_c_p <- exp(meta_sex_cond_estimate_upper_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c +meta_base_estimate_pp_c)/(1+exp(meta_sex_cond_estimate_upper_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c +meta_base_estimate_pp_c)) - exp(meta_sex_cond_estimate_lower_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c +meta_base_estimate_pp_c)/(1+exp(meta_sex_cond_estimate_lower_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c +meta_base_estimate_pp_c))
  
  meta_base_uncertainty_pp_c_pb_p <- exp(meta_base_estimate_upper_pp_c_pb)/(1+exp(meta_base_estimate_upper_pp_c_pb)) - exp(meta_base_estimate_lower_pp_c_pb)/(1+exp(meta_base_estimate_lower_pp_c_pb))
  meta_sex_uncertainty_pp_c_pb_p <- exp(meta_sex_estimate_upper_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_estimate_upper_pp_c_pb+meta_base_estimate_pp_c_pb)) - exp(meta_sex_estimate_lower_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_estimate_lower_pp_c_pb+meta_base_estimate_pp_c_pb))
  meta_cond_uncertainty_pp_c_pb_p <- exp(meta_cond_estimate_upper_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_cond_estimate_upper_pp_c_pb+meta_base_estimate_pp_c_pb)) - exp(meta_cond_estimate_lower_pp_c_pb+meta_base_estimate_pp_c_pb)/(1+exp(meta_cond_estimate_lower_pp_c_pb+meta_base_estimate_pp_c_pb))
  meta_sex_cond_uncertainty_pp_c_pb_p <- exp(meta_sex_cond_estimate_upper_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb +meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_cond_estimate_upper_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb +meta_base_estimate_pp_c_pb)) - exp(meta_sex_cond_estimate_lower_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb +meta_base_estimate_pp_c_pb)/(1+exp(meta_sex_cond_estimate_lower_pp_c_pb + meta_sex_estimate_pp_c_pb + meta_cond_estimate_pp_c_pb +meta_base_estimate_pp_c_pb))
  
  return(data.frame(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
                    meta_n_trials_per_participant, meta_n_people,
                    meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
                    meta_base_estimate_pp_l, meta_sex_estimate_pp_l, meta_cond_estimate_pp_l, meta_sex_cond_estimate_pp_l,
                    meta_base_estimate_pp_l_pb, meta_sex_estimate_pp_l_pb, meta_cond_estimate_pp_l_pb, meta_sex_cond_estimate_pp_l_pb,
                    meta_base_estimate_pp_c, meta_sex_estimate_pp_c, meta_cond_estimate_pp_c, meta_sex_cond_estimate_pp_c,
                    meta_base_estimate_pp_c_pb, meta_sex_estimate_pp_c_pb, meta_cond_estimate_pp_c_pb, meta_sex_cond_estimate_pp_c_pb,
                    meta_base_estimate_pp_l_p, meta_sex_estimate_pp_l_p, meta_cond_estimate_pp_l_p, meta_sex_cond_estimate_pp_l_p,
                    meta_base_estimate_pp_l_pb_p, meta_sex_estimate_pp_l_pb_p, meta_cond_estimate_pp_l_pb_p, meta_sex_cond_estimate_pp_l_pb_p,
                    meta_base_estimate_pp_c_p, meta_sex_estimate_pp_c_p, meta_cond_estimate_pp_c_p, meta_sex_cond_estimate_pp_c_p,
                    meta_base_estimate_pp_c_pb_p, meta_sex_estimate_pp_c_pb_p, meta_cond_estimate_pp_c_pb_p, meta_sex_cond_estimate_pp_c_pb_p,
                    meta_base_estimate_lower_pp_l, meta_sex_estimate_lower_pp_l, meta_cond_estimate_lower_pp_l, meta_sex_cond_estimate_lower_pp_l,
                    meta_base_estimate_lower_pp_l_pb, meta_sex_estimate_lower_pp_l_pb, meta_cond_estimate_lower_pp_l_pb, meta_sex_cond_estimate_lower_pp_l_pb,
                    meta_base_estimate_lower_pp_c, meta_sex_estimate_lower_pp_c, meta_cond_estimate_lower_pp_c, meta_sex_cond_estimate_lower_pp_c,
                    meta_base_estimate_lower_pp_c_pb, meta_sex_estimate_lower_pp_c_pb, meta_cond_estimate_lower_pp_c_pb, meta_sex_cond_estimate_lower_pp_c_pb,
                    meta_base_estimate_upper_pp_l, meta_sex_estimate_upper_pp_l, meta_cond_estimate_upper_pp_l, meta_sex_cond_estimate_upper_pp_l,
                    meta_base_estimate_upper_pp_l_pb, meta_sex_estimate_upper_pp_l_pb, meta_cond_estimate_upper_pp_l_pb, meta_sex_cond_estimate_upper_pp_l_pb,
                    meta_base_estimate_upper_pp_c, meta_sex_estimate_upper_pp_c, meta_cond_estimate_upper_pp_c, meta_sex_cond_estimate_upper_pp_c,
                    meta_base_estimate_upper_pp_c_pb, meta_sex_estimate_upper_pp_c_pb, meta_cond_estimate_upper_pp_c_pb, meta_sex_cond_estimate_upper_pp_c_pb,
                    meta_sex_positive_rate_pp_l, meta_cond_positive_rate_pp_l, meta_sex_cond_positive_rate_pp_l,
                    meta_base_uncertainty_pp_l, meta_sex_uncertainty_pp_l, meta_cond_uncertainty_pp_l, meta_sex_cond_uncertainty_pp_l,
                    meta_base_uncertainty_pp_l_pb, meta_sex_uncertainty_pp_l_pb, meta_cond_uncertainty_pp_l_pb, meta_sex_cond_uncertainty_pp_l_pb,
                    meta_base_uncertainty_pp_c, meta_sex_uncertainty_pp_c, meta_cond_uncertainty_pp_c, meta_sex_cond_uncertainty_pp_c,
                    meta_base_uncertainty_pp_c_pb, meta_sex_uncertainty_pp_c_pb, meta_cond_uncertainty_pp_c_pb, meta_sex_cond_uncertainty_pp_c_pb,
                    meta_base_uncertainty_pp_l_p, meta_sex_uncertainty_pp_l_p, meta_cond_uncertainty_pp_l_p, meta_sex_cond_uncertainty_pp_l_p,
                    meta_base_uncertainty_pp_l_pb_p, meta_sex_uncertainty_pp_l_pb_p, meta_cond_uncertainty_pp_l_pb_p, meta_sex_cond_uncertainty_pp_l_pb_p,
                    meta_base_uncertainty_pp_c_p, meta_sex_uncertainty_pp_c_p, meta_cond_uncertainty_pp_c_p, meta_sex_cond_uncertainty_pp_c_p,
                    meta_base_uncertainty_pp_c_pb_p, meta_sex_uncertainty_pp_c_pb_p, meta_cond_uncertainty_pp_c_pb_p, meta_sex_cond_uncertainty_pp_c_pb_p))
}

tidy_workspace <- function() {
  rm(meta_n_repeats,meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
     meta_n_trials_per_participant, meta_n_people,
     meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond, 
     meta_base_estimate_pp_l, meta_sex_estimate_pp_l, meta_cond_estimate_pp_l, meta_sex_cond_estimate_pp_l,
     meta_base_estimate_pp_l_pb, meta_sex_estimate_pp_l_pb, meta_cond_estimate_pp_l_pb, meta_sex_cond_estimate_pp_l_pb,
     meta_base_estimate_pp_c, meta_sex_estimate_pp_c, meta_cond_estimate_pp_c, meta_sex_cond_estimate_pp_c,
     meta_base_estimate_pp_c_pb, meta_sex_estimate_pp_c_pb, meta_cond_estimate_pp_c_pb, meta_sex_cond_estimate_pp_c_pb,
     meta_base_estimate_pp_l_p, meta_sex_estimate_pp_l_p, meta_cond_estimate_pp_l_p, meta_sex_cond_estimate_pp_l_p,
     meta_base_estimate_pp_l_pb_p, meta_sex_estimate_pp_l_pb_p, meta_cond_estimate_pp_l_pb_p, meta_sex_cond_estimate_pp_l_pb_p,
     meta_base_estimate_pp_c_p, meta_sex_estimate_pp_c_p, meta_cond_estimate_pp_c_p, meta_sex_cond_estimate_pp_c_p,
     meta_base_estimate_pp_c_pb_p, meta_sex_estimate_pp_c_pb_p, meta_cond_estimate_pp_c_pb_p, meta_sex_cond_estimate_pp_c_pb_p,
     meta_base_estimate_lower_pp_l, meta_sex_estimate_lower_pp_l, meta_cond_estimate_lower_pp_l, meta_sex_cond_estimate_lower_pp_l,
     meta_base_estimate_lower_pp_l_pb, meta_sex_estimate_lower_pp_l_pb, meta_cond_estimate_lower_pp_l_pb, meta_sex_cond_estimate_lower_pp_l_pb,
     meta_base_estimate_lower_pp_c, meta_sex_estimate_lower_pp_c, meta_cond_estimate_lower_pp_c, meta_sex_cond_estimate_lower_pp_c,
     meta_base_estimate_lower_pp_c_pb, meta_sex_estimate_lower_pp_c_pb, meta_cond_estimate_lower_pp_c_pb, meta_sex_cond_estimate_lower_pp_c_pb,
     meta_base_estimate_upper_pp_l, meta_sex_estimate_upper_pp_l, meta_cond_estimate_upper_pp_l, meta_sex_cond_estimate_upper_pp_l,
     meta_base_estimate_upper_pp_l_pb, meta_sex_estimate_upper_pp_l_pb, meta_cond_estimate_upper_pp_l_pb, meta_sex_cond_estimate_upper_pp_l_pb,
     meta_base_estimate_upper_pp_c, meta_sex_estimate_upper_pp_c, meta_cond_estimate_upper_pp_c, meta_sex_cond_estimate_upper_pp_c,
     meta_base_estimate_upper_pp_c_pb, meta_sex_estimate_upper_pp_c_pb, meta_cond_estimate_upper_pp_c_pb, meta_sex_cond_estimate_upper_pp_c_pb,
     meta_sex_positive_rate_pp_l, meta_cond_positive_rate_pp_l, meta_sex_cond_positive_rate_pp_l,
     meta_base_uncertainty_pp_l, meta_sex_uncertainty_pp_l, meta_cond_uncertainty_pp_l, meta_sex_cond_uncertainty_pp_l,
     meta_base_uncertainty_pp_l_pb, meta_sex_uncertainty_pp_l_pb, meta_cond_uncertainty_pp_l_pb, meta_sex_cond_uncertainty_pp_l_pb,
     meta_base_uncertainty_pp_c, meta_sex_uncertainty_pp_c, meta_cond_uncertainty_pp_c, meta_sex_cond_uncertainty_pp_c,
     meta_base_uncertainty_pp_c_pb, meta_sex_uncertainty_pp_c_pb, meta_cond_uncertainty_pp_c_pb, meta_sex_cond_uncertainty_pp_c_pb,
     meta_base_uncertainty_pp_l_p, meta_sex_uncertainty_pp_l_p, meta_cond_uncertainty_pp_l_p, meta_sex_cond_uncertainty_pp_l_p,
     meta_base_uncertainty_pp_l_pb_p, meta_sex_uncertainty_pp_l_pb_p, meta_cond_uncertainty_pp_l_pb_p, meta_sex_cond_uncertainty_pp_l_pb_p,
     meta_base_uncertainty_pp_c_p, meta_sex_uncertainty_pp_c_p, meta_cond_uncertainty_pp_c_p, meta_sex_cond_uncertainty_pp_c_p,
     meta_base_uncertainty_pp_c_pb_p, meta_sex_uncertainty_pp_c_pb_p, meta_cond_uncertainty_pp_c_pb_p, meta_sex_cond_uncertainty_pp_c_pb_p,
     b_base, b_bases, b_cond, b_conds, b_sex, b_sexs, b_sex_cond, b_sex_conds,
     pos = ".GlobalEnv")
}

prepare_for_simulation <- function() {
  b_base <<- b_bases[i]
  b_sex <<- b_sexs[j]
  b_cond <<- b_conds[k]
  b_sex_cond <<- b_sex_conds[l]
  meta_true_base <<- c(meta_true_base, rep(b_base, n_repeats))
  meta_true_sex <<- c(meta_true_sex, rep(b_sex, n_repeats))
  meta_true_cond <<- c(meta_true_cond, rep(b_cond, n_repeats))
  meta_true_sex_cond <<- c(meta_true_sex_cond, rep(b_sex_cond, n_repeats))
  print(paste("running simulation with parameters: b_base: ", b_base,
              ", b_sex: ", b_sex,
              ", b_cond: ", b_cond,
              ", b_sex_cond: ", b_sex_cond,
              sep=" "))
}


tidy_chain <- function(edges, edge){
  new_edge <- edge
  for (i in 1:nrow(edge)){
    cite <- edges %>% filter(to == edge$from[i]) #filter all the studies the node cites
    if(nrow(cite)>0){ #if the cited study cites anyone
      for (c in 1:nrow(cite)){
        if(cite$from[c] %in% edge$from){ #if a cited study cites the same as the node
          new_edge <- new_edge[!(new_edge$from == cite$from[c]),] #remove the study it cites
        }
      }
    }
  }
  return(new_edge)
}
