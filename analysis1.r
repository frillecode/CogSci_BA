do_analyses_1 <- function() {
  print(">>>>>>>> Doing analyses")

  if (do_pp_linear == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing on linear chain")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp_linear")
    
    #set initial prior for beta[4]
    pp_u <- 0
    pp_sig <- 0.1
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      #update prior in prior_string
      x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
      
      #model formula
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- c(
        prior(normal(0.5, 0.1), class = Intercept), 
        prior_string(x, class = "b", coef = "sex:condition"),
        prior(normal(0,0.1), class = b, coef = "sex"),
        prior(normal(0,0.1), class = b, coef = "condition"),
        prior(normal(0,0.1), class = sd),
        prior(normal(0.5,0.1), class = sigma)
      )
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the pp
      pb_true <<- c(pb_true, 0)
      
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_base_error <<- c(b_base_error, fixef(model)[,2][[1]])
      b_sex_error <<- c(b_sex_error, fixef(model)[,2][[2]])
      b_cond_error <<- c(b_cond_error, fixef(model)[,2][[3]])
      b_sex_cond_error <<- c(b_sex_cond_error, fixef(model)[,2][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
      
      # update the priors for the next run
      pp_u <- fixef(model)[,1][[4]]
      pp_sig <- fixef(model)[,2][[4]] 
      pp_true <<- c(pp_true, 1)
      
    } # end of for each experiment loop
  }# end of do pp_linear
   
  if (do_pp_citation == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing on citation chain")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp_citation")
    
    # empty df to save data in each iteration
    empty_df <- as.data.frame(matrix(0,nrow = 0, ncol = 4))  #måske problematisk med nrow..... ?????????
    
    # loop
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # set initial prior for beta[4] and update afterwards (if not first round, update pp_u and pp_sig)
      if(nrow(empty_df) == 0){
        pp_u <- 0
        pp_sig <- 0.1
        
      }else{
        this_edgelist <- edgelist %>% filter(edgelist$to == this_data_set$studyID[1])
        
        if(nrow(this_edgelist) > 0){
          this_empty_df <- empty_df %>% filter(this_edgelist$from %in% empty_df$studyID)
          pp_u <- mean(this_empty_df$pp_u)
          pp_sig <- mean(this_empty_df$pp_sig)
          
        }else{
          pp_u <- 0
          pp_sig <- 0.1
        }
      }
      
      # update prior in prior_string
      x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
      
      # model formula
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- c(
        prior(normal(0.5, 0.1), class = Intercept), 
        prior_string(x, class = "b", coef = "sex:condition"),
        prior(normal(0,0.1), class = b, coef = "sex"),
        prior(normal(0,0.1), class = b, coef = "condition"),
        prior(normal(0,0.1), class = sd),
        prior(normal(0.5,0.1), class = sigma)
      )
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the pp
      pb_true <<- c(pb_true, 0)
      
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_base_error <<- c(b_base_error, fixef(model)[,2][[1]])
      b_sex_error <<- c(b_sex_error, fixef(model)[,2][[2]])
      b_cond_error <<- c(b_cond_error, fixef(model)[,2][[3]])
      b_sex_cond_error <<- c(b_sex_cond_error, fixef(model)[,2][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
      
      # new dataframe with saved PP values 
      
      empty_df$pp_u <- fixef(model)[,1][[4]]
      empty_df$pp_sig <- fixef(model)[,2][[4]] 
      empty_df$pp_true <<- c(pp_true, 1)
      empty_df$studyID <- this_data_set$studyID
      
    } # end of for each experiment loop
  }# end of do pp_citation
  
  if (publication_bias_sym == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing with pb (sym)")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp")
    
    #set initial prior for beta[4]
    pp_u <- 0
    pp_sig <- 0.1
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      #update prior in prior_string
      x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
      
      #model formula
      model_f <- bf(response ~ sex * condition + (1|participant_id))
      
      prior_m <- c(
        prior(normal(0.5, 0.1), class = Intercept), 
        prior_string(x, class = "b", coef = "sex:condition"),
        prior(normal(0,0.1), class = b, coef = "sex"),
        prior(normal(0,0.1), class = b, coef = "condition"),
        prior(normal(0,0.1), class = sd),
        prior(normal(0.5,0.1), class = sigma)
      )
      
      model <- brm(
        formula = model_f,
        data = this_data_set,
        family = gaussian,
        prior = prior_m,
        sample_prior = T,
        chains = 2,
        cores = 2
      )
      
      # save the results of the pp
      pb_true <<- c(pb_true, 1)
      
      b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
      b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
      b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
      
      b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
      b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
      b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
      b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
      
      b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
      b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
      b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
      b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
      
      b_base_error <<- c(b_base_error, fixef(model)[,2][[1]])
      b_sex_error <<- c(b_sex_error, fixef(model)[,2][[2]])
      b_cond_error <<- c(b_cond_error, fixef(model)[,2][[3]])
      b_sex_cond_error <<- c(b_sex_cond_error, fixef(model)[,2][[4]])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
      
      # update the priors for the next run
      max_distance <- ifelse(b_sex_cond == 0, 0.05, (boot::inv.logit(b_sex_cond) - boot::inv.logit(b_base))*2)
      
      pb_prob <- ifelse(abs(fixef(model)[,1][[4]]) >= max_distance, 
                        1, 
                        abs(fixef(model)[,1][[4]]/max_distance))
      pb <- rbinom(1, size = 1, prob = pb_prob)
      pp_u <- ifelse(pb == 1,
                     fixef(model)[,1][[4]],
                     pp_u[1])
      pp_sig <- ifelse(pb ==1, 
                       fixef(model)[,2][[4]],
                       pp_sig[1])
      pp_true <<- c(pp_true, ifelse(pb == 0, 0, 1))
      
    } # end of for each experiment loop
  }# end of do pp_pb
}

if (publication_bias_asym == TRUE) {
  print(">>>>>>>>>>>> Doing posterior passing with pb (asym)")
  # prep columns
  # this function is in util.R
  save_analysis_results_1("pp")
  
  #set initial prior for beta[4]
  pp_u <- 0
  pp_sig <- 0.1
  
  for (experiment in 1:n_experiments_per_repeat) {
    # for every experiment get the relevant data set
    this_data_set <- data_sets[data_sets$data_set == experiment,]
    
    #update prior in prior_string
    x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
    
    #model formula
    model_f <- bf(response ~ sex * condition + (1|participant_id))
    
    prior_m <- c(
      prior(normal(0.5, 0.1), class = Intercept), 
      prior_string(x, class = "b", coef = "sex:condition"),
      prior(normal(0,0.1), class = b, coef = "sex"),
      prior(normal(0,0.1), class = b, coef = "condition"),
      prior(normal(0,0.1), class = sd),
      prior(normal(0.5,0.1), class = sigma)
    )
    
    model <- brm(
      formula = model_f,
      data = this_data_set,
      family = gaussian,
      prior = prior_m,
      sample_prior = T,
      chains = 2,
      cores = 2
    )
    
    # save the results of the pp
    pb_true <<- c(pb_true, 1)
    
    b_base_med <<- c(b_base_med, fixef(model)[,1][[1]])
    b_sex_med <<- c(b_sex_med, fixef(model)[,1][[2]])
    b_cond_med <<- c(b_cond_med, fixef(model)[,1][[3]])
    b_sex_cond_med <<- c(b_sex_cond_med, fixef(model)[,1][[4]])
    
    b_base_lower <<- c(b_base_lower, fixef(model)[,3][[1]])
    b_sex_lower <<- c(b_sex_lower, fixef(model)[,3][[2]])
    b_cond_lower <<- c(b_cond_lower, fixef(model)[,3][[3]])
    b_sex_cond_lower <<- c(b_sex_cond_lower, fixef(model)[,3][[4]])
    
    b_base_upper <<- c(b_base_upper, fixef(model)[,4][[1]])
    b_sex_upper <<- c(b_sex_upper, fixef(model)[,4][[2]])
    b_cond_upper <<- c(b_cond_upper, fixef(model)[,4][[3]])
    b_sex_cond_upper <<- c(b_sex_cond_upper, fixef(model)[,4][[4]])
    
    b_base_error <<- c(b_base_error, fixef(model)[,2][[1]])
    b_sex_error <<- c(b_sex_error, fixef(model)[,2][[2]])
    b_cond_error <<- c(b_cond_error, fixef(model)[,2][[3]])
    b_sex_cond_error <<- c(b_sex_cond_error, fixef(model)[,2][[4]])
    
    b_sex_p_value <<- c(b_sex_p_value, NaN)
    b_cond_p_value <<- c(b_cond_p_value, NaN)
    b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
    
    # update the priors for the next run
    max_distance <- ifelse(b_sex_cond == 0, 0.05, (boot::inv.logit(b_sex_cond) - boot::inv.logit(b_base))*2)
    
    pb_prob <- ifelse(abs(fixef(model)[,1][[4]]) >= max_distance, 
                      1, 
                      abs(fixef(model)[,1][[4]]/max_distance))
    pb <- rbinom(1, size = 1, prob = pb_prob)
    pp_u <- ifelse(pb == 1,
                   fixef(model)[,1][[4]],
                   pp_u[1])
    pp_sig <- ifelse(pb ==1, 
                     fixef(model)[,2][[4]],
                     pp_sig[1])
    pp_true <<- c(pp_true, ifelse(pb == 0, 0, 1))
    
  } # end of for each experiment loop
}# end of do pp_pb




do_meta_analysis <- function() {
  #define vectors
  meta_repeat_id <- vector()
  n_exp <- vector()
  true_effect <- vector()
  pb_true <- vector()
  
  b_sex_cond_meta <- vector()
  b_sex_cond_lower_meta <- vector()
  b_sex_cond_upper_meta <- vector()
  b_sex_cond_error_meta <- vector()
  
  #only for bskep
  meta_data <- saved_results[saved_results$analysis_type == "bskep",]         ####### OBS
  
  for (rep in 1:n_repeats) {
    #one meta analysis pr repeat
    this_meta_data <- meta_data[meta_data$repeat_id == rep,]
    
    #do meta analysis
    meta_f <- bf(b_sex_cond_med | se(b_sex_cond_error) ~ 1 + (1 | expt))
    
    prior_meta <- c(
      prior(normal(0,0.1), class = Intercept),
      prior(normal(0,0.1), class = sd)
    )
    
    model <- brm(
      formula = meta_f,
      data = this_meta_data,
      family = gaussian,
      prior = prior_meta,
      sample_prior = T,
      chains = 2,
      cores =2
    )
    
    #save results
    b_sex_cond_meta <- c(b_sex_cond_meta, fixef(model)[,1][[1]])
    
    b_sex_cond_error_meta <- c(b_sex_cond_upper_meta, fixef(model)[,2][[1]])
    
    b_sex_cond_lower_meta <- c(b_sex_cond_lower_meta, fixef(model)[,3][[1]])
    
    b_sex_cond_upper_meta <- c(b_sex_cond_upper_meta, fixef(model)[,4][[1]])
    
    meta_repeat_id <- c(meta_repeat_id, rep)
    
    n_exp <- c(n_exp, length(this_meta_data$expt))
    
    true_effect <- c(true_effect, this_meta_data$true_sex_cond[1])
    
    pb_true <- c(pb_true, 0)
  }
  
  if(publication_bias_meta == TRUE){
    for (rep in 1:n_repeats) {
      #one meta analysis pr repeat
      this_meta_data <- meta_data[meta_data$repeat_id == rep,]
      
      #publication bias
      max_distance <-  ifelse(b_sex_cond == 0, 0.05, (boot::inv.logit(b_sex_cond) - boot::inv.logit(b_base))*2)
      
      for (exp in 1:n_experiments_per_repeat){
        pb_prob <- ifelse(abs(this_meta_data[this_meta_data$expt == exp,]$b_sex_cond_med) >= max_distance, 
                          1, 
                          abs(this_meta_data[this_meta_data$expt == exp,]$b_sex_cond_med/max_distance))
        pb <- rbinom(1, size = 1, prob = pb_prob)
        if(pb == 0){
          this_meta_data <- this_meta_data %>% 
            filter(!(expt == exp))
        }
      }
      
      #do meta analysis
      meta_f <- bf(b_sex_cond_med | se(b_sex_cond_error) ~ 1 + (1 | expt))
      
      prior_meta <- c(
        prior(normal(0,0.1), class = Intercept),
        prior(normal(0,0.1), class = sd)
      )
      
      model <- brm(
        formula = meta_f,
        data = this_meta_data,
        family = gaussian,
        prior = prior_meta,
        sample_prior = T,
        chains = 2,
        cores =2
      )
      
      #save results
      b_sex_cond_meta <- c(b_sex_cond_meta, fixef(model)[,1][[1]])
      
      b_sex_cond_error_meta <- c(b_sex_cond_upper_meta, fixef(model)[,2][[1]])
      
      b_sex_cond_lower_meta <- c(b_sex_cond_lower_meta, fixef(model)[,3][[1]])
      
      b_sex_cond_upper_meta <- c(b_sex_cond_upper_meta, fixef(model)[,4][[1]])
      
      meta_repeat_id <- c(meta_repeat_id, rep)
      
      n_exp <- c(n_exp, length(this_meta_data$expt))
      
      true_effect <- c(true_effect, this_meta_data$true_sex_cond[1])
      
      pb_true <- c(pb_true, 1)
    }
    
  }
  
  #save all results
  return(data.frame(meta_repeat_id, n_exp, true_effect, pb_true,
                    b_sex_cond_meta, b_sex_cond_lower_meta, b_sex_cond_upper_meta, b_sex_cond_error_meta))
  
}
