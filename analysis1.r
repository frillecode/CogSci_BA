do_analyses_1 <- function() {
  print(">>>>>>>> Doing analyses")

## LINEAR CHAIN ##
  if (do_pp_linear == TRUE) {
    
    ## WITHOUT PUBLICATION BIAS ##
    
    print(">>>>>>>>>>>> Doing posterior passing on linear chain (no pb)")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp_linear")
    pub_method <<- c(pub_method, rep(NA, n_experiments_per_repeat))
    
    #set initial prior for beta[4]
    pp_u <- 0
    pp_sig <- 0.1
    
    for (experiment in 1:n_experiments_per_repeat) {
      print(paste(">>>> Model", experiment, "for linear_no_pb, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # adding a meassure for number of posteriors included 
      if(experiment==1){
        pp_n <<- c(pp_n, 0)
      } else {
        pp_n <<- c(pp_n, 1)
      }
      
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
      pub_true <<- c(pub_true, 1)
      
      } # end of for each experiment loop
     # end of pp_linear without pb
    
    ## WITH PUBLICATION BIAS ##
    
    if (do_publication_bias == TRUE){
      print(">>>>>>>>>>>> Doing posterior passing on linear chain (sym pb)")
      # prep columns
      # this function is in util.R
      save_analysis_results_1("pp_linear_pb_sym")
      pub_method <<- c(pub_method, rep("sym", n_experiments_per_repeat))
      
      #set initial prior for beta[4]
      pp_u <- 0
      pp_sig <- 0.1
      
      for (experiment in 1:n_experiments_per_repeat) {
        print(paste(">>>> Model", experiment, "for linear_sym, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment,]
        
        # adding a meassure for number of posteriors included 
        if(pp_u == 0 & pp_sig == 0.1){
          pp_n <<- c(pp_n, 0)
        } else {
          pp_n <<- c(pp_n, 1)
        }
        
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
        
        if (fixef(model)[,1][[4]] > 0 & fixef(model)[,3][[4]] > 0){
          pb_prob <- pb_prob_pos
        } else if (fixef(model)[,1][[4]] < 0 & fixef(model)[,4][[4]] < 0){ 
          pb_prob <- pb_prob_pos
        } else {  # if ((fixef(model)[,1][[4]] >= 0 & fixef(model)[,3][[4]] <= 0) | (fixef(model)[,1][[4]] <= 0 & fixef(model)[,4][[4]] >= 0)) 
          pb_prob <- pb_prob_null
        } 
        
        pb <- rbinom(1, size = 1, prob = pb_prob)
        pp_u <- ifelse(pb == 1,
                       fixef(model)[,1][[4]],
                       pp_u[1])
        pp_sig <- ifelse(pb ==1,
                         fixef(model)[,2][[4]],
                         pp_sig[1])
        pub_true <<- c(pub_true, ifelse(pb == 0, 0, 1))
        
      } # end of p_linear_sym
      
        print(">>>>>>>>>>>> Doing posterior passing on linear chain (asym pb)")
        # prep columns
        # this function is in util.R
        save_analysis_results_1("pp_linear_pb_asym")
        pub_method <<- c(pub_method, rep("asym", n_experiments_per_repeat))
        
        #set initial prior for beta[4]
        pp_u <- 0
        pp_sig <- 0.1
        
        for (experiment in 1:n_experiments_per_repeat) {
          print(paste(">>>> Model", experiment, "for linear_asym, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
          # for every experiment get the relevant data set
          this_data_set <- data_sets[data_sets$data_set == experiment,]
          
          # adding a meassure for number of posteriors included 
          if(pp_u == 0 & pp_sig == 0.1){
            pp_n <<- c(pp_n, 0)
          } else {
            pp_n <<- c(pp_n, 1)
          }
          
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
          
          if (fixef(model)[,1][[4]] > 0 & fixef(model)[,3][[4]] > 0){
            pb_prob <- pb_prob_pos
          } else if (fixef(model)[,1][[4]] < 0 & fixef(model)[,4][[4]] < 0){ 
            pb_prob <- pb_prob_neg
          } else {  # if ((fixef(model)[,1][[4]] >= 0 & fixef(model)[,3][[4]] <= 0) | (fixef(model)[,1][[4]] <= 0 & fixef(model)[,4][[4]] >= 0)) 
            pb_prob <- pb_prob_null
          } 
          
          pb <- rbinom(1, size = 1, prob = pb_prob)
          pp_u <- ifelse(pb == 1,
                         fixef(model)[,1][[4]],
                         pp_u[1])
          pp_sig <- ifelse(pb ==1,
                           fixef(model)[,2][[4]],
                           pp_sig[1])
          pub_true <<- c(pub_true, ifelse(pb == 0, 0, 1))
          
        } # end of pp_linear_asym
    } # end of pp_linear with pb
  }# end of do_pp_linear

## CITATION CHAIN ##  
  if (do_pp_citation == TRUE) {
    
    ## WITHOUT PUBLCIATION BIAS ##
    
      print(">>>>>>>>>>>> Doing posterior passing on citation chain (no pb)")
      # prep columns
      # this function is in util.R
      save_analysis_results_1("pp_citation")
      pub_method <<- c(pub_method, rep(NA, n_experiments_per_repeat))
      
      # empty df to save data in each iteration
      chain_df <- as.data.frame(matrix(0,nrow = 0, ncol = 3))  
      colnames(chain_df) <-  c("pp_u", "pp_sig", "studyID")
      
      # loop
      for (experiment in 1:n_experiments_per_repeat) {
        print(paste(">>>> Model", experiment, "for citation_no_pb, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment,]
        this_data_set$studyID <- as.character(this_data_set$studyID)
        this_chain_df <- as.data.frame(matrix(0,nrow = 1, ncol = 3))  
        colnames(this_chain_df) <- colnames(chain_df)
        
        # set initial prior for beta[4] and update afterwards (if not first round, update pp_u and pp_sig)
        if(nrow(chain_df) == 0){
          pp_u <- 0
          pp_sig <- 0.1
          
          pp_n <<- c(pp_n, 0) 
          
        }else{
          this_citation_chain <- citation_chain %>% 
            filter(to == this_data_set$studyID[1]) %>% 
            filter(from %in% chain_df$studyID)
          
          if(!(nrow(this_citation_chain) == 0)){
            this_citation_chain <- tidy_chain(citation_chain, this_citation_chain) 
          }
          
          if(nrow(this_citation_chain) == 0){ # if no studies are cited
            pp_u <- 0
            pp_sig <- 0.1
            
            pp_n <<- c(pp_n, 0)
            
          }  else{ 
            this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)
            
            if (nrow(this_chain_df)==1){ # if one study is cited
              pp_u <- this_chain_df$pp_u
              pp_sig <- this_chain_df$pp_sig
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            } else{
              pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)
              
              pp_u <- pp[,1]
              pp_sig <- pp[,2]
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            }
            
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
        
        this_chain_df$pp_u <- fixef(model)[,1][[4]]
        this_chain_df$pp_sig <- fixef(model)[,2][[4]] 
        this_chain_df$studyID <- this_data_set$studyID[1]
        
        # binding into df with all PP values
        
        chain_df <- rbind(chain_df, this_chain_df)
        
        pub_true <<- c(pub_true, 1)
        
      } # end of for each experiment loop
     # end of pp_citation without pb
    
    ## WITH PUBLICATION BIAS ##
    
    if (do_publication_bias == TRUE){
      print(">>>>>>>>>>>> Doing posterior passing on citation chain (pb sym)")
      # prep columns
      # this function is in util.R
      save_analysis_results_1("pp_citation_pb_sym")
      pub_method <<- c(pub_method, rep("sym", n_experiments_per_repeat))
      
      # empty df to save data in each iteration
      chain_df <- as.data.frame(matrix(0,nrow = 0, ncol = 3))  
      colnames(chain_df) <-  c("pp_u", "pp_sig", "studyID")
      
      # loop
      for (experiment in 1:n_experiments_per_repeat) { 
        print(paste(">>>> Model", experiment, "for citation_sym, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment,]
        this_data_set$studyID <- as.character(this_data_set$studyID)
        this_chain_df <- as.data.frame(matrix(0,nrow = 1, ncol = 3))  
        colnames(this_chain_df) <- colnames(chain_df)
        
        # set initial prior for beta[4] and update afterwards (if not first round, update pp_u and pp_sig)
        if(nrow(chain_df) == 0){
          pp_u <- 0
          pp_sig <- 0.1
          
          pp_n <<- c(pp_n, 0) # adding a meassure for number of posteriors included
          
        }else{
          this_citation_chain <- citation_chain %>% 
            filter(to == this_data_set$studyID[1]) %>% 
            filter(from %in% chain_df$studyID)
          
          if(!(nrow(this_citation_chain) == 0)){
            this_citation_chain <- tidy_chain(citation_chain, this_citation_chain) 
          }
          
          if(nrow(this_citation_chain) == 0){
            pp_u <- 0
            pp_sig <- 0.1
            
            pp_n <<- c(pp_n, 0)
            
          }else{
            this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)
            
            if (nrow(this_chain_df)==1){ # if one study is cited
              pp_u <- this_chain_df$pp_u
              pp_sig <- this_chain_df$pp_sig
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            } else{
              pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)
              
              pp_u <- pp[,1]
              pp_sig <- pp[,2]
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            }
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
        
        # publication
        if (fixef(model)[,1][[4]] > 0 & fixef(model)[,3][[4]] > 0){
          pb_prob <- pb_prob_pos
        } else if (fixef(model)[,1][[4]] < 0 & fixef(model)[,4][[4]] < 0){ 
          pb_prob <- pb_prob_pos
        } else {  # if ((fixef(model)[,1][[4]] >= 0 & fixef(model)[,3][[4]] <= 0) | (fixef(model)[,1][[4]] <= 0 & fixef(model)[,4][[4]] >= 0)) 
          pb_prob <- pb_prob_null
        } 
        
        pb <- rbinom(1, size = 1, prob = pb_prob)
        
        # if published
        if (pb == 1){
          # new dataframe with saved PP values 
          this_chain_df$pp_u <- fixef(model)[,1][[4]]
          this_chain_df$pp_sig <- fixef(model)[,2][[4]] 
          this_chain_df$studyID <- this_data_set$studyID[1]
          
          # binding into df with all PP values
          chain_df <- rbind(chain_df, this_chain_df) 
        }
        pub_true <<- c(pub_true, ifelse(pb == 0, 0, 1))
        
      } # end of pp_citation_sym
      
      print(">>>>>>>>>>>> Doing posterior passing on citation chain (pb asym)")
      # prep columns
      # this function is in util.R
      save_analysis_results_1("pp_citation_pb_asym")
      pub_method <<- c(pub_method, rep("asym", n_experiments_per_repeat))
      
      # empty df to save data in each iteration
      chain_df <- as.data.frame(matrix(0,nrow = 0, ncol = 3))  
      colnames(chain_df) <-  c("pp_u", "pp_sig", "studyID")
      
      # loop
      for (experiment in 1:n_experiments_per_repeat) { 
        print(paste(">>>> Model", experiment, "for citation_asym, in repeat", rep, "with true effect", b_sex_cond, sep=" "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment,]
        this_data_set$studyID <- as.character(this_data_set$studyID)
        this_chain_df <- as.data.frame(matrix(0,nrow = 1, ncol = 3))  
        colnames(this_chain_df) <- colnames(chain_df)
        
        # set initial prior for beta[4] and update afterwards (if not first round, update pp_u and pp_sig)
        if(nrow(chain_df) == 0){
          pp_u <- 0
          pp_sig <- 0.1
          
          pp_n <<- c(pp_n, 0) # adding a meassure for number of posteriors included
          
        }else{
          this_citation_chain <- citation_chain %>% 
            filter(to == this_data_set$studyID[1]) %>% 
            filter(from %in% chain_df$studyID)
          
          if(!(nrow(this_citation_chain) == 0)){
            this_citation_chain <- tidy_chain(citation_chain, this_citation_chain) 
          }
          
          if(nrow(this_citation_chain) == 0){
            pp_u <- 0
            pp_sig <- 0.1
            
            pp_n <<- c(pp_n, 0)
            
          }else{
            this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)
            
            if (nrow(this_chain_df)==1){ # if one study is cited
              pp_u <- this_chain_df$pp_u
              pp_sig <- this_chain_df$pp_sig
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            } else{
              pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)
              
              pp_u <- pp[,1]
              pp_sig <- pp[,2]
              
              pp_n <<- c(pp_n, nrow(this_chain_df))
            }
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
        
        # publication
        if (fixef(model)[,1][[4]] > 0 & fixef(model)[,3][[4]] > 0){
          pb_prob <- pb_prob_pos
        } else if (fixef(model)[,1][[4]] < 0 & fixef(model)[,4][[4]] < 0){ 
          pb_prob <- pb_prob_neg
        } else {  # if ((fixef(model)[,1][[4]] >= 0 & fixef(model)[,3][[4]] <= 0) | (fixef(model)[,1][[4]] <= 0 & fixef(model)[,4][[4]] >= 0)) 
          pb_prob <- pb_prob_null
        } 
        
        pb <- rbinom(1, size = 1, prob = pb_prob)
        
        # if published
        if (pb == 1){
          # new dataframe with saved PP values 
          this_chain_df$pp_u <- fixef(model)[,1][[4]]
          this_chain_df$pp_sig <- fixef(model)[,2][[4]] 
          this_chain_df$studyID <- this_data_set$studyID[1]
          
          # binding into df with all PP values
          chain_df <- rbind(chain_df, this_chain_df) 
        }
        pub_true <<- c(pub_true, ifelse(pb == 0, 0, 1))
        
      } # end of pp_citation_asym
    } # end of pp_citation with pb
  } # end of do_pp_citation
  
} # end of do_analyses_1

kalman <- function(mean,sd){
  for (i in 1:length(mean)){
    if (i == 2){
      k <- sd[1]/(sd[1]+sd[2]) # kalman gain
      k_mean <- mean[1] + k*(mean[2]-mean[1]) # kalman mean
      k_sd <- sd[1]-(k*sd[1]) # kalman sd
    }
    if (i>2){
      k <- k_sd/(k_sd+sd[i])
      k_mean <- k_mean + k*(mean[i]-k_mean)
      k_sd <- k_sd-(k*k_sd)
    }
  }
  
  return(data.frame(
    mean = k_mean,
    sd = k_sd
  ))
}

