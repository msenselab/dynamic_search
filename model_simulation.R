source('dyn_data_analysis.R')
library(parallel)

#---------------------------------------#
#  model simulation of the expriments   #
#---------------------------------------#

# Read data for experiment 1 and extract the means into a data frame
e1_data_mean <-
  full_join(
    dat$exp1$mrt %>% filter(dyn == "Dynamic") %>% select(setsize, target, mmrt) %>%
      spread(target, mmrt),
    dat$exp1$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, mdprime, mbias)
  )

# Read data for experiment 1 and extract the standard errors into a data frame
e1_data_se <-
  full_join(
    dat$exp1$mrt %>% filter(dyn == "Dynamic") %>% select(setsize, target, sert) %>%
      spread(target, sert),
    dat$exp1$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, dse, cse)
  )

# Rename columns for standard error data frame to distinguish from means
names(e1_data_se)[2:3] <- c("Absent_se", "Present_se")

# combine mean and standard error into a single data frame
e1_data <- full_join(e1_data_mean, e1_data_se)

# Read data for experiment 2 and extract the means into a data frame
e2_data_mean <-
  full_join(
    dat$exp2$mrt %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, target, reward, mmrt) %>% spread(target, mmrt),
    dat$exp2$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, mdprime, reward, mbias)
  )

# Read data for experiment 2 and extract the standard errors into a data frame
e2_data_se <-
  full_join(
    dat$exp2$mrt %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, target, reward, sert) %>% spread(target, sert),
    dat$exp2$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, dse, reward, cse)
  )

# Rename columns for standard error data frame to distinguish from means
names(e2_data_se)[3:4] <- c("Absent_se", "Present_se")

# combine mean and standard error into a single data frame
e2_data <- full_join(e2_data_mean, e2_data_se)

# Read data for experiment 13and extract the means into a data frame
e3_data_mean <-
  full_join(
    dat$exp3$mrt %>% filter(dyn == "Dynamic") %>% select(setsize, target, mmrt) %>%
      spread(target, mmrt),
    dat$exp3$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, mdprime, mbias)
  )

# Read data for experiment 3 and extract the standard errors into a data frame
e3_data_se <-
  full_join(
    dat$exp3$mrt %>% filter(dyn == "Dynamic") %>% select(setsize, target, sert) %>%
      spread(target, sert),
    dat$exp3$sensitivity %>% filter(dyn == "Dynamic") %>% ungroup() %>%
      select(setsize, dse, cse)
  )

# Rename columns for standard error data frame to distinguish from means
names(e3_data_se)[2:3] <- c("Absent_se", "Present_se")

# combine mean and standard error into a single data frame
e3_data <- full_join(e3_data_mean, e3_data_se)


# ---- Functions ----
# parallel processing of estimates. 
parEstimate <- function(fun, para){
  no_cores <- min(detectCores() - 1, 10)
  print(no_cores)
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, {library(dplyr) })
  clusterExport(cl, c('simulate_trial', 'simulate_trial2', 'simulate_trial3', 
                      'simulate_experiment', 'fit_model', 'fit_model_e2', 
                      'e1_data', 'e2_data', 'e3_data', 'simulate_trial4', 'simulate_trial5', 'simulate_trial6'))
  
  t0 = proc.time()
  ll <- clusterMap(cl, fun, para)
  stopCluster(cl)
  print(proc.time()-t0)
  return(ll)
}

#' Simulate one trial of dynamic search experiment
#' This simple first attempt at modelling assumes that participants sample items in the search display randomly and respond
#' target present after sampling a target (or what they believe to be a target, see below about misperception) a single time,
#' while they respond target absent after samplinmg distractors enough times that the probability of sampling that many distractors
#' in a row, if a target is present, falls below a threshold. The model does allow for misperception. That is a partiicpant may
#' sample a distractor and think it is the target or sample the target and think it is a distractor.
#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param p_ds The subjective probability a participant assigns to randomly sampling a distractor (if target is present)
#' @param p_thresh The threshold of posterior probability (of target present) below which a participant decides to stop sampling and respond absent
#' @param dt_m Mean time spent per sampled item
#' @param dt_sd SD of normal distribution for time spent per sampled item
#' @param ndt Nondecision time
#' @param N_max Maximum number of samples, participant stops at this point regardless of the current p
#' @param p_fa Probability of misperceiving a distractor as a target (and responding present, note this is not exactly the FA rate)
#' @param p_miss Probability of misperceiving the target as a distractor (and updating posterior like for any other distractor)
#' @param p_prior Prior probability of target present
#' @return rt and participant response (1=present, 0=absent)
simulate_trial <-
  function(p_d,
           p_ds,
           p_thresh,
           dt_m,
           dt_sd = 0,
           ndt = 0,
           N_max = 200,
           p_fa = 0,
           p_miss = 0,
           p_prior = 0.5) {
    n <- 1
    
    p <- p_prior
    # Start with prior probability of 0.5 for
    
    tp <- runif(N_max) < p_d # Target present or not
    cp <- runif(N_max) # Correct/incorrect perception
    
    while (n < N_max & p > p_thresh) {
      n <- n + 1
      if (tp[n]) {
        # Sampled a distractor
        if (cp[n] > p_fa) {
          # Correctly perceived distractor as distractor
          p <- p * p_ds
        } else {
          # Incorrectly perceived distractor as target
          p <- 1
          break
        }
      } else {
        # Sampled the target
        if (cp[n] > p_miss) {
          # Correctly detected the target
          p <- 1
          break
        } else {
          # Missed the target
          p <- p * p_ds
        }
      }
    }
    rt <- sum(rnorm(n, dt_m, dt_sd))
    #  if (p<=p_thresh) { # only distractors, and the p-thresh reaches... (should based on the prior belief of P/A)
    #    p = runif(1) # randomly decide target absent or present
    #  }
    #  if (n>=N_max) {
    #    # same randome decision
    #    p = runif(1)
    #  }
    return(c(rt + ndt, p > 0.5))
  }

#' Simulate one trial of dynamic search experiment
#' In this version of the model participants do not necessarily respond target present as soon as they sample a target, but
#' instead accumulate evidence (log likelihood ratio) until they reach one of two boundaries. Sampling a target is strong evidence
#' for target present but not necessarily strong enough to push the evidence across the boundary the first time it happens (because
#' of the possiblity of misperception).
#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param p_ds The subjective probability a participant assigns to randomly sampling a distractor (if target is present)
#' @param p_thresh The threshold on posterior probability, beyond which the particpant decides to makes a response
#' @param dt_m Mean time spent per sampled item
#' @param dt_sd SD of normal distribution for time spent per sampled item
#' @param ndt Nondecision time
#' @param N_max Maximum number of samples, participant stops at this point regardless of the current p
#' @param p_fa Probability of misperceiving a distractor as a target (and responding present, note this is not exactly the FA rate)
#' @param p_miss Probability of misperceiving the target as a distractor (and updating posterior like for any other distractor)
#' @param p_prior Prior probability of target present
#' @return rt and participant response (1=present, 0=absent)
simulate_trial2 <-
  function(p_d,
           p_ds,
           p_thresh,
           dt_m,
           dt_sd = 0,
           noise_sd = 0,
           ndt = 0,
           N_max = Inf,
           p_fa = 0,
           p_miss = 0,
           p_prior = 0.5) {
    n <- 1
    
    lpo <-
      log(p_prior / (1 - p_prior))
    # lpo = log posterior odds, initialize to log prior odds
    rt <- 0
    
    llr_d <-
      log((p_ds * (1 - p_fa) + (1 - p_ds) * p_miss) / (1 - p_miss)) # Log likelihood ratio for perceiveing distractor when target present vs. absent
    llr_d_s <-
      log(p_ds) # Simplified log likelihood ratio assuming participant do not take into account their misperception rates
    llr_t <-
      min(log(((1 - p_ds) * (1 - p_miss) + p_ds * p_fa) / p_fa), 1000) # LLR for perceiving target when present vs. absent
    llr_t_s <- 1000
    lpo_thresh <- log(p_thresh / (1 - p_thresh))
    while (n < N_max & abs(lpo) < lpo_thresh) {
      rt <- rt + rnorm(1, dt_m, dt_sd)
      n <- n + 1
      if (runif(1) < p_d) {
        # Sampled a distractor
        if (runif(1) > p_fa) {
          # Correctly perceived distractor as distractor
          lpo <- lpo + llr_d + rnorm(1, 0, noise_sd)
        } else {
          # Incorrectly perceived distractor as target
          lpo <- lpo + llr_t + rnorm(1, 0, noise_sd)
        }
      } else {
        # Sampled the target
        if (runif(1) > p_miss) {
          # Correctly detected the target
          lpo <- lpo + llr_t + rnorm(1, 0, noise_sd)
        } else {
          # Missed the target
          lpo <- lpo + llr_d + rnorm(1, 0, noise_sd)
        }
      }
    }
    return(c(rt + ndt, lpo > 0))
  }

#' Simulate one trial of dynamic search experiment
#' In this version of the model accumulation of evidence for target present and target absent is independent.
#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param p_ds The subjective probability a participant assigns to randomly sampling a distractor (if target is present)
#' @param p_thresh The threshold of accumulated evidence for a target present decision
#' @param p_thresh The threshold of accumulated evidence for a target absent decision
#' @param dt_m Mean time spent per sampled item
#' @param dt_sd SD of normal distribution for time spent per sampled item
#' @param ndt Nondecision time
#' @param N_max Maximum number of samples, participant stops at this point regardless of the current p
#' @param p_fa Probability of misperceiving a distractor as a target (and responding present, note this is not exactly the FA rate)
#' @param p_miss Probability of misperceiving the target as a distractor (and updating posterior like for any other distractor)
#' @param p_prior Prior probability of target present
#' @return rt and participant response (1=present, 0=absent)
simulate_trial3 <-
  function(p_d,
           p_ds,
           p_thresh_p,
           p_thresh_a,
           dt_m,
           dt_sd = 0,
           noise_sd = 0,
           ndt = 0,
           N_max = Inf,
           p_fa = 0,
           p_miss = 0,
           p_prior = 0.5) {
    n <- 1
    
    lpo_p <- 0
    
    lpo_a <- 0
    
    rt <- 0
    
    llr_d <-
      log((p_ds * (1 - p_fa) + (1 - p_ds) * p_miss) / (1 - p_miss)) # Log likelihood ratio for perceiveing distractor when target present vs. absent
    llr_d_s <-
      log(p_ds) # Simplified log likelihood ratio assuming participant do not take into account their misperception rates
    llr_t <-
      min(log(((1 - p_ds) * (1 - p_miss) + p_ds * p_fa) / p_fa), 1000) # LLR for perceiving target when present vs. absent
    llr_t_s <- 1000
    lpo_thresh_p <- log(p_thresh_p / (1 - p_thresh_p))
    lpo_thresh_a <- log(p_thresh_a / (1 - p_thresh_a))
    while (n < N_max &
           abs(lpo_p) < lpo_thresh_p & abs(lpo_a) < lpo_thresh_a) {
      rt <- rt + rnorm(1, dt_m, dt_sd)
      n <- n + 1
      if (runif(1) < p_d) {
        # Sampled a distractor
        if (runif(1) > p_fa) {
          # Correctly perceived distractor as distractor
          lpo_a <- lpo_a + llr_d + rnorm(1, 0, noise_sd)
        } else {
          # Incorrectly perceived distractor as target
          lpo_p <- lpo_p + llr_t + rnorm(1, 0, noise_sd)
        }
      } else {
        # Sampled the target
        if (runif(1) > p_miss) {
          # Correctly detected the target
          lpo_p <- lpo_p + llr_t + rnorm(1, 0, noise_sd)
        } else {
          # Missed the target
          lpo_a <- lpo_a + llr_d + rnorm(1, 0, noise_sd)
        }
      }
    }
    return(c(rt + ndt, lpo_p >= lpo_thresh_p))
  }

#' Simulate one trial of dynamic search experiment
#' This model assumes that on each frame of the dynamic search display participants first sample one or
#' a few items and then make a decision about whether a target is present among the sampled items.
#' If this decision is positive the participant stops and response "target present". If negative
#' the participant either waits another frame or stops and responds "target absent" depending on
#' whether a threshold on the number of frames has been reached. This threshold could be set based
#' on learning how long it typically takes to find the target but with some random trial to trial
#' variation.
#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param dt Time spent per samoled item(s)
#' @param ndt Nondecision time
#' @param N_m Mean stopping threshold
#' @param N_sd SD of stopping threshold
#' @param C Criterion level on the decision variable on each frame
#' @param dm Mean of decision variable when sampled items include target
#' @param dsd SD of decision variable (assumed to be the same whether target is included or not)
#' @return rt and participant response (1=present, 0=absent)
simulate_trial4 <-
  function(p_d,
           N_m,
           N_sd,
           dm,
           dsd,
           C = 1,
           dt = 110,
           ndt = 0) {
    N_max <- round(rnorm(1, N_m, N_sd))
    tp <- runif(N_max) < p_d # Target present or not
    da <- rnorm(N_max, 0, dsd) > C # Decisions on target absent trials
    dp <-
      rnorm(N_max, dm, dsd) > C # Decisions on target present trials
    
    n <-
      which((dp * (1 - tp) + da * tp) > 0)[1]  # When is target present decision first reached
    
    if (is.na(n)) {
      # Target present decision was not reached before the stopping threshold
      d <- FALSE # Decide "target absent"
      rt <- N_max * dt
    } else {
      # Target present decision was reached before the stopping threshold
      d <- TRUE  # Decide "target present"
      rt <- n * dt
    }
    
    return(c(rt + ndt, d))
  }


#' Simulate one trial of dynamic search experiment
#' This model assumes that on each frame of the dynamic search display participants first sample one or
#' a few items and then make a decision about whether a target is present among the sampled items.
#' If this decision is positive the participant stops and response "target present". If negative
#' the participant either waits another frame or stops and responds "target absent" depending on
#' whether an accumulating stop signal has reached threshold. The stop signal increases on each
#' frame depending on how far below criterion level the decision variable is.
#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param dt Time spent per samoled item(s)
#' @param ndt Nondecision time
#' @param N_m Stopping threshold
#' @param C Criterion level on the decision variable on each frame
#' @param dm Mean of decision variable when sampled items include target
#' @param dsd SD of decision variable (assumed to be the same whether target is included or not)
#' @return rt and participant response (1=present, 0=absent)
simulate_trial5 <- function(p_d,
                            Ts,
                            dm,
                            dsd,
                            C,
                            dt = 110,
                            ndt = 0) {
  d <- -1
  ss_start <- 0 # Starting value for stop signal
  n <- 0
  N <-
    20 # Number of frames to check each iteration, does not change result only speed
  
  while (d == -1) {
    # Repeat while a decision has not been reached yet
    tp <- runif(N) < p_d # absent assigned 1, present 0
    sa <- rnorm(N, 0, dsd) # target absent variables
    da <-
      sa > C # Decisions with 'target present' on target absent trials
    sp <- rnorm(N, dm, dsd) # target present variables
    dp <-
      sp > C # Decisions with 'target present' on target present trials
    ss <-
      ss_start + cumsum(tp * (C - sa) + (1 - tp) * (C - sp)) # cumulative evidence on target absence
    
    n_p <-
      which((dp * (1 - tp) + da * tp) > 0)[1]  # When is target present decision first reached
    n_a <-
      which(ss > Ts)[1] # When is a target absent decision first reached
    
    if (is.na(n_a) & is.na(n_p)) {
      # No decision was reached yet
      n <- n + N
    } else if (is.na(n_a)) {
      # A target present decision was reached
      n <- n + n_p
      d <- TRUE
    } else if (is.na(n_p)) {
      # A target absent decision was reached
      n <- n + n_a
      d <- FALSE
    } else {
      # Both decisions were reached so we check which one was reached first
      if (n_p <= n_a) {
        n <- n + n_p
        d <- TRUE
      } else {
        n <- n + n_a
        d <- FALSE
      }
    }
    ss_start <- ss[N]
  }
  rt <- n * dt
  return(c(rt + ndt, d))
}

#' @param p_d probability that sampled item is a distractor, should be 1 for target absent and arguably 1-1/#items for present
#' @param dt Time spent per samoled item(s)
#' @param ndt Nondecision time
#' @param N_m Stopping threshold
#' @param C Criterion level on the decision variable on each frame
#' @param dm Mean of decision variable when sampled items include target
#' @param dsd SD of decision variable (assumed to be the same whether target is included or not)
#' @return rt and participant response (1=present, 0=absent)
simulate_trial6 <- function(p_d,
                            Ts,
                            dm,
                            dsd,
                            C,
                            dt = 110,
                            ndt = 0) {
  d <- -1
  ss_start <- 0 # Starting value for stop signal
  n <- 0
  N <-
    20 # Number of frames to check each iteration, does not change result only speed
  
  while (d == -1) {
    # Repeat while a decision has not been reached yet
    tp <- runif(N) < p_d # Target present or not
    sa <- rnorm(N, 0, dsd)
    da <-  sa > C # Decisions on target absent trials
    sp <- rnorm(N, dm, dsd)
    dp <- sp > C # Decisions on target present trials
    ss <- ss_start + cumsum(rnorm(N, 1, dsd))
    
    n_p <-
      which((dp * (1 - tp) + da * tp) > 0)[1]  # When is target present decision first reached
    n_a <-
      which(ss > Ts)[1] # When is a target absent decision first reached
    
    if (is.na(n_a) & is.na(n_p)) {
      # No decision was reached yet
      n <- n + N
    } else if (is.na(n_a)) {
      # A target present decision was reached
      n <- n + n_p
      d <- TRUE
    } else if (is.na(n_p)) {
      # A target absent decision was reached
      n <- n + n_a
      d <- FALSE
    } else {
      # Both decisions were reached so we check which one was reached first
      if (n_p <= n_a) {
        n <- n + n_p
        d <- TRUE
      } else {
        n <- n + n_a
        d <- FALSE
      }
    }
    ss_start <- ss[N]
  }
  rt <- n * dt
  return(c(rt + ndt, d))
}

#' Simulates many trials of a dynamic search experiment, e.g. a whole experiment or one experimental conditition
#' @param N_trials number of trials
#' @param p_present proportion of target present trials
#' @param p_d probability that sampled item is a distractor on a target present trial
#' @param p_ds The subjective probability a participant assigns to randomly sampling a distractor (if target is present)
#' @param p_thresh The threshold of posterior probability (of target present) below which a participant decides to stop sampling and respond absent
#' @param dt_m Mean time spent per sampled item
#' @param dt_sd SD of normal distribution for time spent per sampled item
#' @param ndt Nondecision time, added to all RTs
#' @param N_max Maximum number of samples, participant stops at this point regardless of the current p
#' @param p_fa Probability of misperceiving a distractor as a target (and responding present, note this is not exactly the FA rate)
#' @param p_miss Probability of misperceiving the target as a distractor (and updating posterior like for any other distractor)
#' @param p_prior Prior probability of target present
#' @param N_m Mean stopping threshold
#' @param N_sd SD of stopping threshold
#' @param dm Mean of decision variable when sampled items include target
#' @param dsd SD of decision variable (assumed to be the same whether target is included or not
#' @return rt and participant response (1=present, 0=absent)
simulate_experiment <-
  function(N_trials,
           p_present,
           which_model,
           p_d,
           p_ds = 0,
           p_thresh = 0,
           dt_m = 0,
           dt_sd = 0,
           noise_sd = 0,
           p_thresh_a = 0,
           ndt = 0,
           N_max = Inf,
           p_fa = 0,
           p_miss = 0,
           p_prior = 0.5,
           N_m = 0,
           N_sd = 0,
           dm = 0,
           dsd = 0,
           C = 1)
  {
    rts <- rep(0, N_trials)
    hit <- rep(0, N_trials)
    cr <- rep(0, N_trials)
    miss <- rep(0, N_trials)
    fa <- rep(0, N_trials)
    for (i in 1:N_trials) {
      if (i > floor(N_trials * p_present)) {
        # simulate target present trial
        switch(
          which_model,
          t <-
            simulate_trial(
              p_d = p_d,
              p_ds = p_ds,
              p_thresh = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial2(
              p_d = p_d,
              p_ds = p_ds,
              p_thresh = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              noise_sd = noise_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial3(
              p_d = p_d,
              p_ds = p_ds,
              p_thresh_p = p_thresh,
              p_thresh_a = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              noise_sd = noise_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial4(
              p_d = p_d,
              N_m = N_m,
              N_sd = N_sd,
              dm = dm,
              dsd = dsd,
              C = C
            ),
          t <-
            simulate_trial5(
              p_d = p_d,
              Ts = N_m,
              dm = dm,
              dsd = dsd,
              C = C
            )
        )
        rts[i] <- t[1]
        if (t[2]) {
          hit[i] <- 1
        } else {
          miss[i] <- 1
        }
      } else {
        # simulate target absent trial
        switch(
          which_model,
          t <-
            simulate_trial(
              p_d = 1,
              p_ds = p_ds,
              p_thresh = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial2(
              p_d = 1,
              p_ds = p_ds,
              p_thresh = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              noise_sd = noise_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial3(
              p_d = 1,
              p_ds = p_ds,
              p_thresh_p = p_thresh,
              p_thresh_a = p_thresh,
              dt_m = dt_m,
              dt_sd = dt_sd,
              noise_sd = noise_sd,
              ndt = ndt,
              N_max = N_max,
              p_fa = p_fa,
              p_miss = p_miss,
              p_prior = p_prior
            ),
          t <-
            simulate_trial4(
              p_d = 1,
              N_m = N_m,
              N_sd = N_sd,
              dm = dm,
              dsd = dsd,
              C = C
            ),
          t <-
            simulate_trial5(
              p_d = 1,
              Ts = N_m,
              dm = dm,
              dsd = dsd,
              C = C
            )
        )
        rts[i] <- t[1]
        if (t[2]) {
          fa[i] <- 1
        } else {
          cr[i] <- 1
        }
      }
    }
    hr = sum(hit) / (sum(hit) + sum(miss))
    far = sum(fa) / (sum(fa) + sum(cr))
    return(
      list(
        rts_hit = rts[hit == 1],
        rts_miss = rts[miss == 1],
        rts_cr = rts[cr == 1],
        rts_fa = rts[fa == 1],
        hr = hr,
        far = far,
        dprime = qnorm(min(hr, 0.995)) - qnorm(max(far, 0.005)),
        C = -(qnorm(min(hr, 0.995)) + qnorm(max(far, 0.005))) / 2,
        mRT_hit = mean(rts[hit == 1]),
        mRT_miss = mean(rts[miss == 1]),
        mRT_cr = mean(rts[cr == 1]),
        mRT_fa = mean(rts[fa == 1])
      )
    )
  }

fit_model <-
  function (param,
            data,
            model = 1,
            weights = c(1, 1, 1),
            plot = 0,
            return_model = 0,
            N = 5000) {
    if (model == 1) {
      p_fa = param[1]
      N_max_8 = param[2] + round(8 * param[3])
      
      N_max_12 = param[2] + round(12 * param[3])
      
      N_max_16 = param[2] + round(16 * param[3])
      
      pd_8 <-
        1 - 1 / (8 * param[4]) # Participants may scan more than one item at a time
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_8,
        p_ds = pd_8,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_8
      ) -> mod_8
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_12,
        p_ds = pd_12,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_12
      ) -> mod_12
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_16,
        p_ds = pd_16,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_16
      ) -> mod_16
    } else if (model == 2) {
      p_fa = param[1]
      p_t_8 = param[2] - round(8 * param[3])
      
      p_t_12 = param[2] - round(12 * param[3])
      
      p_t_16 = param[2] - round(16 * param[3])
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      
      simulate_experiment(
        N,
        0.5,
        2,
        p_d = pd_8,
        p_ds = pd_8,
        p_thresh = p_t_8,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_8
      simulate_experiment(
        N,
        0.5,
        2,
        p_d = pd_12,
        p_ds = pd_12,
        p_thresh = p_t_12,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_12
      simulate_experiment(
        N,
        0.5,
        2,
        p_d = pd_16,
        p_ds = pd_16,
        p_thresh = p_t_16,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_16
    } else if (model == 3) {
      p_fa = param[1]
      p_t_8 = param[2] - round(8 * param[3])
      
      p_t_12 = param[2] - round(12 * param[3])
      
      p_t_16 = param[2] - round(16 * param[3])
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      
      simulate_experiment(
        N,
        0.5,
        3,
        p_d = pd_8,
        p_ds = pd_8,
        p_thresh = p_t_8,
        p_thresh_a = p_t_8,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_8
      simulate_experiment(
        N,
        0.5,
        3,
        p_d = pd_12,
        p_ds = pd_12,
        p_thresh = p_t_12,
        p_thresh_a = p_t_12,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_12
      simulate_experiment(
        N,
        0.5,
        3,
        p_d = pd_16,
        p_ds = pd_16,
        p_thresh = p_t_16,
        p_thresh_a = p_t_16,
        dt_m = 110,
        noise_sd = 0,
        ndt = 0,
        p_fa = p_fa
      ) -> mod_16
    } else if (model == 4)  {
      N_sd <- param[1]
      N_max_8 = param[2] + round(8 * param[3])
      
      N_max_12 = param[2] + round(12 * param[3])
      
      N_max_16 = param[2] + round(16 * param[3])
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      dm <- param[5]
      dsd <- param[6]
      
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_8,
        N_m = N_max_8,
        N_sd = N_sd,
        dm = dm,
        dsd = dsd
      ) -> mod_8
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_12,
        N_m = N_max_12,
        N_sd = N_sd,
        dm = dm,
        dsd = dsd
      ) -> mod_12
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_16,
        N_m = N_max_16,
        N_sd = N_sd,
        dm = dm,
        dsd = dsd
      ) -> mod_16
    } else if (model == 5)  {
      C <- param[1]
      Ts_8 = param[2] + 8 * param[3]
      
      Ts_12 = param[2] + 12 * param[3]
      
      Ts_16 = param[2] + 16 * param[3]
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      dm <- param[5]
      
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_8,
        N_m = Ts_8,
        dm = dm,
        dsd = 1,
        C = C
      ) -> mod_8
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_12,
        N_m = Ts_12,
        dm = dm,
        dsd = 1,
        C = C
      ) -> mod_12
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_16,
        N_m = Ts_16,
        dm = dm,
        dsd = 1,
        C = C
      ) -> mod_16
    }
    
    # For reaction times look at differences since one of them can easily be fit by setting the NDT parameter appropriately
    model_rts <-
      c(
        mod_12$mRT_hit - mod_8$mRT_hit,
        mod_16$mRT_hit - mod_8$mRT_hit,
        mod_8$mRT_cr - mod_8$mRT_hit,
        mod_12$mRT_cr - mod_8$mRT_hit,
        mod_16$mRT_cr - mod_8$mRT_hit
      ) / 1000
    data_rts <-
      c(data$Present[2:3] - data$Present[1], data$Absent - data$Present[1])
    model_dp <- c(mod_8$dprime, mod_12$dprime, mod_16$dprime)
    data_dp <- data$mdprime
    model_c <- c(mod_8$C, mod_12$C, mod_16$C)
    data_c <- data$mbias
    
    # normalize errors
    err_rt = model_rts - data_rts
    #  err_rt = err_rt / (max(err_rt)-min(err_rt))
    err_dp = model_dp - data_dp
    #  err_dp = err_dp / (max(err_dp)-min(err_dp))
    
    err_c = model_c - data_c
    #  err_c = err_c / (max(err_c)-min(err_c))
    
    if (plot) {
      rtdata <-
        data %>% select(setsize, Absent, Present, Absent_se, Present_se)
      mrtdata <-
        gather(rtdata, target, mrt, Absent, Present) %>% select(setsize, target, mrt)
      names(rtdata)[c(2:5)] <-
        c("Absent_m", "Present_m", "Absent", "Present")
      sertdata <-
        gather(rtdata, target, sert, Absent, Present) %>% select(setsize, target, sert)
      rtdata <- full_join(mrtdata, sertdata)
      
      
      rts_mod <- data.frame(
        setsize = c(8, 12, 16, 8, 12, 16),
        target = c(
          'Present',
          'Present',
          'Present',
          'Absent',
          'Absent',
          'Absent'
        ),
        mRT = c(
          mod_8$mRT_hit,
          mod_12$mRT_hit,
          mod_16$mRT_hit,
          mod_8$mRT_cr,
          mod_12$mRT_cr,
          mod_16$mRT_cr
        )
      )
      sorted_data <-
        gather(data, target, rt, Absent, Present) %>% arrange(desc(target))
      ndt <- mean(sorted_data$rt * 1000 - mean(rts_mod$mRT))
      rts_mod$mRT <- rts_mod$mRT + ndt
      rt_plot <-
        ggplot(rts_mod,
               aes(
                 x = setsize,
                 y = mRT,
                 color = target,
                 group = target,
                 shape = target
               )) + geom_line() +
        geom_point(data = rtdata,
                   aes(
                     x = setsize,
                     y = mrt * 1000,
                     color = target
                   ),
                   size = 3) +
        geom_errorbar(data = rtdata,
                      aes(
                        y = mrt * 1000,
                        ymin = (mrt - sert * 1.96) * 1000,
                        ymax = (mrt + sert * 1.96) * 1000
                      ),
                      width = 0.3) +
        theme_bw() + labs(x = "Set Size", y = "RT (ms)") +
        scale_color_manual(values = c('black', 'gray')) +
        theme(strip.background = element_blank(),
              legend.position = 'none')
      
      
      signal_detection <-
        data.frame(
          set_size = c(8, 12, 16),
          dprime = c(mod_8$dprime, mod_12$dprime, mod_16$dprime),
          C = c(mod_8$C, mod_12$C, mod_16$C)
        )
      dp_plot <-
        ggplot(signal_detection, aes(x = set_size, y = dprime, group = 1)) +
        geom_line() + geom_point(data = data,
                                 aes(x = setsize, y = mdprime),
                                 size = 3) +
        geom_errorbar(
          data = data,
          aes(
            x = setsize,
            y = mdprime,
            ymin = mdprime - 1.96 * dse,
            ymax = mdprime + 1.96 * dse,
            width = 0.3
          )
        ) +
        theme_bw() + labs(x = "Set Size", y = "d'") + theme(legend.position = "bottom") +
        scale_color_manual(values = c('black', 'gray'))
      C_plot <-
        ggplot(signal_detection, aes(x = set_size, y = C, group = 1)) +
        geom_line() + geom_point(data = data, aes(x = setsize, y = mbias), size =
                                   3) +
        scale_color_manual(values = c('black', 'gray')) +
        geom_errorbar(
          data = data,
          aes(
            x = setsize,
            y = mdprime,
            ymin = mbias - 1.96 * cse,
            ymax = mbias + 1.96 * cse,
            width = 0.3
          )
        ) +
        theme_bw() + labs(x = "Set Size", y = "C") + theme(legend.position = "bottom") + coord_cartesian(ylim =
                                                                                                           c(-1, 1))
      #    bottom_row <- plot_grid(dp_plot, C_plot, nrow=1, labels = c("B", "C"))
      #    print(plot_grid(dp_plot, C_plot, rt_plot, NULL, ncol=2, nrow=2, rel_heights = c(1,1.2),
      #                    labels=c("A","B", "C", "")))
      print(plot_grid(
        rt_plot,
        dp_plot,
        C_plot,
        ncol = 3,
        nrow = 1,
        rel_heights = c(1, 1.2)
      ))
      
    }
    
    if (return_model == 1) {
      mod_8$ndt <- ndt
      mod_12$ndt <- ndt
      mod_16$ndt <- ndt
      return(list(mod_8, mod_12, mod_16))
    } else {
      return(weights[1] * sum((err_rt) ^ 2) + weights[2] * sum((err_dp) ^ 2) +
               weights[3] * sum((err_c) ^ 2))
    }
  }

# Experiment 2 involves more data points and more parameters so we need a separarate function
fit_model_e2 <-
  function (param,
            model = 1,
            data,
            weights = c(1, 1, 1),
            plot = 0,
            return_model = 0,
            N = 5000) {
    if (model == 1) {
      p_fa = param[1]
      N_max_8 = param[2] + round(8 * param[3])
      N_max_12 = param[2] + round(12 * param[3])
      N_max_16 = param[2] + round(16 * param[3])
      pd_8 <-
        1 - 1 / (8 * param[4]) # Participants may scan more than one item at a time
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      N_max_8_rp = N_max_8 + param[5]
      N_max_12_rp = N_max_12 + param[5]
      N_max_16_rp = N_max_16 + param[5]
      N_max_8_ra = N_max_8 - param[5]
      N_max_12_ra = N_max_12 - param[5]
      N_max_16_ra = N_max_16 - param[5]
      
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_8,
        p_ds = pd_8,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_8_rp
      ) -> mod_8_rp
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_12,
        p_ds = pd_12,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_12_rp
      ) -> mod_12_rp
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_16,
        p_ds = pd_16,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_16_rp
      ) -> mod_16_rp
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_8,
        p_ds = pd_8,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_8_ra
      ) -> mod_8_ra
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_12,
        p_ds = pd_12,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_12_ra
      ) -> mod_12_ra
      simulate_experiment(
        N,
        0.5,
        1,
        p_d = pd_16,
        p_ds = pd_16,
        p_thresh = 0.0001,
        dt_m = 110,
        dt_sd = 0,
        ndt = 0,
        p_fa = p_fa,
        N_max = N_max_16_ra
      ) -> mod_16_ra
      
    } else if (model == 4) {
      N_sd <- 0
      C <- param[1]
      N_max_8 = param[2] + round(8 * param[3])
      
      N_max_12 = param[2] + round(12 * param[3])
      
      N_max_16 = param[2] + round(16 * param[3])
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      dm <- param[5]
      C_rp <- C - param[6]
      C_ra <- C + param[6]
      N_max_8_rp = N_max_8 + param[7]
      N_max_12_rp = N_max_12 + param[7]
      N_max_16_rp = N_max_16 + param[7]
      N_max_8_ra = N_max_8 - param[7]
      N_max_12_ra = N_max_12 - param[7]
      N_max_16_ra = N_max_16 - param[7]
      
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_8,
        N_m = N_max_8_rp,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_8_rp
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_12,
        N_m = N_max_12_rp,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_12_rp
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_16,
        N_m = N_max_16_rp,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_16_rp
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_8,
        N_m = N_max_8_ra,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_8_ra
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_12,
        N_m = N_max_12_ra,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_12_ra
      simulate_experiment(
        N,
        0.5,
        4,
        p_d = pd_16,
        N_m = N_max_16_ra,
        N_sd = N_sd,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_16_ra
      
      # For reaction times look at differences since one of them can easily be fit by setting the NDT parameter appropriately
      model_rts <-
        c(
          mod_12_ra$mRT_hit - mod_8_ra$mRT_hit,
          mod_16_ra$mRT_hit - mod_8_ra$mRT_hit,
          mod_8_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_12_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_16_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_12_rp$mRT_hit - mod_8_rp$mRT_hit,
          mod_16_rp$mRT_hit - mod_8_rp$mRT_hit,
          mod_8_rp$mRT_cr - mod_8_rp$mRT_hit,
          mod_12_rp$mRT_cr - mod_8_rp$mRT_hit,
          mod_16_rp$mRT_cr - mod_8_rp$mRT_hit
        ) / 1000
      data_rts <-
        c(
          data$Present[c(3, 5)] - data$Present[1],
          data$Absent[c(1, 3, 5)] - data$Present[1],
          data$Present[c(4, 6)] - data$Present[2],
          data$Absent[c(2, 4, 6)] - data$Present[2]
        )
      model_dp <-
        c(
          mod_8_ra$dprime,
          mod_8_rp$dprime,
          mod_12_ra$dprime,
          mod_12_rp$dprime,
          mod_16_ra$dprime,
          mod_16_rp$dprime
        )
      data_dp <- data$mdprime
      model_c <-
        c(mod_8_ra$C,
          mod_8_rp$C,
          mod_12_ra$C,
          mod_12_rp$C,
          mod_16_ra$C,
          mod_16_rp$C)
      data_c <- data$mbias
      
    } else if (model == 5) {
      C <- param[1]
      Ts_8 = param[2] + 8 * param[3]
      
      Ts_12 = param[2] + 12 * param[3]
      
      Ts_16 = param[2] + 16 * param[3]
      
      pd_8 <- 1 - 1 / (8 * param[4])
      pd_12 <- 1 - 1 / (12 * param[4])
      pd_16 <- 1 - 1 / (16 * param[4])
      dm <- param[5]
      C_rp <- C - param[6]
      C_ra <- C + param[6]
      Ts_8_rp = Ts_8 + param[7]
      Ts_12_rp = Ts_12 + param[7]
      Ts_16_rp = Ts_16 + param[7]
      Ts_8_ra = Ts_8 - param[7]
      Ts_12_ra = Ts_12 - param[7]
      Ts_16_ra = Ts_16 - param[7]
      
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_8,
        N_m = Ts_8_rp,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_8_rp
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_12,
        N_m = Ts_12_rp,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_12_rp
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_16,
        N_m = Ts_16_rp,
        dm = dm,
        dsd = 1,
        C = C_rp
      ) -> mod_16_rp
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_8,
        N_m = Ts_8_ra,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_8_ra
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_12,
        N_m = Ts_12_ra,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_12_ra
      simulate_experiment(
        N,
        0.5,
        5,
        p_d = pd_16,
        N_m = Ts_16_ra,
        dm = dm,
        dsd = 1,
        C = C_ra
      ) -> mod_16_ra
      
      # For reaction times look at differences since one of them can easily be fit by setting the NDT parameter appropriately
      model_rts <-
        c(
          mod_12_ra$mRT_hit - mod_8_ra$mRT_hit,
          mod_16_ra$mRT_hit - mod_8_ra$mRT_hit,
          mod_8_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_12_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_16_ra$mRT_cr - mod_8_ra$mRT_hit,
          mod_12_rp$mRT_hit - mod_8_rp$mRT_hit,
          mod_16_rp$mRT_hit - mod_8_rp$mRT_hit,
          mod_8_rp$mRT_cr - mod_8_rp$mRT_hit,
          mod_12_rp$mRT_cr - mod_8_rp$mRT_hit,
          mod_16_rp$mRT_cr - mod_8_rp$mRT_hit
        ) / 1000
      data_rts <-
        c(
          data$Present[c(3, 5)] - data$Present[1],
          data$Absent[c(1, 3, 5)] - data$Present[1],
          data$Present[c(4, 6)] - data$Present[2],
          data$Absent[c(2, 4, 6)] - data$Present[2]
        )
      model_dp <-
        c(
          mod_8_ra$dprime,
          mod_8_rp$dprime,
          mod_12_ra$dprime,
          mod_12_rp$dprime,
          mod_16_ra$dprime,
          mod_16_rp$dprime
        )
      data_dp <- data$mdprime
      model_c <-
        c(mod_8_ra$C,
          mod_8_rp$C,
          mod_12_ra$C,
          mod_12_rp$C,
          mod_16_ra$C,
          mod_16_rp$C)
      data_c <- data$mbias
      
    }
    
    # For reaction times look at differences since one of them can easily be fit by setting the NDT parameter appropriately
    model_rts <-
      c(
        mod_12_ra$mRT_hit - mod_8_ra$mRT_hit,
        mod_16_ra$mRT_hit - mod_8_ra$mRT_hit,
        mod_8_ra$mRT_cr - mod_8_ra$mRT_hit,
        mod_12_ra$mRT_cr - mod_8_ra$mRT_hit,
        mod_16_ra$mRT_cr - mod_8_ra$mRT_hit,
        mod_8_rp$mRT_hit - mod_8_ra$mRT_hit,
        mod_12_rp$mRT_hit - mod_8_ra$mRT_hit,
        mod_16_rp$mRT_hit - mod_8_ra$mRT_hit,
        mod_8_rp$mRT_cr - mod_8_ra$mRT_hit,
        mod_12_rp$mRT_cr - mod_8_ra$mRT_hit,
        mod_16_rp$mRT_cr - mod_8_ra$mRT_hit
      ) / 1000
    data_rts <-
      c(
        data$Present[c(3, 5)] - data$Present[1],
        data$Absent[c(1, 3, 5)] - data$Present[1],
        data$Present[c(2, 4, 6)] - data$Present[1],
        data$Absent[c(2, 4, 6)] - data$Present[1]
      )
    model_dp <-
      c(
        mod_8_ra$dprime,
        mod_8_rp$dprime,
        mod_12_ra$dprime,
        mod_12_rp$dprime,
        mod_16_ra$dprime,
        mod_16_rp$dprime
      )
    
    data_dp <- data$mdprime
    model_c <-
      c(mod_8_ra$C,
        mod_8_rp$C,
        mod_12_ra$C,
        mod_12_rp$C,
        mod_16_ra$C,
        mod_16_rp$C)
    data_c <- data$mbias
    
    if (plot == 1) {
      rtdata <-
        data %>% select(setsize, reward, Absent, Present, Absent_se, Present_se)
      mrtdata <-
        gather(rtdata, target, mrt, Absent, Present) %>% select(setsize, reward, target, mrt)
      names(rtdata)[c(3:6)] <-
        c("Absent_m", "Present_m", "Absent", "Present")
      sertdata <-
        gather(rtdata, target, sert, Absent, Present) %>% select(setsize, reward, target, sert)
      rtdata <- full_join(mrtdata, sertdata)
      
      rts_mod <-
        data.frame(
          setsize = c(8, 12, 16, 8, 12, 16, 8, 12, 16, 8, 12, 16),
          target = c(
            'Present',
            'Present',
            'Present',
            'Absent',
            'Absent',
            'Absent',
            'Present',
            'Present',
            'Present',
            'Absent',
            'Absent',
            'Absent'
          ),
          reward = c(
            'RP',
            'RP',
            'RP',
            'RP',
            'RP',
            'RP',
            'RA',
            'RA',
            'RA',
            'RA',
            'RA',
            'RA'
          ),
          mRT = c(
            mod_8_rp$mRT_hit,
            mod_12_rp$mRT_hit,
            mod_16_rp$mRT_hit,
            mod_8_rp$mRT_cr,
            mod_12_rp$mRT_cr,
            mod_16_rp$mRT_cr,
            mod_8_ra$mRT_hit,
            mod_12_ra$mRT_hit,
            mod_16_ra$mRT_hit,
            mod_8_ra$mRT_cr,
            mod_12_ra$mRT_cr,
            mod_16_ra$mRT_cr
          )
        )
      sorted_data <-
        gather(e2_data, target, rt, Absent, Present) %>% arrange(desc(reward), desc(target))
      ndt <- mean(sorted_data$rt * 1000 - mean(rts_mod$mRT))
      rts_mod$mRT <- rts_mod$mRT + ndt
      rt_plot <-
        ggplot(rts_mod,
               aes(
                 x = setsize,
                 y = mRT,
                 color = target,
                 shape = target,
                 group = target
               )) +
        geom_line() + geom_point(data = rtdata,
                                 aes(
                                   x = setsize,
                                   y = mrt * 1000,
                                   color = target
                                 ),
                                 size = 3) +
        geom_errorbar(data = rtdata,
                      aes(
                        y = mrt * 1000,
                        ymin = (mrt - sert * 1.96) * 1000,
                        ymax = (mrt + sert * 1.96) * 1000
                      ),
                      width = 0.3) +
        theme_bw() + facet_wrap( ~ reward) + labs(x = "Set Size", y = "RT (ms)") +
        scale_color_manual(values = c('black', 'gray')) +
        theme(strip.background = element_blank(),
              legend.position = "bottom")
      
      # Plot signal detection measures: dprime and C
      signal_detection <- data.frame(
        set_size = c(8, 12, 16, 8, 12, 16),
        reward = c('RP', 'RP', 'RP', 'RA', 'RA', 'RA'),
        dprime = c(
          mod_8_rp$dprime,
          mod_12_rp$dprime,
          mod_16_rp$dprime,
          mod_8_ra$dprime,
          mod_12_ra$dprime,
          mod_16_ra$dprime
        ),
        C = c(
          mod_8_rp$C,
          mod_12_rp$C,
          mod_16_rp$C,
          mod_8_ra$C,
          mod_12_ra$C,
          mod_16_ra$C
        )
      )
      dp_plot <-
        ggplot(
          signal_detection,
          aes(
            x = set_size,
            y = dprime,
            color = reward,
            shape = reward,
            group = reward
          )
        ) +
        geom_line() + geom_point(data = data,
                                 aes(x = setsize, y = mdprime),
                                 size = 3) +
        scale_color_manual(values = c('black', 'gray')) +
        geom_errorbar(
          data = data,
          aes(
            x = setsize,
            y = mdprime,
            ymin = mdprime - 1.96 * dse,
            ymax = mdprime + 1.96 * dse,
            width = 0.3
          )
        ) +
        theme_bw() + labs(x = "Set Size", y = "d'") + theme(legend.position = "bottom")
      C_plot <-
        ggplot(
          signal_detection,
          aes(
            x = set_size,
            y = C,
            color = reward,
            shape = reward,
            group = reward
          )
        ) +
        geom_line() + geom_point(data = data, aes(x = setsize, y = mbias), size =
                                   3) +
        scale_color_manual(values = c('black', 'gray')) +
        geom_errorbar(
          data = data,
          aes(
            x = setsize,
            y = mdprime,
            ymin = mbias - 1.96 * cse,
            ymax = mbias + 1.96 * cse,
            width = 0.3
          )
        ) +
        theme_bw() + labs(x = "Set Size", y = "C") + coord_cartesian(ylim =
                                                                       c(-1.5, 1.5))  + theme(legend.position = "bottom")
      bottom_row <-
        plot_grid(dp_plot,
                  C_plot,
                  nrow = 1,
                  labels = c("B", "C"))
      print(plot_grid(
        rt_plot,
        bottom_row,
        ncol = 1,
        labels = c("A", ""),
        rel_heights = c(1.1, 1)
      ))
    }
    
    # normalize errors
    err_rt = model_rts - data_rts
    #  err_rt = err_rt / (max(err_rt)-min(err_rt))
    err_dp = model_dp - data_dp
    #  err_dp = err_dp / (max(err_dp)-min(err_dp))
    
    err_c = model_c - data_c
    #  err_c = err_c / (max(err_c)-min(err_c))
    
    if (return_model == 1) {
      mod_8_rp$ndt <- ndt
      mod_8_ra$ndt <- ndt
      mod_12_rp$ndt <- ndt
      mod_12_ra$ndt <- ndt
      mod_16_rp$ndt <- ndt
      mod_16_ra$ndt <- ndt
      return(list(
        mod_8_rp,
        mod_8_ra,
        mod_12_rp,
        mod_12_ra,
        mod_16_rp,
        mod_16_ra
      ))
    } else {
      return(weights[1] * sum((err_rt) ^ 2) + weights[2] * sum((err_dp) ^ 2) +
               weights[3] * sum((err_c) ^ 2))
    }
  }

# ---- Fit model to Experiment 1 ----

# Prepare a reasonable range of parameter values
p1_range <- seq(2.2, 3, 0.1)
p2_range <- seq(14, 34)
p3_range <- seq(0, 1, 0.1)
p4_range <- 1 / seq(2:5)
p5_range <- seq(2.6, 4.4, 0.2)
para <-
  data.frame(t(expand.grid(
    p1_range, p2_range, p3_range, p4_range, p5_range
  )))

# Helper function for fitting model 5 to data from Exp. 1
e1_m5_fit <- function(p) {
  fit_model(p, model = 5, e1_data)
}

# Find best parameter values in the range defined above
fp_e1_m5 <- parEstimate(e1_m5_fit, para)
saveRDS(fp_e1_m5, 'data/fp_e1_m5.rds')

# Read results from previous optimization and extract best-fitting parameters
fp_e1_m5 <- readRDS('data/fp_e1_m5.rds')
fp_e1_m5 <- unlist(fp_e1_m5)
param <- para[fp_e1_m5 == min(fp_e1_m5)]
param <- param[[1]]

# param <- c(2.6, 21,  1.0,  1/3,  4.2)

l <-
  fit_model(
    param,
    model = 5,
    e1_data,
    plot = 1,
    return_model = 1,
    N = 100000
  )

# Plot model fit figure and extract the predictions of the model
fit_exp1 = last_plot() # save the plot
mod5_e1_8 <- l[[1]]
mod5_e1_12 <- l[[2]]
mod5_e1_16 <- l[[3]]

saveRDS(l, "data/modelfit_e1.rds")

# ---- Fit model to Experiment 2 ----

# Prepare a reasonable range of parameter values
p1_range <- seq(2.5, 3, 0.1)
p2_range <- seq(18, 23)
p3_range <- seq(0, 0.6, 0.1)
p4_range <- 1 / seq(2:5)
p5_range <- seq(3, 4, 0.2)
p6_range <- seq(0, 1, 0.25)
p7_range <- seq(0, 4.9, 0.7)
para <-
  data.frame(t(
    expand.grid(
      p1_range,
      p2_range,
      p3_range,
      p4_range,
      p5_range,
      p6_range,
      p7_range
    )
  ))

# Helper function for fitting model 5 to data from Exp. 2
e2_m5_fit <- function(p) {
  fit_model_e2(p, model = 5, e2_data)
}

# Find best parameter values in the range defined above
fp_e2_m5 <- parEstimate(e2_m5_fit, para)
saveRDS(fp_e2_m5, 'data/fp_e2_m5.rds')

# Read results from previous optimization and extract best-fitting parameters
fp_e2_m5 <- readRDS('data/fp_e2_m5.rds')
fp_e2_m5 <- unlist(fp_e2_m5)
param <- para[fp_e2_m5 == min(fp_e2_m5)]
param <- param[[1]]

# param <- c(2.7, 21, 0.4, 0.25, 3.4, 0.25, 4.2)

# Plot model fit figure and extract the predictions of the model
l <-
  fit_model_e2(
    param,
    model = 5,
    e2_data,
    plot = 1,
    return_model = 1,
    N = 100000
  )
fit_exp2 = last_plot() # save the plot
mod5_e2_8_rp <- l[[1]]
mod5_e2_12_rp <- l[[2]]
mod5_e2_16_rp <- l[[3]]
mod5_e2_8_ra <- l[[4]]
mod5_e2_12_ra <- l[[5]]
mod5_e2_16_ra <- l[[6]]

ggsave(
  'figures/Model_predictions_e2.png',
  fit_exp2,
  units = 'in',
  dpi = 600,
  width = 6,
  height = 7
)

 saveRDS(l, "data/modelfit_e2.rds")

# ---- Fit model to Experiment 3 ----

# Prepare a reasonable range of parameter values
p1_range <- seq(1.8, 2.6, 0.1)
p2_range <- seq(14, 34)
p3_range <- seq(0, 1, 0.1)
p4_range <- 1 / seq(2:5)
p5_range <- seq(1.4, 3.4, 0.2)
para <-
  data.frame(t(expand.grid(
    p1_range, p2_range, p3_range, p4_range, p5_range
  )))

# Helper function for fitting model 5 to data from Exp. 3
e3_m5_fit <-
  function(p) {
    fit_model(p, model = 5, e3_data, weights = c(1, 1, 4))
  }

# Find best parameter values in the range defined above
fp_e3_m5 <- parEstimate(e3_m5_fit, para)
saveRDS(fp_e3_m5, 'data/fp_e3_m5.rds')

# Read results from previous optimization and extract best-fitting parameters
fp_e3_m5 <- readRDS('data/fp_e3_m5.rds')
fp_e3_m5 <- unlist(fp_e3_m5)
param <- para[fp_e3_m5 == min(fp_e3_m5)]
param <- param[[1]]

# param <- c(2.1, 14,  0.7,  1/3,  2.0)

# Plot model fit figure and extract the predictions of the model
l <-
  fit_model(
    param,
    model = 5,
    e3_data,
    plot = 1,
    return_model = 1,
    N = 100000
  )
fit_exp3 = last_plot() # save the plot
mod5_e3_8 <- l[[1]]
mod5_e3_12 <- l[[2]]
mod5_e3_16 <- l[[3]]

saveRDS(l, "data/modelfit_e3.rds")


# Combine model fit figures for Exp 1 and Exp 3 and save
fit_figs = plot_grid(fit_exp1,
                     fit_exp3,
                     nrow = 2,
                     labels = c("A", "B"))
fit_figs
ggsave(
  'figures/fit_exp13.png',
  fit_figs,
  units = 'in',
  dpi = 600,
  width = 7,
  height = 4
)
