snf_abm <- function(inputs,
                    inf_per_100k = 12,
                    R0 = 1.4,
                    init_inf_staff = 0,
                    init_inf_res = 0,
                    R_0_staff = 0,
                    R_0_res   = 0,
                    test_delay = 2,
                    test_freq  = 3,
                    test_sensitivity = list(0.95, 0.95),
                    test_specificity = 0.995,
                    rm_asym = TRUE,
                    date_start = "2020-01-01",
                    sim_dur = 50,
                    delta_t = 1,
                    pIR = 0.9,
                    pAQ = 0.1,
                    contact_mat = matrix(c(7,1,7,
                                           1,3,2,
                                           7,2,1),3,3)) {
  
  # Static Model Parameters -------------------------------------------------
  gamma = 8 # time to recovery in days^-1
  De = 5       # incubation period in days
  Dq = 14      # recommended quarantine period
  # DiI = 8-De   # 
  # DiAI = 8     # 
  # DiAQ = 3     # 
  # R0int = 1    # a social distancing parameter
  # z = 1.65     # ratio of symptomatic to asymptomatic infectiousness
  pI = 0.4286  # probability of symptomatic
  eir = (14 * inf_per_100k)/100000 * 5
  pIR = 1 - ((1-pIR)^(1/7)) # probability that symptomatic does quarantine
  # pAQ = 0.3  # probability that asymptomatic quarantine
  
  # Static Probabilities ----------------------------------------------------
  recover_prob   <- 1 - exp(-1/gamma * delta_t) # p of recovery at each time
  infective_prob <- 1 - exp(-1/De * delta_t) # p of moving from E -> I | A
  
  t <- 2
  tend <- t + sim_dur
  N <- nrow(inputs)
  current_roster <- inputs
  current_roster$on_site[current_roster$id %in% shift_change(current_roster) |
                           current_roster$role == 0] <- TRUE
  
  # Starter vaccinations
  if (R_0_staff + R_0_res > 0) {
    current_roster <- vaccinate(current_roster, R_0_staff, R_0_res)
  }
  
  # Starter infections
  if (sum(init_inf_staff, init_inf_res) > 0) {
    n_staff <- length(which(current_roster$role > 0))
    n_res <- length(which(current_roster$role == 0))
    
    init_inf_staff <- round(init_inf_staff * n_staff)
    init_inf_res <- round(init_inf_res * n_res)
    inf_staff <- sample(which(current_roster$role > 0), init_inf_staff)
    inf_res <- sample(which(current_roster$role == 0), init_inf_res)
    current_roster$state[c(inf_staff, inf_res)] <- "A"
  }
  
  # Initialize quarantine register and sim outputs
  q_register <- vector("list", sim_dur * 1.2)
  outputs <- vector("list")
  current_roster$time <- as.numeric(as.Date(date_start))
  outputs[[1]] <- current_roster %>%
    summarise_daily() %>%
    mutate("new_inf" = 0)
  
  while (t < tend) {
    current_roster$time <- as.numeric(as.Date(date_start)) + t
    new_roster <- current_roster
    
    # Assign day and night shift staff
    new_roster$on_site[shift_change(current_roster)] <- TRUE
    
    # New infections
    betas <- calc_betas(contact_mat, R0, pI)
    exposed <- exposure_fun(new_roster, contact_mat, betas$BI, betas$BA, eir, delta_t)
    if (length(exposed) > 0) {
      new_roster$state[exposed] <- "E"
    }

    probE <- runif(N)
    probSym <- runif(N)
    probR <- runif(N) 
    probQ <- runif(N)
    
    # Changing States ---------------------------------------------------------
    # After incubation period Exposed agents become Sym. or Asym.
    new_roster$state[which(current_roster$state=="E" & probE<infective_prob & probSym<pI)] <- "I"
    new_roster$state[which(current_roster$state=="E" & probE<infective_prob & probSym>pI)] <- "A"
    
    # Symptomatic moving to R
    new_roster$state[which(current_roster$state==2 & probR<recover_prob)] <- "R"
    
    # Asymptomatic moving to R
    new_roster$state[which(current_roster$state==4 & probR<recover_prob)] <- "R"
    
    # Testing then updating the quarantine registry
    if (t %% test_freq == 0) {
      index_q <- testing_fun(current_roster, test_sensitivity, test_specificity)
      q_register[[t + test_delay]] <- index_q
    }
    
    # Assign agents that tested Positive to quarantine
    if (test_delay <= sim_dur) {
      # cat("time: ", t, "\n")
      new_roster$q_status[current_roster$id %in% q_register[[t]]] <- TRUE
    }
    
    # Self isolation of symptomatic
    new_roster$q_status[current_roster$state == "I" &
                          !current_roster$q_status & 
                          probQ < pIR] <- TRUE
    
    # If an agent is quarantined increment q_day
    new_roster$q_day[current_roster$q_status] <- current_roster$q_day[current_roster$q_status] + 1
    
    # After q_day > Dq reset q_day, and q_status, and recover non-susceptible agents
    new_roster$q_status[which(current_roster$q_status & current_roster$q_day >= Dq)] <- FALSE
    new_roster$q_day[which(current_roster$q_status & current_roster$q_day >= Dq)] <- 0
    new_roster$state[which(current_roster$state != "S" &
                             current_roster$q_status &
                             current_roster$q_day >= Dq)] <- "R"
    
    # Clean Up ----------------------------------------------------------------

    daily_output <- summarise_daily(new_roster)
    daily_new_inf <- count_daily_inf(new_roster, current_roster)
    outputs[[t]] <- left_join(daily_output, daily_new_inf,
                              by = c("time", "role", "q_status", "on_site", "vaccinated"))
    
    current_roster <- new_roster
    t <- t + delta_t # update time
  }
  
  outputs %>%
    bind_rows() %>%
    mutate("new_inf" = ifelse(is.na(new_inf), 0, new_inf))
}

multi_run <- function(nruns, lock_seed = FALSE, ...)
{
  if (lock_seed) {
    set.seed(2345)
  }
  map_dfr(1:nruns, 
          function(run_num) {
            out <- snf_abm(...)
            out$run <- run_num
            out
          })
}
