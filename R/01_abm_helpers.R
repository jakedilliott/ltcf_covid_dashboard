## For functions used in the agent based model

make_agents <- function(day, night, res) {
  data.frame(id = seq_len(day + night + res),
             role = c(rep_len(1, day),
                      rep_len(2, night),
                      rep_len(0, res)),
             state = "S",
             q_status = FALSE,
             q_day = 0,
             on_site = FALSE,
             vaccinated = FALSE)
}

shift_change <- function(agents_in) {
  # clean slate, move all staff off site
  agents_in$on_site[agents_in$role > 0] <- FALSE
  
  #Day (25 staff)
  day_duty <- sample(agents_in$id[agents_in$role == 1], 25)
  
  #Night (9 staff)
  night_duty <- sample(agents_in$id[agents_in$role == 2], 9)
  
  agents_in$id[agents_in$id %in% c(day_duty, night_duty) &
                 !(agents_in$state %in% c("I")) &
                 !agents_in$q_status]
}

calc_betas <- function(contac_mat, R0, pSym) {
  a <- eigen(contac_mat)$value[1]
  BI <- R0 / ((pSym + (2/3)*(1-pSym)) * a)
  if (BI > 1) {
    stop("Beta I is greater than 1!")
  }
  list(BI=BI, BA=(2/3)*BI)
  # avg_inf <- (sum(pop * c(15, 6, 10))) / sum(pop)
  # R0 / (avg_inf * (pSym + ((2/3) * (1 - pSym))))
}

# calc_betas <- function(gamma, R0) {
#   inf_vals <- c(15/31, 6/31, 10/31)
#   BI <- sapply(inf_vals, function(x) x*(1/gamma)*R0)
#   BA <- sapply(inf_vals, function(x) x*(2/3)*(1/gamma)*R0)
#   
#   list(BI=BI,BA=BA)
# }

# contact_mat = contact matrix where C_ij is the average number of contacts from an
# individual in group i to people in group j
# pop = vector containing the number of people in each sub group
# pop = c(<day>, <night>, <residents>)
make_valid_contact_mat <- function(contact_mat, pop) {
  (diag(pop)%*%contact_mat + t(contact_mat%*%diag(pop)))/(2*outer(pop, pop, "+")/2)  
}

#agents_in=new_df
exposure_fun <- function(agents_in, contact_mat, BI, BA, eir, delta_t) {
  #on_site <- nrow(agents_in)
  on_site <- sapply(c(1,2,0),function(x){length(which(agents_in$role == x & agents_in$on_site))}) 
  total <- sapply(c(1,2,0),function(x){length(which(agents_in$role == x))})
  
  valid_matrix <- make_valid_contact_mat(contact_mat, total)  
  #Vector version of calculating number of infectious by role - note the order must conform to matrix construction below
  I <- sapply(c(1,2,0),function(x){
    length(which(agents_in$role == x &
                   agents_in$state == "I" &
                   !agents_in$q_status &
                   agents_in$on_site))
  })
  A <- sapply(c(1,2,0),function(x){
    length(which(agents_in$role == x &
                   agents_in$state == "A" &
                   !agents_in$q_status &
                   agents_in$on_site))
  })
  
  #Number of contacts per person (day,night,resident)
  # valid_matrix <- matrix(c(7,1,7,
  #                         1,3,2,
  #                         7,2,1),nrow = 3,ncol = 3)
  # BI <- calc_beta_inf(R0, pSym = pI, total) 
  # BA <- (2/3) * BI
  #avg_inf_contacted <- ((BI * I) + (BA * A)) * (delta_t / on_site)
  
  avg_inf_contacted <- as.vector(valid_matrix %*% (BI*I + BA*A)/on_site)
  num_inf_contacted <- sapply(c(1:3), function(x){
    rpois(total[x], avg_inf_contacted[x])
  }) %>% 
    unlist() #unlist bc this output is a list - it should always conform to agents_in
  
  exposed_index <- agents_in$id[agents_in$state == "S" &
                                  num_inf_contacted > 0 &
                                  !agents_in$q_status &
                                  agents_in$on_site]
  
  # eir
  if (eir > 0) {
    probE <- runif(nrow(agents_in))
    eir_exposed <- agents_in$id[agents_in$role > 0 &
                                  agents_in$state == "S" &
                                  !agents_in$q_status &
                                  agents_in$on_site &
                                  probE < eir]
    exposed_index <- c(exposed_index, eir_exposed)
  }
  
  exposed_index
}

vaccinate <- function(agents, pstaff_to_vax, pres_to_vax) {
  staff <- length(which(agents$role > 0))
  res <- nrow(agents) - staff
  vaxed_staff <- sample(agents$id[agents$role > 0], staff * pstaff_to_vax)
  vaxed_res <- sample(agents$id[agents$role == 0], res * pres_to_vax)
  rVax <- runif(nrow(agents))
  recovered <- agents$id[agents$id %in% c(vaxed_staff, vaxed_res) & rVax < 0.95]
  
  agents$state <- ifelse(agents$id %in% recovered, "R", "S")
  agents$vaccinated <- ifelse(agents$id %in% c(vaxed_res, vaxed_staff), TRUE, FALSE)
  
  agents
}

testing_fun <- function(agents, sensitivity, specificity) {
  probQ <- runif(nrow(agents))
  test_params <- list(c("I", "A"), sensitivity)
  true_inf <- pmap(test_params, function(state, senstivity) {
    agents$id[agents$state == state &
                probQ < sensitivity &
                !agents$q_status]
  }) %>% 
    unlist()
  
  false_pos <- agents$id[agents$state %in% c("S", "E", "R") &
                           probQ > specificity &
                           !agents$q_status]
  return(c(true_inf, false_pos))
}
