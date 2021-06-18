# calculating analysis for model outputs
summarise_daily <- function(data) {
  out <- data %>%
    count(time, role, q_status, on_site, vaccinated, state) %>%
    pivot_wider(names_from = "state", values_from = n, values_fill = 0)
  
  states <- c("S", "E", "A", "I", "R")
  if (ncol(out) < 10) {
    missing <- states[!(states %in% colnames(out))]
    for (i in missing) {
      out <- out %>% mutate("new_col" = 0)
      colnames(out)[length(out)] <- i
    }
  }
  
  return(out)
}

count_daily_inf <- function(new_roster, current_roster) {
  new_inf_df <- new_roster[new_roster$state == "E" & current_roster$state == "S", ]
  new_inf_df %>%
    count(time,role,on_site,q_status,vaccinated, name = "new_inf")
}

# Calculate confidence intervals
ci <- function(x, ci) {
  a <- mean(x)
  s <- sd(x)
  n <- length(x)
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  
  c(mean = a, lower = a-error, upper = a+error)
}
