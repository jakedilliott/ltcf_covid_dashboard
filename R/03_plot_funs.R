states <- c("S", "E", "I", "A", "R")

# A function for creating integer y-axis values
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

preprocess <- function(df) {
  df <- mutate(df, "role" = ifelse(role > 0, "Staff", "Residents"))
  df_long <- pivot_longer(df, cols = all_of(states), names_to = "state", values_to = "count")
  
  duration <- max(df$time) - min(df$time)
  total_days <- 34 * duration # 25 day staff, 9 night staff
  wd_missed <- df_long %>%
    filter(role == "Staff" & q_status & on_site & !vaccinated) %>%
    group_by(run) %>%
    summarise("wd_missed" = sum(count)) %>%
    ungroup() %>%
    mutate("p_wd_missed" = wd_missed/total_days)
  
  isolation <- df_long %>%
    filter(q_status) %>%
    group_by(run, time, role) %>%
    summarise("n_isolated" = sum(count)) %>%
    ungroup()
  
  infections <- df %>% 
    select(run, time, role, new_inf) %>%
    group_by(run, role) %>%
    mutate("cum_inf" = cumsum(new_inf)) %>%
    ungroup()  
  
  list("wd_missed" = wd_missed, "isolation" = isolation, "infections" = infections)
}

active_inf_prep <- function(df) {
  df %>% 
    mutate(role = ifelse(role > 0, "Staff", "Resident")) %>%
    select(run, time, role, I, A) %>%
    filter(!q_status) %>%
    pivot_longer(I, A, names_to = "state", values_to = "n") %>%
    group_by(run, time, role) %>%
    summarise(n = sum(count)) %>%
    ungroup()
}

isolation_prep <- function(df) {
  df %>%
    mutate(role = ifelse(role > 0, "Staff", "Resident")) %>%
    filter(q_status) %>%
    pivot_longer(cols = c("S", "E", "I", "A", "R"),
                 names_to = "state", values_to = "count") %>%
    group_by(run, time, role) %>%
    summarise("n" = sum(count)) %>%
    ungroup()
}

quantile_fun <- function(data, column) {
  out <- data %>%
    group_by(time, role) %>%
    summarise(across({{ column }}, list("n_med"   = ~quantile(., 0.5),
                                        "n_lower" = ~quantile(., 0.1),
                                        "n_upper" = ~quantile(., 0.9)),
                     .names = "{.fn}")) %>%
    ungroup() %>%
    mutate(time = as.Date(time, origin = "1970-01-01"))
    # mutate(n_med = round(loess(n_med~time, span = 0.15)$fitted, digits = 1),
    #        time = as.Date(time, origin = "1970-01-01"))
  out[out < 0] <- 0
  
  out
}

plot_quantiles <- function(to_plot, role_color, ...) {
  fig <- plot_ly(to_plot, x = ~time, y = ~n_upper,
                 type = "scatter", mode = "lines",
                 line = list(color = "transparent"),
                 showlegend = FALSE, name = "Upper")
  fig <- fig %>% add_trace(y = ~n_lower, type = "scatter", mode = "lines",
                           fill = "tonexty", fillcolor = role_color,
                           line = list(color = "transparent"),
                           showlegend = FALSE, name = "Lower")
  fig <- fig %>% add_trace(y = ~n_med, type = "scatter", mode = "lines",
                           line = list(color = "black"), name = "Median")
  fig <- fig %>% layout(xaxis = list(title = "Date"),
                        hovermode = "x",
                        ...)
  return(fig)
}

plot_cum_inf <- function(inputs) {
  to_plot <- inputs %>%
    quantile_fun(., column = "cum_inf") %>%
    split(., .$role)
  
  fig1 <- plot_quantiles(to_plot$Residents,
                         role_color = "rgba(255, 0, 0, 0.3")
  fig2 <- plot_quantiles(to_plot$Staff,
                         role_color = "rgba(0, 0, 255, 0.3")
  
  y_max <- max(c(to_plot$Residents$n_upper, to_plot$Staff$n_upper))
  
  subplot(fig1, fig2, margin = 0.05) %>%
    layout(
      yaxis = list(title = "Cumulative Infected Residents",
                   range = c(0, y_max + 2)),
      yaxis2 = list(title = "Cumulative Infected Stafff",
                    range = c(0, y_max + 2))
    ) %>%
    config(displaylogo = FALSE)
}

plot_isolated <- function(inputs) {
  to_plot <- inputs %>%
    quantile_fun(., column = "n_isolated") %>%
    split(., .$role)
  
  fig1 <- plot_quantiles(to_plot$Resident,
                         role_color = "rgba(255, 0, 0, 0.3")
  fig2 <- plot_quantiles(to_plot$Staff,
                         role_color = "rgba(0, 0, 255, 0.3")
  
  y_max <- max(c(to_plot$Resident$n_upper, to_plot$Staff$n_upper))
  
  subplot(fig1, fig2, margin = 0.05) %>%
    layout(
      yaxis = list(title = "Residents in Isolation",
                   range = c(0, y_max + 2)),
      yaxis2 = list(title = "Staff in Isolation",
                    range = c(0, y_max + 2))
    ) %>%
    config(displaylogo = FALSE)
}

active_infected <- function(results) {
  to_plot <- results %>% active_inf_prep() %>% calc_quantiles()
  
  if (length(to_plot) > 0) {
    fig1 <- plot_quantiles(to_plot$Resident,
                           role_color = "rgba(255, 0, 0, 0.3)")
    fig2 <- plot_quantiles(to_plot$Staff,
                           role_color = "rgba(0, 0, 255, 0.3)")   
  } else {
    df <- data.frame(time = as.Date(results$time, origin = "1970-01-01"), 
                     zeroes = rep_along(results$time, 0))
    fig1 <- plot_ly(df, x = ~time, y = ~zeroes,
                    type = "scatter", mode = "lines",
                    line = list(color = "black"),
                    showlegend = FALSE) 
    fig2 <- fig1
  }

  y_max <- max(c(to_plot$Resident$n_upper, to_plot$Staff$n_upper))
  
  subplot(fig1, fig2, margin = 0.05) %>%
    layout(
      yaxis = list(title = "Infectious Residents",
                   range = c(0, y_max + 2)),
      yaxis2 = list(title = "Infectious Staff",
                    range = c(0, y_max + 2))
    ) %>%
    config(displaylogo = FALSE)
}

plot_density <- function(column, role_color = "blue", ...) {
  dens <- density(column)
  df <- data.frame(x = unlist(dens$x),
                   y = unlist(dens$y))
  mean_x <- mean(column)
  median_x <- median(column)
  if (mean_x > 1) {
    df$text <- paste(round(df$x), "Worker Days Missed")
  } else {
    df$text <- paste0(round(df$x * 100, digits = 1), "% of Worker Days Missed")
  }
  
  p <- ggplot(df, aes(x, y)) +
    geom_point(aes(text = text), color = role_color, alpha = 0.3,
               size = 0.001, show.legend = FALSE) +
    geom_area(fill = role_color, alpha = 0.3, show.legend = FALSE) +
    geom_vline(xintercept = mean_x, linetype = "dashed", size = 0.8) +
    geom_vline(xintercept = median_x, linetype = "solid", size = 0.8) +
    theme_bw(base_size = 14) +
    labs(...)
  if (mean_x < 1) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }
  ggplotly(p, tooltip = "text") %>%
    layout(
      # xaxis = list(range = c(0, max(df$x))),
      yaxis = list(title = "", showticklabels = FALSE),
      hovermode = "x") %>%
    config(displaylogo = FALSE)
}

workdays_density <- function(inputs) {
  plot_density(inputs$wd_missed, role_color = "blue",
               x = "Work Days Missed", y = "",
               subtitle = "Worker Days Missed Due to Illness")
}

percent_workdays_density <- function(results) {
  duration <- max(results$time) - min(results$time)
  total_days <- 34 * duration # 9 day, 25 day
  
  to_plot <- results %>%
    filter(role > 0, q_status, on_site) %>%
    pivot_longer(cols = c("S", "E", "I", "A", "R"),
                names_to = "state", values_to = "count") %>%
    group_by(run) %>%
    summarise("n" = sum(count)) %>%
    ungroup() %>%
    mutate(n = n/total_days)
  
  plot_density(to_plot, role_color = "blue",
               x = "Percent Worker Days Missed", y = "",
               subtitle = "Percent of Worker Days Missed due to Illness")
}

summarise_runs <- function(results) {
  results_long <- pivot_longer(results, cols = all_of(states),
                               names_to = "state", values_to = "count")
  
  infectious <- results_long %>%
    filter(!q_status & state %in% c("I", "A")) %>%
    group_split(run) %>%
    map_dbl(~sum(.x$count)) %>%
    ci(., ci = 0.9) %>%
    round()
  
  cum_inf <- results %>%
    group_split(run) %>%
    map_dbl(~sum(.x$new_inf)) %>%
    ci(., ci = 0.9) %>%
    round()
  
  days_missed <- results_long %>%
    filter(role > 0, on_site, q_status) %>%
    group_split(run) %>%
    map_dbl(~sum(.x$count)) %>%
    ci(., ci = 0.9) %>%
    round()
  
  list("cum_inf" = cum_inf, "days_missed" = days_missed)
}

makeMetricsBox <- function(confidenceIntervals, metric) {
  text <- case_when(metric == "cum_inf" ~ "Cumulative infections",
                    metric == "days_missed" ~ "Cumulatvie worker-days missed")
  renderInfoBox({
    valueBox(
      confidenceIntervals[["mean"]],
      paste0(text, ", (90% CI: ",
             confidenceIntervals[["lower"]], ", ",
             confidenceIntervals[["upper"]], ")"),
      color = "blue")
  })
}

statesFacet <- function(results) {
  to_plot <- results %>%
    filter(state %in% c("I", "A")) %>%
    count(role, state, time, run) %>%
    mutate(state = factor(state, levels = c("I", "A"), labels = c("Symptomatic", "Asymptomatic")),
           role = factor(role, labels = c("Res", "Day", "Night")),
           time = as.Date(time, origin = "1970-01-01"))

  ggplot(to_plot, aes(time, n, group = run)) +
    geom_line(aes(color = role), alpha = 0.3) +
    facet_grid(cols = vars(state), rows = vars(role)) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Number Infectious",
         subtitle = "Number of Infections Agents by State and Role")
}