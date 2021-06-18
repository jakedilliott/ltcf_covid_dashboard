server <- function(input, output, session) {
  
  # Don't throw an error if an input is blank
  observe({
    req(input$day,
        input$night,
        input$res,
        input$init_inf_staff,
        input$init_inf_res,
        input$R_0_staff,
        input$R_0_res,
        input$test_sensitivity,
        input$test_specificity,
        cancelOutput = TRUE)
    
  })
  
  agents_df <- reactive({
    validate(
      need(input$day > 25, "Facility Input Error: Number of Day Staff must be more than 25"),
      need(input$night > 25, "Facility Input Error: Number of Night Staff must be more than 25"),
      need(input$res > 25, "Facility Input Error: Number of Residents must be more than 25")
    )
    make_agents(input$day, input$night, input$res)
  })
  
  output$input_table <- renderTable({
    total = sum(input$day, input$night, input$res)
    out <- data.frame(
      "Variable" = c('Total Agents',
                     'Initial infectious staff',
                     'Initial infectious residents',
                     'Vaccinated Staff',
                     'Vaccinated Residents',
                     'Total Work Days'),
      "Actual Input" = c(total,
                         round((input$day+input$night) * (input$init_inf_staff / 100)),
                         round(input$res * input$init_inf_res / 100), 
                         round(sum(input$day, input$night) * input$R_0_staff / 100), 
                         round(input$res * input$R_0_res / 100), 
                         sum(input$day, input$night) * input$sim_dur)
    )
  }, striped = TRUE, width = "300px", spacing = "m", bordered = TRUE)
  
  observe({
    if (input$test_type != "custom") {
      parms <- case_when(
        input$test_type == "pcr"    ~ list(2, 99.5),
        input$test_type == "rapid"  ~ list(0, 99.5)
        # input$test_type == "none"   ~ c(0, 99.5)
      )
      updateSliderInput(session, "test_delay", value = parms[1])
      updateNumericInput(session, "test_specificity", value = parms[2])
    } 
  })
  
  test_freq <- reactive({
    case_when(
      input$test_freq == "none"  ~ 9999,
      input$test_freq == "month" ~ 30, 
      input$test_freq == "2week" ~ 14,
      input$test_freq == "week"  ~ 7,
      input$test_freq == "3day"  ~ 3,
      input$test_freq == "2day"  ~ 2,
      input$test_freq == "day"   ~ 1,
    )
  })
  
  test_sensitivity <- reactive({
    case_when(
      input$test_type == "pcr"    ~ list(95, 95),
      input$test_type == "rapid"  ~ list(64.2, 35.8),
      input$test_type == "none"   ~ list(100, 100),
      input$test_type == "custom" ~ list(input$test_sensitivity, input$test_sensitivity)
    )
  })
  
  # Multi Run Plots -----
  results <- eventReactive(input$run, {
  # results <- reactive({
    validate(
      need((input$R_0_staff + input$init_inf_staff) < 99,
           "Starting Inputs Error: Initial infectious staff and initial vaccinated staff are overlapping (Their sum is more than 100%)"),
      need((input$R_0_res + input$init_inf_res) < 99,
           "Starting Inputs Error: Initial infectious residents and initial vaccinated residents are overlapping (Their sum is more than 100%)"),
      need(input$sim_dur <= 60, "Advanced Input Error: Pick a duration that is 60 or less, this is to minimize wait times while the model runs"),
      need(input$n_runs <= 50, "Advanced Input Error: Pick a number of runs that is 50 or less, this is to minimize wait times while the model runs"),
      need(input$R0 > 0.5, "Advanced Input Error: R0 must be greater than 0.5"),
      need(input$pIR <= 100, "Advanced Input Error: Probability that a symptomatic case will recognize symptoms cannot be greater than 100%"), 
      need(max(input$test_sensitivity) <= 100, "Testing Input Error: Test sensitivity cannot be greater than 100%"),
      need(max(input$test_specificity) <= 100, "Testing Input Error: Test specificity cannot be greater than 100%")
    )
    
    results <- multi_run(nruns = input$n_runs, lock_seed = TRUE,
                         inputs = agents_df(),
                         date_start = input$start_date,
                         sim_dur = input$sim_dur,
                         test_delay = input$test_delay,
                         test_freq = test_freq(),
                         pIR = input$pIR / 100,
                         test_sensitivity = map_dbl(test_sensitivity(), ~.x/100),
                         test_specificity = input$test_specificity / 100,
                         R0 = input$R0,
                         inf_per_100k = input$inf_per_100k,
                         init_inf_staff = input$init_inf_staff / 100,
                         init_inf_res = input$init_inf_res / 100,
                         R_0_staff = input$R_0_staff / 100,
                         R_0_res = input$R_0_res / 100)
    
    to_plot <- preprocess(results)
    summaries <- summarise_runs(results)
    list("to_plot" = to_plot, "summaries" = summaries)
  })
  
  # Info Boxes ==========  
  output$infected_agents_box <- makeMetricsBox(results()$summaries$cum_inf,
                                               "cum_inf")
  # output$infected_agents_box <- renderInfoBox({
  #   valueBox(
  #     results()$summaries$cum_inf[["mean"]],
  #     paste0("Cumulative infections per run, (90% CI: ",
  #            results()$summaries$cum_inf[["lower"]], ", ",
  #            results()$summaries$cum_inf[["upper"]], ")"),
  #     color = "blue")
  # })
  
  output$missed_workdays_box <- makeMetricsBox(results()$summaries$days_missed,
                                               "days_missed")
  # output$missed_workdays_box <- renderInfoBox({
  #   valueBox(
  #     results()$summaries$days_missed[["mean"]],
  #     paste0("Cumulative Worker Days Missed, (90% CI: ",
  #            results()$summaries$days_missed[["lower"]], ", ",
  #            results()$summaries$days_missed[["upper"]], ")"),
  #     color = "blue")
  # })
  
  # Plots ========== 
  output$inf_plot <- renderPlotly({
    plot_cum_inf(results()$to_plot$infections) 
  })
  
  output$iso_plot <- renderPlotly({
    plot_isolated(results()$to_plot$isolation)
  })
  
  output$workdays <- renderPlotly({
    # workdays_density(sim_multi())
    plot_density(results()$to_plot$wd_missed[["wd_missed"]],
                 x = "Work Days Missed", y = "",
                 subtitle = "Worker Days Missed Due to Illness")
  })
  
  output$workdaysPercent <- renderPlotly({
    plot_density(results()$to_plot$wd_missed[["p_wd_missed"]],
                 x = "Percent Worker Days Missed", y = "",
                 subtitle = "Percent of Worker Days Missed due to Illness")
  })
  
  # output$debug_table <- renderTable({
  #   count(sim_multi(), time, run, role, q_status)
  # })
}