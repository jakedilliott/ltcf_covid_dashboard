lapply(list.files("R", full.names = TRUE), source, echo = FALSE)

input_box_col <- "primary"
side_width <- 180

header <- dashboardHeader(
  title = "LTCF Sim", titleWidth = side_width
)

sidebar <- dashboardSidebar(
  disable = TRUE, 
  width = side_width,
  sidebarMenu(
    # menuItem('Home', tabName = 'home', icon = icon('home')),
    menuItem('Dashboard', tabName = 'simdash', icon = icon('dashboard'))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ),
  tabItems(
    tabItem(
      # Splash page -----
      tabName = 'home',
      splashpage()
    ),

    tabItem(
      # Dashboard -----
      tabName = 'simdash',
      
      fluidRow(
        column(width = 8, offset = 2,
        box(title = "Getting started", width = NULL, status = "primary",
            solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            p("The LTCF Simulation dashboard is a tool to help guide decisions on
              COVID-19 testing and protocol within the context of Long Term Care 
              Facilities. The simulation uses agent based modeling to approximate
              the interactions between 3 groups: facility day staff, facility night staff,
              and residents of the facility. By default the simulation is run using
              a duration of 45 days, and the metrics shown on the right are calculated
              after running the simulation for a number of times (20 times by default).
              Please be patient and only click the ", strong("'Run'"), " button once
              every time you change one or more of the parameters. You may change the 
              duration of the simulation or number runs, by opening the", strong("'Advanced +'"),
              "inputs panel.", 
              "Doing so may cause longer wait times for runs to be displayed."),
            p("It is understood that decision makers do not have the power in the real
              world to change all of the inputs availabe on this dashboard, the intended
              use is that the user will set the parameters that are outside of their
              control then experiment with the inputs that are within their control. ")
            )
        )
      ),
      
      column(width = 4,
             column(width = 6,
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = FALSE,
                        title = "Facility", 
                        numericInput("day", "Day Staff",
                                     min = 0, value = 125, step = 10),
                        numericInput("night", "Night Staff",
                                     min = 0, value = 45, step = 10),
                        numericInput("res", "Residents",
                                     min = 0, value = 90, step = 10)),
                    
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = FALSE,
                        title = "Starting Conditions",
                        numericInput("init_inf_staff", "Initial infectious staff (%)",
                                     value = 2, min = 0, max = 100, step = 1),
                        numericInput("init_inf_res", "Initial infectious residents (%)",
                                     value = 0, min = 0, max = 100, step = 1),
                        numericInput("R_0_staff", "Staff that have been vaccinated (%)",
                                     value = 40, min = 0, max = 100, step = 1),
                        
                        numericInput("R_0_res", "Residents that have vaccinated (%)",
                                     value = 78, min = 0, max = 100, step = 1))),
             
             box(width = 6, status = "success", solidheader = TRUE,
                 actionButton("run", "Run", icon = icon("play"))),
             
             column(width = 6,
                    # Testing
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = FALSE,
                        title = "Testing",
                        selectInput("test_type", "Type of COVID-19 test",
                                    choices = list("PCR, 2 day result delay" = "pcr",
                                                   "Same day rapid test (Abbott BinaxNow)" = "rapid",
                                                   "Custom" = "custom")), 
                        selectInput("test_freq", "Testing cadence (days)",
                                    choices = list("No Testing" = "none",
                                                   "Monthly" = "month", 
                                                   "Every 2 Weeks" = "2week",
                                                   "Weekly" = "week", 
                                                   "Every 3 Days" = "3day", 
                                                   "Every 2 Days" = "2day", 
                                                   "Daily" = "daily"),
                                    selected = "week"),
                        conditionalPanel(
                          "input.test_type == 'custom'",
                          sliderInput("test_delay", "Test result delay (days)",
                                      value = 3, min = 0, max = 10, step = 1),
                          numericInput("test_sensitivity", "Test sensitivity (%)",
                                       value = 95, min = 0, max = 100),
                          numericInput("test_specificity", "Test specificity (%)",
                                       value = 99.5, min = 0, max = 100)
                        )),
                    
                    
                    # Advanced
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                        title = "Advanced",
                        tags$h3("Behavior"),
                        numericInput("pIR", "Probability staff will recognize symptoms and self isolate",
                                     value = 95, min = 0, max = 100, step = 1),
                        
                        tags$h3("Epidemiology"),
                        numericInput("R0", "R0 (Expected number of cases directly generated by one case in a population where all individuals are susceptible to infection.)",
                                     value = 1.4, min = 0.1, max = 5, step = 0.5),
                        numericInput("inf_per_100k", 
                                     tags$p("Daily Cases Per 100,000. Sate and local data can be found using the",
                                            a(href="https://www.nytimes.com/interactive/2021/us/covid-cases-deaths-tracker.html", "New York Times Coronavirus Tracker")),
                                     value = 10, min = 0, max = 100, step = 1),
                        tags$h3("Run Pamaters"),
                        dateInput("start_date", "Start Date"),
                        numericInput("sim_dur", "Duration (days)",
                                     value = 45, min = 25, max = 90, step = 5),
                        numericInput("n_runs", label = "Number of runs",
                                     min = 1, max = 25, value = 15, step = 1))
             )
      ),
      
      
      # Plots Section =====
      column(width = 8,
             fluidRow(
               valueBoxOutput("infected_agents_box", width = 4),
               valueBoxOutput("missed_workdays_box", width = 4),
               # valueBoxOutput("per_missed_workdays_box", width = 4)
             ),
             
             box(width = NULL, title = "Simulation Inputs Table",
                 collapsible = TRUE, collapsed = TRUE,
                 tableOutput("input_table")),
             
             tabBox(width = NULL, side = "left", selected = "Worker Days Missed",
                    title = "",
                    tabPanel("Worker Days Missed", plotlyOutput("workdays")),
                    tabPanel("Percent Worker Days Missed", plotlyOutput("workdaysPercent"))),
             
             box(width = NULL, title = "Residents and Staff in Isolation",
                 plotlyOutput("iso_plot")),
             
             box(width = NULL, title = "Cumulative Infections Over Time",
                 plotlyOutput("inf_plot"))
      )
      
    )
  )
)

ui <- dashboardPage(header = header, body = body, sidebar = sidebar)