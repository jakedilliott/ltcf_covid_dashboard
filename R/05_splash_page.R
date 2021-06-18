# snf_testing_abm/R/04_splash_page.R

splashpage <- function() {
  column(
    width = 8, offset = 2,
    box(
      width = 12,
      tags$p(
        "Idk just write some stuff here you know, like explaining why this thing is important."
      )
    )
  )
}