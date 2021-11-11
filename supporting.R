# supporting functionality

run_button <- function(disabled = FALSE) {
  button <- actionButton("start", "Run")
  if (disabled) {
    result <- tagList(disabled(button),
                      htmlOutput("runMessage"))
  } else {
    result <- button
  }
  result
}

# initial view for show and run mode
createInitialView <- function(disableRun = FALSE) {
  tagList(
    checkboxInput("affine", "Affine normalization", value = TRUE),
    checkboxInput("refset", "Use reference data", value = FALSE),
    conditionalPanel(condition = 'input.refset',
                     selectInput("reffactor", "Reference annotation factor", choices = list(), selected = "nothing"),
                     selectInput("reflevel",  "Reference annotation level", choices = list())
    ),
    run_button(disableRun),
    tags$p(),
    uiOutput("status")
  )
}