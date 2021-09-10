# supporting functionality

run_button <- function(disabled = FALSE) {
  button <- actionButton("start", "Run")
  if (disabled) {
    button <- disabled(button)
  }
  button
}

upload_html <- function(enableButton = TRUE) {
  if (enableButton) {
    button_html <- actionButton("button", "Transform data") 
  } else {
    button_html <- disabled(actionButton("button", "Transform data"))
  }
  HTML(paste("<center><h5>Click below to send data back to Tercen</h5>", button_html),"</center>")
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
    verbatimTextOutput("status"),
    tags$hr(),
    upload_html(FALSE)
  )
}