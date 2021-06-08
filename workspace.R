library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(reshape2)
library(vsn)
library(hexbin)
library(shinyjs)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  # Set appropriate options
  #options("tercen.serviceUri"="http://tercen:5400/api/v1/")
  #options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
  #options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
  #options("tercen.username"= "admin")
  #options("tercen.password"= "admin")
  ctx <- tercenCtx()
  return(ctx)
}
####
############################################

ui <- shinyUI(uiOutput("body"))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getData(session)
  })
  
  output$body <- renderUI({
    tagList(
      shinyjs::useShinyjs(),
      tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
      tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
      plotOutput("main.plot", height = "800px"))
  }) 
  
  output$main.plot <- renderPlot({
    data <- dataInput()
    lapply(data, FUN = function(vsn) { meanSdPlot(vsn) } )
  })
  
})

vsnOperator = function(df, calib.type) {
  vsn2(as.matrix(acast(df, .ri ~ .ci, value.var = ".y")), calib = calib.type)
}

getData <- function(session){
  ctx <- getCtx(session)

  normalization   <- ifelse(is.null(ctx$op.value('Normalization')), 'affine', ctx$op.value('Normalization'))
  
  data            <- ctx %>% select(.y, .ri, .ci)
  grouping_values <- "NA"
  if (length(ctx$colors) > 1) {
    grouping_values <- droplevels(interaction(ctx$select(ctx$colors)))
  } else if (length(ctx$colors) == 1) {
    grouping_values <- ctx$select(ctx$colors) %>% pull()
  }
  data <- data %>% mutate(grouping = grouping_values)
  dlply(data, ~grouping, .fun = vsnOperator, calib.type = normalization)
}

runApp(shinyApp(ui, server))  
