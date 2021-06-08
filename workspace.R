library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(reshape2)
library(vsn)

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
    plotOutput("main.plot", height = "800px")
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

  normalization = ifelse(is.null(ctx$op.value('Normalization')), 'affine', ctx$op.value('Normalization'))
  
  data <- ctx %>% select(.y, .ri, .ci)
  if (length(ctx$colors) >= 1) {
    data <- data %>% mutate(grouping = "NA")
  } else {
    data <- data %>% mutate(grouping = "NA")
  }
  dlply(data, ~grouping, .fun = vsnOperator, calib.type = normalization)
}

runApp(shinyApp(ui, server))  
