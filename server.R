library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(reshape2)
library(vsn)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
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
