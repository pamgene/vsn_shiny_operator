library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(reshape2)
library(vsn)
library(hexbin)
library(shinyjs)

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

server <- shinyServer(function(input, output, session) {
  
  context <- reactive({
    getCtx(session)
  })
  
  plotData <- reactive({
    getPlotData(session)
  })
  
  returnData <- reactive({
    getReturnData(session)
  })
  
  output$body <- renderUI({
    tagList(
      shinyjs::useShinyjs(),
      tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
      tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
      plotOutput("main.plot", height = "600px"),
      HTML(paste("<center><h5>Click below to send data back to Tercen</h5>", actionButton("button", "Transform data")),"</center>")
    )
  }) 
  
  output$main.plot <- renderPlot({
    data <- plotData()
    lapply(data, FUN = function(vsn) { meanSdPlot(vsn) } )
  })
  
  observeEvent(input$button, {
    shinyjs::disable("button")
    
    ctx <- context()
    returnData() %>% 
      ctx$addNamespace() %>% 
      ctx$save()
  })
  
})

vsnReturn <- function(vsn){
  result <- melt(attr(vsn, "hx"))
  colnames(result) = c(".ri", ".ci", "Hvsn")
  result
}

vsnOperator <- function(df, calib.type) {
  vsn2(as.matrix(acast(df, .ri ~ .ci, value.var = ".y")), calib = calib.type)
}

getPlotData <- function(session){
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

getReturnData <- function(session) {
  plot_data <- getPlotData(session)
  result    <- ldply(plot_data, .fun = vsnReturn)
  result %>% select(-grouping)
}
