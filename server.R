library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(hexbin)
library(shinyjs)
source("fvsn.R")

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
  
  results <- reactiveValues()
  viewer  <- reactiveValues(view = "run")
  
  context <- reactive({
    getCtx(session)
  })
  
  inputData <- reactive({
    getData(session)
  })
  
  returnData <- reactive({
    results$hdf
  })
  
  getComputedResults <- reactive({
    getCtxResults(session)
  })
  
  output$body <- renderUI({
    view <- viewer$view
    if (isRunView(view)) {
      tagList(
        checkboxInput("affine", "Affine normalization", value = TRUE),
        checkboxInput("refset", "Use reference data", value = FALSE),
        conditionalPanel(condition = 'input.refset',
                         selectInput("reffactor", "Reference annotation factor", choices = list(), selected = "nothing"),
                         selectInput("reflevel",  "Reference annotation level", choices = list())
        ),
        actionButton("start", "Run"),
        verbatimTextOutput("status"),
        tags$hr(),
        disabled(actionButton("switchToResult", "Switch to Results View"))
      )
    } else if (isResultView(view)) {
      computedResults     <- getCtxResults(session)
      if (!is.null(computedResults)) {
        results$df        <- computedResults$df
        results$vsnResult <- computedResults$vsnResult
        results$hdf       <- computedResults$hdf
        results$reslist   <- computedResults$reslist
        
        tagList(
          selectInput("group", "Show meanSdPlot for", choices = results$vsnResult$grp),
          plotOutput("msplot"),
          verbatimTextOutput("ref"),
          tags$hr(),
          actionButton("switchToRun", "Switch to Run Analysis View"),
          tags$hr(),
          HTML(paste("<center><h5>Click below to send data back to Tercen</h5>", actionButton("button", "Transform data")),"</center>")
        )
      }
    }
  })
  
  observeEvent(viewer$view, {
    view <- viewer$view
    if (isRunView(viewer$view)) {
      nid <<- showNotification("Press Run to start the analysis.", duration = NULL, type = "message", closeButton = FALSE)
    } else {
      removeNotification(nid)
    }
  })
  
  observeEvent(input$switchToRun, {
    if (input$switchToRun == 0) {
    } else {
      viewer$view <- "run"
    }
  })
  
  observeEvent(input$switchToResult, {
    if (input$switchToResult == 0) {
    } else {
      viewer$view <- "showResult"
    }
  })
  
  observeEvent(getComputedResults(), {
    results <- getComputedResults()
    if (is.null(results)) {
      viewer$view <- "run"
    } else {
      viewer$view <- "showResult"
    }
  })
  
  observe({
    data     <- inputData()
    df       <- data$df
    col_df   <- data$col_df
    color_df <- data$color_df
    array_labels <- colnames(col_df)
    
    updateSelectInput(session,  inputId = "reffactor", choices = array_labels)
    rf <- factor(col_df[[array_labels[1]]])
    updateSelectInput(session, inputId = "reflevel", choices = levels(rf))
    
    output$status = renderText({
      
      observeEvent(input$reffactor, {
        rf <- factor(col_df[[input$reffactor]])
        updateSelectInput(session, inputId = "reflevel", choices = levels(rf))
      })
      
      isolate({
        bRef <- input$refset
      })
      
      if (input$start > 0) {
        showNotification(ui = "Running VSN ...", id = nid, type = "message", closeButton = FALSE, duration = NULL)
        if (!is.null(color_df) && ncol(color_df) >= 1) {
          grouping <- droplevels(interaction(color_df))
        } else {
          grouping <- "Main"
        }
        df <- data.frame(df, grp = grouping)
        hdf <- vsnResult <- NULL
        isolate({
          if (!bRef) {
            vsnResult <- df %>% group_by(grp) %>% do(vsn0(., normalization = input$affine))
            hdf       <- vsnResult %>% group_by(grp) %>% do(vsnh(.))
            reslist   <- list(vsnResult = vsnResult)
          } else {
            df$RefFactor <- factor(df[[input$reffactor]])
            df$RefFactor <- relevel(df$RefFactor, ref = input$reflevel)
            vsnResult    <- df %>% group_by(grp) %>% do(vsnr(., normalization = input$affine))
            hdf          <- vsnResult %>% group_by(grp) %>% do(vsnh(.))
            reslist      <- list(vsnResult = vsnResult)
          }
          settings = list(affine = input$affine,
                          refset = input$refset,
                          reffactor = input$reffactor,
                          reflevel = input$reflevel)
        })
        reslist$settings <- settings
        reslist$df <- df
        hdf        <- hdf[,-1]
        hdf        <- hdf[!is.na(hdf$Hvsn),]
        hdf$.ri    <- as.double(hdf$.ri)
        hdf$.ci    <- as.double(hdf$.ci)

        # save objects in tercen context
        saveData(session, list(df = df, vsnResult = vsnResult, hdf = hdf, reslist = reslist))
        shinyjs::enable("switchToResult")
        showNotification(ui = "Done", id = nid, type = "message", closeButton = FALSE)
        return("Done")
      } else {
        return(".")
      }
    })
  })

  observeEvent(input$button, {
    shinyjs::disable("button")
    
    ctx <- context()
    returnData() %>% 
      ctx$addNamespace() %>% 
      ctx$save()
  })
  
  ## Results
  
  output$msplot = renderPlot({
    req(input$group)
    
    idx = which(input$group ==  results$vsnResult$grp)
    meanSdPlot(results$vsnResult$vsn[[idx[1]]])
  })
  
  output$ref = renderText({
    req(results$reslist)
    
    if(results$reslist$settings$refset){
      str = paste("Data with ", results$reslist$settings$reffactor," @",levels(results$reslist$df$RefFactor)[1]," used as reference data.", sep = "")
      return(str)
    } else {
      return("No reference data used.")
    }
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

getData <- function(session) {
  result <- list()
  ctx    <- getCtx(session)
  
  result$df       <- ctx %>% select(.y, .ri, .ci)
  result$col_df   <- ctx$cselect()
  if (identical(ctx$colors, list())) {
    result$color_df <- NULL 
  } else {
    result$color_df <- ctx$select(ctx$colors)
  }
  result
}

getWorkflowId = function(session){
  workflowId = getOption("tercen.workflowId")
  if (!is.null(workflowId)) return(workflowId)
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["workflowId"]])
}

getStepId = function(session){
  stepId = getOption("tercen.stepId")
  if (!is.null(stepId)) return(stepId)
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["stepId"]])
}

removeCurrentFiles <- function(session, workflowId, stepId) {
  ctx   <-  getCtx(session)
  files <- getFilesByWorkflowAndStep(ctx, workflowId, stepId)
  for (file in files) {
    ctx$client$fileService$delete(file$id, file$rev)
  }
}

saveData <- function(session, result_list) {
  ctx        <- getCtx(session)
  workflowId <- getWorkflowId(session)
  stepId     <- getStepId(session)
  workflow   <- ctx$client$workflowService$get(workflowId)
  
  fileDoc = FileDocument$new()
  fileDoc$name = "results"
  fileDoc$projectId = workflow$projectId
  fileDoc$acl$owner = workflow$acl$owner
  fileDoc$metadata$contentType = 'application/octet-stream'
  
  metaWorkflowId = Pair$new()
  metaWorkflowId$key = 'workflow.id'
  metaWorkflowId$value = workflowId
  
  metaStepId = Pair$new()
  metaStepId$key = 'step.id'
  metaStepId$value = stepId
  
  fileDoc$meta <- list(metaWorkflowId, metaStepId)
  con          <- rawConnection(raw(0), "r+")
  # store the query object to be able to check if input data has changed
  result_list  <- append(result_list, list(query = ctx$query))
  saveRDS(result_list, con)
  bytes        <- rawConnectionValue(con)
  removeCurrentFiles(session, workflowId, stepId)
  fileDoc <- ctx$client$fileService$upload(fileDoc, bytes)
}

getFilesByWorkflowAndStep <- function(ctx, workflowId, stepId) {
  ctx$client$fileService$findFileByWorkflowIdAndStepId(
    startKey   = list(workflowId, stepId),
    endKey     = list(workflowId, ''),
    descending = TRUE, limit = 10)
}

getResultsFile = function(session, name) {
  result     <- NULL
  ctx        <-  getCtx(session)
  workflowId <- getWorkflowId(session)
  stepId     <- getStepId(session)
  
  files <- getFilesByWorkflowAndStep(ctx, workflowId, stepId)
  
  if (length(files) > 0) {
    files <- files[unlist(lapply(files, FUN = function(x) x$name == name))]
    if (!identical(files, list())) {
      result <- files[[1]]
    }
  } 
  result
}

getCtxResults <- function(session) {
  ctx      <-  getCtx(session)
  result   <- NULL
  file     <- getResultsFile(session, "results")
  if (!is.null(file)) {
    bytes    <- ctx$client$fileService$download(file$id)
    raw_con  <- rawConnection(object = bytes, open = "r")
    result   <- readRDS(raw_con)
    # check if input data is equal
    if (!inputDataEqual(ctx, result)) {
      result <- NULL
    }
  }
  result
}

inputDataEqual <- function(ctx, results) {
  result <- FALSE
  if (!is.null(results) && class(results) == "list" && "query" %in% names(results) &&
      ctx$query$qtHash == results$query$qtHash && ctx$query$rowHash == results$query$rowHash && ctx$query$columnHash == results$query$columnHash) {
    result <- TRUE
  }
  result
}

isRunView <- function(view) {
  view == "run"
}
isResultView <- function(view) {
  view == "showResult"
}
