library(shiny)
library(tercen)
library(plyr)
library(dplyr)
library(hexbin)
library(shinyjs)
source("fvsn.R")
source("supporting.R")

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
  message <- reactiveValues(text = NULL, error = NULL)
  
  context <- reactive({
    getCtx(session)
  })
  
  inputData <- reactive({
    getData(session)
  })
  
  returnData <- reactive({
    results$hdf
  })
  
  mode <- reactive({ 
    getMode(session) 
  })
  
  output$body <- renderUI({
    mode <- mode()

    if (isShowView(mode)) {
      createInitialView(disableRun = TRUE)
    } else if (isRunView(mode)) {
      createInitialView()
    } else if (isResultView(mode)) {
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
          upload_html()
        )
      }
    }
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
  
  output$status = renderUI({
    result <- NULL
    if (!is.null(message$text)) {
      result <- message$text
    } else if (!is.null(message$error)) {
      result <- HTML(paste("<div style='color: red'>", message$error, "</div>"))
    }
    result
  })

  ## Observe (event)
    
  observe({
    data     <- inputData()
    col_df   <- data$col_df
    array_labels <- colnames(col_df)
    
    observeEvent(input$refset, {
      updateSelectInput(session,  inputId = "reffactor", choices = array_labels)
    })
    
    observeEvent(input$reffactor, {
      rf <- factor(col_df[[input$reffactor]])
      updateSelectInput(session, inputId = "reflevel", choices = levels(rf))
    })
    
    start_value <- isolate(input$start)
    if (!is.null(start_value) && start_value == 0) {
      message$text = "."
    }
  })
  
  observeEvent(input$button, {
    shinyjs::disable("button")
    
    ctx <- context()
    returnData() %>% 
      ctx$addNamespace() %>% 
      ctx$save()
  })
  
  observeEvent(mode(), {
    mode <- mode()
    if (isRunView(mode)) {
      nid <<- showNotification("Press Run to start the analysis.", duration = NULL, type = "message", closeButton = FALSE)
    } else if (isResultView(mode)) {
      if (exists("nid")) { 
        removeNotification(nid) 
      }
    }
  })
  
  # calculation
  observeEvent(input$start, {
    shinyjs::disable("start")
    showNotification(ui = "Running VSN ...", id = nid, type = "message", closeButton = FALSE, duration = NULL)
    
    data     <- inputData()
    df       <- data$df
    col_df   <- data$col_df
    color_df <- data$color_df
    bRef     <- input$refset
    
    if (!is.null(color_df) && ncol(color_df) >= 1) {
      grouping <- droplevels(interaction(color_df))
    } else {
      grouping <- "Main"
    }
    
    df <- data.frame(df, grp = grouping)
    hdf <- vsnResult <- NULL
    tryCatch({
      if (!bRef) {
        vsnResult <- df %>% group_by(grp) %>% do(vsn0(., normalization = input$affine))
        hdf       <- vsnResult %>% group_by(grp) %>% do(vsnh(.))
        reslist   <- list(vsnResult = vsnResult)
      } else {
        col_df       <- col_df %>% mutate(.ci = seq(0, nrow(.) - 1))
        df           <- df %>% left_join(col_df %>% select(input$reffactor, .ci))
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
      
      reslist$settings <- settings
      reslist$df <- df
      hdf        <- hdf[,-1]
      hdf        <- hdf[!is.na(hdf$Hvsn),]
      hdf$.ri    <- as.double(hdf$.ri)
      hdf$.ci    <- as.double(hdf$.ci)
      
      # save objects in tercen context
      saveData(session, list(df = df, vsnResult = vsnResult, hdf = hdf, reslist = reslist))
      results$df        <- df
      results$vsnResult <- vsnResult
      results$hdf       <- hdf
      results$reslist   <- reslist
      
      showNotification(ui = "Done", id = nid, type = "message", closeButton = FALSE)
      message$text  <- "Done"
      message$error <- NULL
      shinyjs::enable("button")
      shinyjs::enable("start")
      
    }, error = function(e) {
      showNotification(ui = "Done with errors", id = nid, type = "message", closeButton = FALSE)
      message$text  <- NULL
      message$error <- e$message
      shinyjs::disable("button")
      shinyjs::enable("start")
    })
  })
})

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

getMode = function(session){
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
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

isShowView <- function(mode) {
  !is.null(mode) && mode == "show"
}
isRunView <- function(mode) {
  !is.null(mode) && mode == "run"
}
isResultView <- function(mode) {
  !is.null(mode) && mode == "showResult"
}
