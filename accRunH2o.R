accRun <- function(inputData,
                   Model="XGB",
                   codeTest=TRUE,
                   Parallel=TRUE,
                   K = 10,
                   SAVE = TRUE,
                   Cores = 8,
                   printSummary = TRUE,
                   DS=T){
  
  library("caret")
  #   library("doRNG")
  # library("ROCR")
  library("dplyr")
  source("~/Documents/r_files/telstra/parallelCVH2o.R")
  

  trainData <- inputData %>% select(-id) %>% mutate(fault_severity = factor(fault_severity))
#   browser()
  if(DS){
  trainData <- downSample(x = trainData %>% select(-fault_severity),
                          y = trainData$fault_severity,
                          yname = "fault_severity")
  }
  switch(Model,
         "RF" = {
           
           if(codeTest){

             paramList <- NULL
             paramList$mtry <- 7
             paramList$ntree <- 100
             paramList$max_depth <- 100
             paramList$nbins <- 25
           }
           else{
             paramList <- NULL
             paramList$mtry <- 7
             paramList$ntree <- 500
             paramList$max_depth <- 100
             paramList$nbins <- 25
             
           }
           outputData <- parallelCV(inputData = trainData,
                                    k = K,
                                    trainTargets = "fault_severity",
                                    paramList = paramList,
                                    Model="RF",
                                    Cores = Cores)
           
           
           # browser()
         },
         "GBM" = {
           source("~/Documents/r_files/telstra/parallelCVH2o.R")
           
           if(codeTest){
             
             paramList <- NULL
             paramList$ntree = 500
             paramList$max_depth = 100
             paramList$nbins = 50
             paramList$learn_rate = .01
             paramList$min_rows = 1
             
           }
           else{
             
             paramList <- NULL
             paramList$ntree = 1000
             paramList$max_depth = 100
             paramList$nbins = 50
             paramList$learn_rate = .01

           }
           
           outputData <- parallelCV(inputData = trainData,
                                    k = K,
                                    trainTargets = "fault_severity",
                                    paramList = paramList,
                                    Model= "GBM",
                                    Cores = Cores)

         },
         "XGB"  ={
           
           source("~/Documents/r_files/telstra/parallelCVxgb.r")
           
           if(codeTest){
             
             paramList <- NULL
             paramList$eta = 0.2
             paramList$max.depth = 100
             paramList$nrounds = 70
             paramList$nthread = 8
             paramList$subsample = 0.5
             paramList$colsample_bytree = 0.5
             paramList$lambda = 0
             paramList$alpha = 0.7
             paramList$max_delta_step = 0 #don't think this param is doing anything leave at default
             
           }
           else{
             
             paramList <- NULL
             paramList$eta = 0.01
             paramList$max.depth = 10
             paramList$nrounds = 2000
             paramList$nthread = 8
             paramList$subsample = 0.5
             paramList$colsample_bytree = 0.5
             paramList$lambda = 0.5
             paramList$alpha = 0.1
             paramList$max_delta_step = 0 #don't think this param is doing anything leave at default
           }
           outputData <- parallelCVxgb(inputData = trainData,
                                    k = K,
                                    trainTargets = "fault_severity",
                                    paramList = paramList,
                                    Model= "XGB",
                                    Cores = Cores)
           
         }
         )
  
  if(SAVE){
    if(Model == "SVM"){
      fName <- paste0(Model, " Model with ",Kernel," kernel, ", K, "-fold CV, on ", date(),".RData")
    }else{
      fName <- paste0(Model, " Model, ", K, "-fold CV, on ", date(),".RData")
    }
    cat("##### saving", fName, "\n")
    
    mainDir <- getwd()
    subDir <- "outputDirectory"
    
    if (file.exists(subDir)){
      setwd(file.path(mainDir, subDir))
    } else {
      dir.create(file.path(mainDir, subDir))
      setwd(file.path(mainDir, subDir))
    }
    save(outputData, file=fName)
    setwd(mainDir)   
  }
  return(outputData)  
}