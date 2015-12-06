parallelCVxgb <- function(inputData,
                          k = 5,
                          paramList = NULL,
                          Model = "xgb",
                          trainTargets = "EventIds",
                          Cores = 4,
                          testDataSplit = NULL,
                          plotCvScore = TRUE){
  
  library("dplyr")
  
  # inputData <- 
  
  dataNames <- names(inputData)
  idx <- dataNames %in% trainTargets
  # browser()
  dtrain <- xgb.DMatrix(data = as.matrix(inputData[, !idx]), label = as.matrix(inputData[, idx]))
  set.seed(123)
  
  xgbModel <- xgb.cv(params = list(objective = "multi:softmax",
                                   eta = paramList$eta,
                                   max.depth = paramList$max.depth,
                                   nthread = paramList$nthread,
                                   subsample = paramList$subsample,
                                   colsample_bytree = paramList$colsample_bytree,
                                   lambda = paramList$lambda,
                                   alpha = paramList$alpha,
                                   max_delta_step = paramList$max_delta_step,
                                   num_class = 3),
                     # booster = "gblinear",
                     data = dtrain,
                     nrounds = paramList$nrounds,
                     nfold = k,
                     metrics = list("mlogloss"),
                     prediction = FALSE,
                     showsd = TRUE,
                     stratified = TRUE,
                     verbose = F,
                     # print.every.n = 10L,
                     # early.stop.round = 20,
                     maximize = FALSE)
  # browser()
  if(plotCvScore){
  plotData = xgbModel
  plotData$boostingRound <- 1:paramList$nrounds
  pp <- ggplot() + 
    geom_line(data = plotData, aes(x = boostingRound, y = test.mlogloss.mean), colour = "red") +
    geom_line(data = plotData, aes(x = boostingRound, y = train.mlogloss.mean))
  cat("min test logloss = ", min(xgbModel$test.mlogloss.mean),"\n")
  cat("at boosting itteration", which.min(xgbModel$test.mlogloss.mean),"\n")
  print(pp)
  }
  return(xgbModel)
}