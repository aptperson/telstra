parallelCV <- function(inputData,
                       k = 5,
                       paramList = NULL,
                       Model = "RF",
                       trainTargets = "EventIds",
                       Cores = 4,
                       testDataSplit = NULL){
  
  # library("caret")
  library("dplyr")
  
  library("h2o")
  ##### start h2o
  localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads = -1, max_mem_size = "8gb")
  
  print(str(inputData))
  
  ##### create the cross validation folds
  if(Model != "DL"){
    inputData$fold_column <- createFolds(inputData[, trainTargets], k = k, list = F)
  }
  ##### results storage
  #   fittnessScore <- matrix(nrow=nParma1, ncol=nParma2)
  
  ##### export the data to the h2o cluster
  cat("exporting the training data to the h2o cluster\n")
  inputData.hex <- as.h2o(conn = localH2O, object = inputData)
  cat("exporting the testing data to the h2o cluster\n")
  # testDataSplit.hex <- as.h2o(conn = localH2O, object = testDataSplit)
  
  switch(Model,
         "RF"={
           ##### train the model accross the parameter grid
           cat("training progress \n")
           # browser()
           paramGrid <- h2o.grid(algorithm = "randomForest",
                                 x = names(inputData)[names(inputData) != trainTargets],
                                 y = trainTargets,
                                 conn = localH2O,
                                 training_frame = inputData.hex,
                                 hyper_params = list(mtries = paramList$mtry,
                                                     ntrees = paramList$ntree,
                                                     max_depth = paramList$max_depth,
                                                     nbins = paramList$nbins),
                                 # fold_column = "fold_column",
                                 keep_cross_validation_predictions = TRUE,
                                 balance_classes = F,
                                 nfolds = k)
           
           ##### extract models
           rfModels <- lapply(paramGrid@model_ids, function(id) { h2o.getModel(id)})
           # browser()
           ##### it is possible that the parameter list has been reordered
           ##### extract parameters and create a new list
           paramList2 <- do.call(rbind, lapply(1:length(rfModels), function(i){
             data.frame(ntree = rfModels[[i]]@allparameters$ntrees,
                        mtry = rfModels[[i]]@allparameters$mtries,
                        max_depth = rfModels[[i]]@allparameters$max_depth,
                        nbins = rfModels[[i]]@allparameters$nbins,
                        logLoss = h2o.logloss(rfModels[[i]])
             )}))
           
         }, # end RF
         "GBM" = {
           ##### train the model accross the parameter grid
           cat("training progress \n")
           browser()
           
           paramGrid <- h2o.grid(algorithm = "gbm",
                                 x = names(inputData)[names(inputData) != trainTargets],
                                 y = trainTargets,
                                 training_frame = inputData.hex,
                                 distribution = "multinomial",
                                 hyper_params = list(ntrees = paramList$ntree,
                                                     max_depth = paramList$max_depth,
                                                     nbins = paramList$nbins,
                                                     learn_rate = paramList$learn_rate),
                                 # fold_column = "fold_column",
                                 keep_cross_validation_predictions = TRUE,
                                 balance_classes = TRUE,
                                 nfolds = k)
           
           ##### extract models
           rfModels <- lapply(paramGrid@model_ids, function(id) { h2o.getModel(id)})
           
           ##### it is possible that the parameter list has been reordered
           ##### extract parameters and create a new list
           paramList2 <- do.call(rbind, lapply(1:length(rfModels), function(i){
             data.frame(ntree = rfModels[[i]]@allparameters$ntrees,
                        max_depth = rfModels[[i]]@allparameters$max_depth,
                        learn_rate = rfModels[[i]]@allparameters$learn_rate,
                        nbins = rfModels[[i]]@allparameters$nbins,
                        logLoss = h2o.logloss(rfModels[[i]])
             )}))
           
         }
         
  ) #end switch
  
  ##### ertract the confustion matricies
  rfConfusionMat <- lapply(rfModels, h2o.confusionMatrix, valid = TRUE)
  
  ##### defins the prediction fold for each of the models
  foldIdxM1 <- c(k, 1:(k-1))
  
  # browser()
  ##### preformance metrics
  fittnessCV <- lapply(1:length(rfModels), function(j){sapply(1:k, function(i, j){
    # browser()
    sum(
      as.character(
        as.data.frame(
          h2o.getFrame(
            rfModels[[j]]@model$cross_validation_predictions[[i]]$name))$predict)[inputData$fold_column==foldIdxM1[i]] ==
        as.character(inputData[inputData$fold_column==foldIdxM1[i], trainTargets])) /
      sum(inputData$fold_column==foldIdxM1[i])
  }, j)
  })
  
  ##### combine the preformance metrics and the parameters
  meanfittnessCV <- data.frame(paramList2,
                               fittness = matrix(sapply(fittnessCV, mean), ncol=1),
                               sd = matrix(sapply(fittnessCV, sd), ncol=1))
  cat("CV fittness: \n")
  print(meanfittnessCV)
  
  cat("the best parameters are: \n")
  bestParams = paramList2[which.max(meanfittnessCV$fittness),]
  print(bestParams)
  print(rfConfusionMat[which.max(meanfittnessCV$fittness)])
  
  ##### shutdown the h20 cluster
  # tmp = h2o.shutdown(conn = localH2O, prompt = FALSE)
  
  return(list(meanfittnessCV = meanfittnessCV,
              bestParams = bestParams,
              confusionMat = rfConfusionMat,
              models = rfModels))
}