trainData <- tData %>% select(-id)# %>% mutate(fault_severity = factor(fault_severity))
#   browser()
trainData <- downSample(x = trainData %>% select(-fault_severity),
                        y = trainData$fault_severity,
                        yname = "fault_severity")


glmModel <- cv.glmnet(x = as.matrix(trainData %>% dplyr::select(-fault_severity)), y = trainData$fault_severity, family = "multinomial", nfolds = 5)
preds = predict(glmModel, newx = as.matrix(tData %>% dplyr::select(-id, -fault_severity)), type = "response")
logLoss(preds[,,1], tData$fault_severity)
plot(glmModel)

