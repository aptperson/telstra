telstraDataLoad <- function(dataLocation = "~/Documents/r_files/telstra/"){
  
  library("dplyr")
  library("reshape2")
  library("randomForest")
  #####
  trainData <- read.csv(file = file.path(dataLocation, "train.csv"), stringsAsFactors = FALSE)
  
  ##### location = categorical nominal
  trainData$location <- as.numeric(sapply(trainData$location, FUN=function(x) {strsplit(x, split='[ ]')[[1]][2]}))
  trainDataMat <- dcast(data = trainData, formula = id ~ location, fill = 0)
  ids <- trainDataMat$id
  trainDataMat[trainDataMat!=0] <- 1
  trainDataMat$id <- ids
  
  ##### severity_type = categorical nominal
  # only 1 severity type per id
  severity_type <- read.csv(file = file.path(dataLocation, "severity_type.csv"), stringsAsFactors = FALSE)
  severity_type$severity_type <- as.numeric(sapply(severity_type$severity_type, FUN=function(x) {strsplit(x, split='[ ]')[[1]][2]}))
  severity_typeMat <- dcast(data = severity_type, formula = id ~ severity_type, fill = 0)
  ids <- severity_typeMat$id
  severity_typeMat[severity_typeMat!=0] <- 1
  severity_typeMat$id <- ids
  # browser()
  names(severity_typeMat)[2:ncol(severity_typeMat)] <-
    paste0("severity_type", names(severity_typeMat[2:ncol(severity_typeMat)]))
  
  
  
  resource_type <- read.csv(file = file.path(dataLocation, "resource_type.csv"), stringsAsFactors = FALSE)
  resource_type$resource_type <- as.numeric(sapply(resource_type$resource_type, FUN=function(x) {strsplit(x, split='[ ]')[[1]][2]}))
  # resource_typeMat <- resource_type
  resource_typeMat <- dcast(data = resource_type, formula = id ~ resource_type, fill = 0)
  ids <- resource_typeMat$id
  resource_typeMat[resource_typeMat!=0] <- 1
  names(resource_typeMat)[2:ncol(resource_typeMat)] <-
    paste0("resource_type", names(resource_typeMat[2:ncol(resource_typeMat)]))
  resource_typeMat$id <- ids
  
  log_feature <- read.csv(file = file.path(dataLocation, "log_feature.csv"), stringsAsFactors = FALSE)
  log_feature$log_feature <- as.numeric(sapply(log_feature$log_feature, FUN=function(x) {strsplit(x, split='[ ]')[[1]][2]}))
  log_featureMat <- log_feature
#   log_featureMat <- dcast(data = log_feature, formula = id ~ log_feature, fill = 0)
#   ids <- log_featureMat$id
#   log_featureMat[log_featureMat!=0] <- 1
#   log_featureMat$id <- ids
  
  event_type <- read.csv(file = file.path(dataLocation, "event_type.csv"), stringsAsFactors = FALSE)
  event_type$event_type <- as.numeric(sapply(event_type$event_type, FUN=function(x) {strsplit(x, split='[ ]')[[1]][2]}))
  event_typeMat <- dcast(data = event_type, formula = id ~ event_type, fill = 0)
  ids <- event_typeMat$id
  names(event_typeMat)[2:ncol(event_typeMat)] <-
    paste0("event_type", names(event_typeMat[2:ncol(event_typeMat)]))
  event_typeMat[event_typeMat!=0] <- 1
  event_typeMat$id <- ids
  
  
  ##### join all the data
  trainData <- left_join(x = trainData, y = severity_typeMat, by = "id")
  trainData <- left_join(x = trainData, y = resource_typeMat, by = "id")
  trainData <- left_join(x = trainData, y = log_featureMat, by = "id")
  trainData <- left_join(x = trainData, y = event_typeMat, by = "id")
  names(trainData)=gsub(pattern = "[ ]", replacement = "", x = names(trainData))
  # browser()
  # head(trainData)
  # tmp = trainData %>% select(-id) %>% mutate(fault_severity = factor(fault_severity))
  
  # rfModel <- randomForest(formula = fault_severity ~., data = tmp)
  
  # return(list(rfModel = rfModel, trainData=trainData))
  return(trainData)
}