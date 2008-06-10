
trainNWS <- function(x, y,
   method = "rf",
   ...,
   metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
   sleighObj = NULL,
   trControl = trainNWSControl(),
   tuneGrid = NULL,
   tuneLength = 3)
{
   library(nws)
   
   if(is.null(sleighObj))
   {
       sleighObj <- trControl$start()
       on.exit(try(stopSleigh(sleighObj), silent = TRUE))
   }
   
   funcCall <- match.call(expand.dots = TRUE)
   
   modelType <- if(is.factor(y)) "Classification"  else "Regression"
   modelInfo <- caret:::modelLookup(method)
           
   if(modelType == "Classification")
   {
      if(!any(modelInfo$forClass)) stop("wrong model type for classification")
      # we should get and save the class labels to ensure that predictions are coerced
      # to factors that have the same levels as the original data. This is especially
      # important with multiclass systems where one or more classes have low sample sizes
      # relative to the others
      classLevels <- levels(y)
      if(length(classLevels) > 2 & (method %in% c("gbm", "glmboost", "ada", "gamboost", "blackboost")))
         stop("This model is only implimented for two class problems")
      if(!(metric %in% c("Accuracy", "Kappa")))
         stop(paste("Metric", metric, "not applicable for classification models"))
   } else {
      if(!any(modelInfo$forReg)) stop("wrong model type for regression")
      if(!(metric %in% c("RMSE", "Rsquared")))
         stop(paste("Metric", metric, "not applicable for regression models"))
      classLevels <- NA
   }
   
   if(trControl$method == "oob" & !(method %in% c("rf", "treebag", "cforest", "bagEarth", "bagFDA")))
      stop("for oob error rates, model bust be one of: rf, cforest, bagEarth, bagFDA or treebag")
   
   if(is.null(trControl$index)) trControl$index <- switch(
      tolower(trControl$method),
      oob = NULL,
      cv = createFolds(y, trControl$number, returnTrain = TRUE),
      loocv = createFolds(y, length(y), returnTrain = TRUE),
      boot = createResample(y, trControl$number),
      test = createDataPartition(y, 1, trControl$p),
      lgocv = createDataPartition(y, trControl$number, trControl$p))
   
   # combine the features and classes into one df, as needed by ipred.
   trainData <- as.data.frame(x)

   #check mode for some models
   if(!(method %in% c("rf", "rpart", "gbm", "treebag", "nb")))
   {
      isFactor <- lapply(trainData, is.factor)
      isCharacter <- lapply(trainData, is.character)
      if(any(isFactor)   | any(isCharacter))
         stop("All predictors must be numeric for this model")
   }

   # add the outcome to the data passed into the functions
   trainData$.outcome <- y

   # if no default training grid is specified, get one. We have to pass in the formula
   # and data for some models (rpart, pam, etc - see manual fo rmore details)
   if(is.null(tuneGrid)) tuneGrid <- createGrid(method, tuneLength, trainData)

#------------------------------------------------------------------------------------------------------------------------------------------------------#
   # For each tuning parameter combination, we will loop over them, fit models and generate predictions.
   # We only save the predictions at this point, not the models (and in the case of method = "oob" we
   # only save the prediction summaries at this stage.
   
   
   # trainInfo will hold teh infomration about how we should loop to train the model and what types
   # of parameters are used. If method = "oob", we need to setip a container for the resamplng
   # summary statistics
   
   trainInfo <- caret:::tuneScheme(method, tuneGrid, trControl$method == "oob")
   paramCols <- paste(".", trainInfo$model$parameter, sep = "")
      
   if(trainInfo$scheme == "oob")
   {
      if(modelType == "Regression")
      {
         performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(trainInfo$loop)[1]))
         names(performance) <- c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
   
      } else {
         performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(trainInfo$loop)[1]))
         names(performance) <- c("Accuracy", "Kappa", "AccuracySD", "KappaSD")
      }
   }

   makeIndex <- function(n, k)
   {
      out <- rep(1:k, n%/%k)
      if(n %% k > 0)  out <- c(out, sample(1:k, n %% k))
      out
   }

   workerGroups <- makeIndex(length(trControl$index), workerCount(sleighObj))
   uniqueGroups <- sort(unique(workerGroups))
   if(trControl$verboseIter)
   {
      cat("Distribution of resamples per job:\n")
      print(table(paste("job", workerGroups)))
      cat("\n")
   }
   splitList <- vector(mode = "list", length = workerCount(sleighObj))
   for(i in seq(along = splitList)) splitList[[i]] <- trControl$index[which(workerGroups == i)]

   results <- NULL
   for(j in 1:nrow(trainInfo$loop))
   {

      if(trControl$verboseIter)
      {
         caret:::iterPrint(trainInfo, j)
         flush.console()
      }
            
      argList <- list(
         data = trainData,
         method = method,
         tuneValue = trainInfo$loop[j,, drop = FALSE],
         obsLevels = classLevels)
      argList <- append(argList, list(...))
      
      # add fault tolerance switch
      switch(
         trainInfo$scheme,
         basic =
         {
            # start NWS changes
               thisIter <- do.call(
                  rbind,
                  eachElem(
                     sleighObj,
                     caret:::byResampleBasic,
                     list(splitList),
                     list(
                        x = argList,
                        combo = trainInfo$loop[j, trainInfo$constant, drop = FALSE]
                        )))
            #end NWS changes
            results <- rbind(results, thisIter)
         },
         seq =
         {
            # start NWS changes
               thisIter <- do.call(
                  rbind,
                  eachElem(
                     sleighObj,
                     caret:::byResampleSeq,
                     list(splitList),
                     list(
                        x = argList,
                        seq = trainInfo$seqParam[[j]],
                        combo = trainInfo$loop[j, trainInfo$constant, drop = FALSE]
                        )))
            #end NWS changes
            results <- rbind(results, thisIter)
         },
         oob =
         {
            tmpModelFit <- do.call(createModel, argList)
            tmpPerf <- switch(
               class(tmpModelFit)[1],
               randomForest = caret:::rfStats(tmpModelFit),
               RandomForest = caret:::cforestStats(tmpModelFit),
               bagEarth =, bagFDA = caret:::bagEarthStats(tmpModelFit),
               regbagg =, classbagg = caret:::ipredStats(tmpModelFit))
            performance[j, names(performance) %in% names(tmpPerf)] <- tmpPerf
         
         
         })
   }

   paramNames <- substring(names(tuneGrid), 2)
   if(trControl$method != "oob")
   {
      perResample <- caret:::poolByResample(results, tuneGrid, foo)
      performance <- caret:::summarize(perResample, tuneGrid, foo)
      pNames <- names(performance)
      pNames[pNames %in% names(tuneGrid)] <- paramNames
      names(performance) <- pNames
 
   } else {
      tmpLoop <- trainInfo$loop
      names(tmpLoop) <- substring(names(tmpLoop), 2)
      performance <- cbind(tmpLoop, performance)
   }
          
   perfCols <- names(performance)
   perfCols <- perfCols[!(perfCols %in% paramNames)]
               
# sort the tuning parameters form least complex to most complex
  performance <- caret:::byComplexity(performance, method)

  # select the optimal set

  selectClass <- class(trControl$selectionFunction)[1]

  if(selectClass == "function")
    {
      bestIter <- trControl$selectionFunction(performance, metric)
    }
  else {
    if(trControl$selectionFunction == "oneSE")
      {
        bestIter <- oneSE(performance, metric, length(trControl$index))
      } else {

        bestIter <- do.call(
                            trControl$selectionFunction,
                            list(
                                 x = performance,
                                 metric = metric))
      }
  }
         
   bestTune <- performance[bestIter, trainInfo$model$parameter, drop = FALSE]
    names(bestTune) <- paste(".", names(bestTune), sep = "")
    
   if(trControl$method != "oob")
   {
      byResample <- merge(bestTune, perResample)
      byResample <- byResample[,!(names(perResample) %in% names(tuneGrid))]
   } else {
      byResample <- NULL
   }

   # reorder rows of performance
   orderList <- list()
   for(i in seq(along = trainInfo$model$parameter))
   {
      orderList[[i]] <- performance[,trainInfo$model$parameter[i]]
   }
   names(orderList) <- trainInfo$model$parameter
   performance <- performance[do.call("order", orderList),]
  
       
#------------------------------------------------------------------------------------------------------------------------------------------------------#

   finalModel <- createModel(
      trainData,
      method = method,
      bestTune,
      obsLevels = classLevels,
      ...)
    
   # remove this and check for other places it is reference
   # replaced by tuneValue
   if(method == "pls") finalModel$bestIter <- bestTune
  
   outData <- if(trControl$returnData) trainData else NULL
   
   # in the case of pam, the data will need to be saved differently
   if(trControl$returnData & method == "pam")
   {
      finalModel$xData <- x
      finalModel$yData <- y
   }
   
  structure(list(
                 method = method,
                 modelType = modelType,
                 results = performance,
                 bestTune = bestTune,
                 call = funcCall, 
                 dots = list(...),
                 metric = metric,
                 control = trControl,
                 finalModel = finalModel,
                 trainingData = outData,
                 resample = byResample
                 ), 
            class = "train")
}


