#' Ensemble Prediction without using training labels
#'
#' @param best_modelname Best model name is one which performance better while evaluating using any evaluation matrix like confusion matrix.
#' @param object prediction_list object, as from `tune_models`
#'
#' @return A list of ensembled predictions. You can evaluate the performance of ensembled prediction using the evaulation matrix as Confusion matrix or AUROC.
#' @export
#' @import caret
#' @import rlist
#' @import tidyverse
#' @import stats
#' @import glmnet
#' 
#' @examples
#' # nonet_ensemble functionality can be explained via below example
#' # Setup
#' library(caret)
#' library(nonet)
#' library(rlist)
#' 
#' # Load Data
#' dataframe <- data.frame(banknote_authentication[600:900, ])
#' dataframe$class <- as.factor(ifelse(dataframe$class >= 1, 'Yes', 'No'))
#' 
#' # First Model
#' # Spliting into train and test
#' index <- createDataPartition(dataframe$class, p=0.75, list=FALSE)
#' trainSet <- dataframe[ index,]
#' testSet <- dataframe[-index,]
#' 
#' #Feature selection 
#' control <- rfeControl(functions = rfFuncs,
#'   method = "repeatedcv",
#'   repeats = 1,
#'   verbose = FALSE)
#' 
#' outcomeName <- 'class'
#' predictors <- c("variance", "skewness")
#' 
#' banknote_rf <- train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
#' preds_rf_first <- predict.train(object=banknote_rf,testSet[,predictors],type="prob")
#' preds_rf_first_raw <- predict.train(object=banknote_rf,testSet[,predictors],type="raw")
#' 
#' # Second Model
#' # Spliting into train and test
#' index <- createDataPartition(dataframe$class, p=0.75, list=FALSE)
#' trainSet <- dataframe[ index,]
#' testSet <- dataframe[-index,]
#' 
#' #Feature selection 
#' control <- rfeControl(functions = rfFuncs,
#'   method = "repeatedcv",
#'   repeats = 2,
#'   verbose = FALSE)
#' 
#' outcomeName <- 'class'
#' predictors <- c("curtosis", "entropy")
#' 
#' banknote_rf <- train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
#' preds_rf_second <- predict.train(object=banknote_rf,testSet[,predictors],type="prob")
#' preds_rf_second_raw <- predict.train(object=banknote_rf,testSet[,predictors],type="raw")
#' 
#' Stack_object <- list(preds_rf_first$Yes, preds_rf_second$Yes)
#' names(Stack_object) <- c("model_rf_first", "model_rf_second")
#' 
#' # Prediction using nonet_ensemble function
#' prediction_nonet <- nonet_ensemble(Stack_object, "model_rf_second")
#'

nonet_ensemble <- function(object, best_modelname) {
  mod <- best_modelname
  prediction_list <- object
  if (is.null(prediction_list)) {
    stop("Please provide the object. It should be a list of predictions from different models.")
  }
  else {
   if (is.null(best_modelname)) {
    stop("Please provide the best model name")
  }
  else{
   target <- function(x) {
    if (x == mod) {
      x <- "response_variable"
    }
    else{(x != mod)
      x <- x
    }
  }
  names(prediction_list) <- lapply(names(prediction_list), target)
  preds_data_frame <- data.frame(prediction_list)
  weight_modeling <- glm(response_variable ~ ., data = preds_data_frame)
  weight_model_coefficient <- summary(weight_modeling)$coefficient
  weight_model <- data.frame(weight_model_coefficient)
  weight_model$variables <- row.names(weight_model)
  weight_model <- weight_model[c("variables", "Estimate")][-1, ]
  weight_model$Estimate <- abs(weight_model$Estimate)
  weight_model$weight <- weight_model$Estimate / sum(weight_model$Estimate)
  Preds_new <- list.remove(prediction_list, 'response_variable')
  Weighted_model <- Map("*", Preds_new, weight_model$Estimate)
  Weighted_model_updated <- list.append(Weighted_model, prediction_list$response_variable)
  add <- function(x) Reduce("+", x)
  preds_outcome <- add(Weighted_model_updated)
  return(preds_outcome)
  }
 }
}
