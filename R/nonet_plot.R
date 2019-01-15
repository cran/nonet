#' Plot the predictions or results of nonet_ensemble
#'
#' @param x x axis variable name or histogram entity name
#' @param y y axis variable name
#' @param nonet_size size of plot need to feed in ggplot
#' @param nonet_alpha value of alpha for ggplot
#' @param nonet_bins number of bins for histogram
#' @param plot_type type of plot, if not provided it takes "NULL"
#' @param dataframe dataframe which is used for plotting purpose.
#'
#' @return plotted for the plot results provided as input.
#' @export
#' @import caret
#' @import ggplot2
#' @import tidyverse
#'
#' @examples
#' # nonet_plot functionality can be explained via below example
#' # Setup
#' library(caret)
#' library(nonet)
#' library(ggplot2)
#'
#' # Load Data
#' dataframe <- data.frame(banknote_authentication[600:900, ])
#' dataframe$class <- as.factor(ifelse(dataframe$class >= 1, 'Yes', 'No'))
#' 
#' # Spliting into train and test
#' index <- createDataPartition(dataframe$class, p=0.75, list=FALSE)
#' trainSet <- dataframe[ index,]
#' testSet <- dataframe[-index,]
#' 
#' # Feature selection 
#'  control <- rfeControl(functions = rfFuncs,
#'   method = "repeatedcv",
#'   repeats = 2,
#'   verbose = FALSE)
#' 
#' outcomeName <- 'class'
#' predictors <- c("curtosis", "entropy")
#' 
#' # Model Training & predictions
#' banknote_rf <- train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
#' predictions_rf_raw <- predict.train(object=banknote_rf,testSet[,predictors],type="raw")
#' 
#' # Results
#' nonet_eval_rf <- confusionMatrix(predictions_rf_raw,testSet[,outcomeName])
#' eval_rf_df <- data.frame(nonet_eval_rf$table)
#' nonet_plot(eval_rf_df$Prediction, eval_rf_df$Reference, eval_rf_df, plot_type = "point")

nonet_plot <- function (x, y, dataframe, plot_type = NULL , nonet_size = 20, nonet_alpha = .3, nonet_bins = 25) {
  
  if (is.null(dataframe)) {
    stop("Please provide the not null values in the dataframe")
  }
  else{
    if (anyNA(dataframe)) {
      stop("Please provide the not na values in the dataframe")
    }
    else{
      if (plot_type == "hist") {
        plotted <- ggplot(dataframe, aes(x)) + geom_histogram(bins = nonet_bins)
        
	   }
      else{
        if (plot_type == "NULL") {
           plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
            geom_point(size = nonet_size, alpha = nonet_alpha) +
            geom_point()
        }
        else{
          if (plot_type == "point") {
            plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
              geom_point(size = nonet_size, alpha = nonet_alpha) +
              geom_point()
          }
          else{
            if (plot_type == "boxplot") {
              plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
                geom_point(size = nonet_size, alpha = nonet_alpha) +
                geom_boxplot()
            }
            else{
              if (plot_type == "density") {
                plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
                  geom_point(size = nonet_size, alpha = nonet_alpha) +
                 geom_density()
              }
            }
          }
        }
      }
      return(plotted)
    }
  }
}




