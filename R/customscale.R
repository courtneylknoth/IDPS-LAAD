#' Scale training and test datasets for anomaly detection 
#' 
#' @section Warning:
#' Requires that the test and training datasets have the same numeric features 
#' in the same order
#' 
#' @param DF_train The unscaled training dataset used to scale the values in 
#' the test dataset.
#' @param DF_test The unscaled test dataset scaled to the user defined interval 
#' [a, b].
#' @param a The minimum desired scaling value. This will be the minimum value of
#' the scaled training dataset. The test dataset minimum may be smaller than 
#' this value.
#' @param b The maximum desired scaling value. This will be the maximum value of 
#' the scaled training dataset. The test dataset maximum may be larger than this 
#' value.
#' 
#' @return ScaleDF The DF_test dataset scaled to [a,b] as determined from the 
#' DF_train dataset
#' 
#' @examples
#' \dontrun{
#' # Select only numeric features
#' irisExample <- iris[ ,1:4]
#' 
#' # Add column to split 80% training and 20% test
#' splitIris <- irisExample %>%
#'   dplyr::mutate(Cross = sample(c(1, 2),
#'                               size = nrow(irisExample),
#'                               replace = TRUE,
#'                               prob = c(0.80, 0.20)))
#' 
#' # Obtain training dataset
#' irisTrain <- splitIris[splitIris$Cross == 1, ] %>%                                                             
#'   dplyr::select(-Cross)
#' 
#' 
#' # Obtain test dataset
#' irisTest <- splitIris[splitIris$Cross == 2, ] %>%
#'   dplyr::select(-Cross)
#' 
#' # Scale irisTrain into [0, 1] (irisTrainZO)
#' irisTrainZO <- customscale(irisTrain, irisTrain, 0, 1)
#' 
#' # Scale irisTest into [0, 1] using 
#' irisTest <- customscale(irisTrain, irisTest, 0 ,1)
#' }
customscale <- function(DF_train, DF_test, a, b){

  min_train <- apply(DF_train, 2, min)
  max_train <- apply(DF_train, 2, max)
  ScaledDF <- DF_test

  for (i in 1:ncol(DF_test)){
    for(j in 1:nrow(DF_test)){

      ScaledDF[j, i] <- ((b-a) * (ScaledDF[j, i] - min_train[[i]])) /
        (max_train[[i]] - min_train[[i]]) + a
    }
  }
  return(ScaledDF)
}
