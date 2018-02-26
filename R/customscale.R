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
