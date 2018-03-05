#' Default hyperparameter experimental test design for a single-hidden layer undercomplete 
#' autoencoder neural netowrk.
#'
#'@section Information: A .csv dataset containing the factors and levels of 600-test trial designed experiment
#' generated in JMP Pro v12.1. The designed experiment is a flexible space filling design 
#' for 9 test design factors.
#' 
#' @format A .csv with 601 rows and 12 features; 9-test design features, 1-response 
#' variable feature, and 1-design point index feature
#' 
#' \itemize{
#'   \item{Subset_Split}{Categorical 3-level factor; describes the subset to use 
#'   in the Autoencoder}
#'   \item{Design_Point}{An index of each test point}
#'   \item{Activatino_Function}{Categroical 2-level factor; describes the activation 
#'   function within each neuron in the neural network}
#'   \item{Input_DO_Rate}{Continuous multi-level factor; describes the Dropout 
#'   rate of the input layer neurons}
#'   \item{Hidden_DO_Rate}{Continuous multi-level factor; describes the Dropout 
#'   rate of the hidden layer neurons}
#'   \item{Initial_Weight_Distribution}{Categorical 3-level factor; describes the 
#'   distribution by which the initial weights of the autoencoder neural network 
#'   are generated}
#'   \item{Data_Scale}{Categorical 3-level factor; describes the range to which 
#'   the training and test subset data is scaled.}
#'   \item{Rho}{Continuous multi-level factor; describes the value of the rho parameter 
#'   for the ADADELTA learning procedure}
#'   \item{Epsilon}{Continuous multi-level factor; describes the value of the epsilon 
#'   parameter for the ADADELTA learning procedure}
#'   \item{Shuffle_Train_Data}{Categorical 2-level factor; boolean value indicating 
#'   if the training data should be randomly shuffled during neural network training}
#'   \item{Y}{Placeholder for the response value measurement}
#' }
"testDesignShiny"