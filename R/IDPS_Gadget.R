#' @title Interactive anomaly detection using autoencoder neural network
#' 
#' @import shiny
#' @import shinyjs
#' @import tidyverse
#' @import caret
#' @import h2o
#' @importFrom shinythemes shinytheme
#' 
#' @export

IDPS_Gadget <- function(...){
  
  ui <- navbarPage(title = 'Anomaly Detection Autoencoder',
                   tabPanel('Intro and Instructions', 
                            uiOutput('Intro', 
                                     inline = FALSE)
                   ),
                   tabPanel('Data Preperation',
                            navbarPage(title = 'Data Preperation',
                                       tabPanel('Load Data',
                                                fluidPage(
                                                  flowLayout(
                                                    textOutput('OneData'),
                                                    tableOutput('Table')
                                                  )
                                                )
                                       ),
                                       tabPanel('Train/Test Split Data',
                                                fluidPage(
                                                  navlistPanel('Select Dataset to View',
                                                               id = 'SplitSel',
                                                               tabPanel('Training Subset 1',
                                                                        tableOutput('Train1')
                                                               ),
                                                               tabPanel('Test Subset 1',
                                                                        tableOutput('Test1')
                                                               ),
                                                               tabPanel('Train Subset 2',
                                                                        tableOutput('Train2')
                                                               ),
                                                               tabPanel('Test Subset 2',
                                                                        tableOutput('Test2')
                                                               ),
                                                               tabPanel('Train Susbet 3',
                                                                        tableOutput('Train3')
                                                               ),
                                                               tabPanel('Test Subset 3',
                                                                        tableOutput('Test3')
                                                               )
                                                  )
                                                )
                                       ),
                                       tabPanel('Scale Data', 
                                                fluidPage(
                                                  navlistPanel('Select Dataset to View',
                                                               id = 'DatasetSel',
                                                               tabPanel('Subset 1 Train [0,1]', 
                                                                        tableOutput('Scale1TrainZO')
                                                               ),
                                                               tabPanel('Subset 1 Test [0,1]', 
                                                                        tableOutput('Scale1TestZO')
                                                               ),
                                                               tabPanel('Subset 2 Train [0,1]',
                                                                        tableOutput('Scale2TrainZO')
                                                               ),
                                                               tabPanel('Subset 2 Test [0,1]',
                                                                        tableOutput('Scale2TestZO')
                                                               ),
                                                               tabPanel('Subset 3 Train [0,1]',
                                                                        tableOutput('Scale3TrainZO')
                                                               ),
                                                               tabPanel('Subset 3 Test [0,1]',
                                                                        tableOutput('Scale3TestZO')
                                                               ),
                                                               tabPanel('Subset 1 Train [-1,1]', 
                                                                        tableOutput('Scale1TrainPM1')
                                                               ),
                                                               tabPanel('Subset 1 Test [-1,1]', 
                                                                        tableOutput('Scale1TestPM1')
                                                               ),
                                                               tabPanel('Subset 2 Train [-1,1]',
                                                                        tableOutput('Scale2TrainPM1')
                                                               ),
                                                               tabPanel('Subset 2 Test [-1,1]',
                                                                        tableOutput('Scale2TestPM1')
                                                               ),
                                                               tabPanel('Subset 3 Train [-1,1]',
                                                                        tableOutput('Scale3TrainPM1')
                                                               ),
                                                               tabPanel('Subset 3 Test [-1,1]',
                                                                        tableOutput('Scale3TestPM1')
                                                               ),
                                                               tabPanel('Subset 1 Train [-0.5,0.5]', 
                                                                        tableOutput('Scale1TrainPM.5')
                                                               ),
                                                               tabPanel('Subset 1 Test [-0.5,0.5]', 
                                                                        tableOutput('Scale1TestPM.5')
                                                               ),
                                                               tabPanel('Subset 2 Train [-0.5,0.5]',
                                                                        tableOutput('Scale2TrainPM.5')
                                                               ),
                                                               tabPanel('Subset 2 Test [-0.5,0.5]',
                                                                        tableOutput('Scale2TestPM.5')
                                                               ),
                                                               tabPanel('Subset 3 Train [-0.5,0.5]',
                                                                        tableOutput('Scale3TrainPM.5')
                                                               ),
                                                               tabPanel('Subset 3 Test [-0.5,0.5]',
                                                                        tableOutput('Scale3TestPM.5')
                                                               )
                                                  )
                                                ) 
                                       )
                            )
                   ),
                   tabPanel('Hyperparameter DOE',
                            fluidPage(
                              tabsetPanel(
                                tabPanel('Build Custom Designed Experiment',
                                         textOutput('CustDOE')
                                ),
                                tabPanel('Default Experimental Design',
                                         fluidPage(
                                           fluidRow(
                                             column(width = 12,
                                                    titlePanel('Default Experimental Design'),
                                                    tableOutput('DefaultDOE')
                                             )
                                           )
                                         )
                                )
                              )
                            )
                   ),
                   
                   tabPanel('Test Hyperparameters',
                            fluidPage(
                              fluidRow(
                                column(width = 12,
                                       selectInput('TestDesignSel',
                                                   'Select Hyperparameter Test Design',
                                                   list('Run Short Default Experimental Design',
                                                        'Run Full Default Experimental Design'),
                                                   selected = 'Run Short Default Experimental Design',
                                                   multiple = FALSE),
                                       textOutput('DOENote'),
                                       actionButton('ActionTestDesign', 
                                                    label = 'Run Autoencoder Designed Experiment'),
                                       tableOutput('SelectedExperiment')
                                )
                              )
                            )
                   ),
                   tabPanel('DOE Results',
                            tableOutput('ResultsTable')
                   ),
                   tabPanel('Identify Outliers',
                            fluidPage(
                              navlistPanel('Select to View',
                                           tabPanel('Top Outliers',
                                                    tableOutput('TopOutliers')),
                                           tabPanel('Outlier Score Histogram',
                                                    tableOutput('Histogram'))
                                           
                              )
                            )
                   )
  )
  
  server <- function(input, output){
    
    # Display Intro Material
    output$Intro <- renderUI({
      file = system.file('Intro.rmd', package = "IDPS.LAAD")
      withMathJax(HTML(markdown::markdownToHTML(knitr::knit(file))))
    })
    
    # Data Preperation
    # Load Tables and one hot encode using caret ----
    output$OneData <- renderText('Functionality currently limited to use of 
                                 Fisher Iris Data Only')
    irisdata <- as.tibble(iris)
    dmy <- caret::dummyVars(' ~ .', data = irisdata)
    irisdataOH <- data.frame(predict(dmy, newdata = irisdata))
    irisDataFull <- as.tibble(irisdataOH)
    
    output$Table <- renderTable(iris)
    
    # Split into Test and Train Subsets ----
    output$Subset1 <- renderText('Limited Functionality, Using Fisher Iris Data')
    set.seed = 12345
    testTrain1 <- irisDataFull %>%
      dplyr::mutate(Cross = sample(c(1,2), 
                                   size = nrow(irisDataFull), 
                                   replace = TRUE,
                                   prob = c(0.80, 0.20)))
    irisTrain1 <- testTrain1[testTrain1$Cross == 1, ] %>%
      dplyr::select(-Cross)
    irisTest1 <- testTrain1[testTrain1$Cross == 2, ] %>%
      dplyr::select(-Cross)
    output$Train1 <- renderTable(irisTrain1)
    output$Test1 <- renderTable(irisTest1)
    
    output$Subset2 <- renderText('Limited Functionality, Using Fisher Iris Data')
    set.seed = 123434
    testTrain2 <- irisDataFull %>%
      dplyr::mutate(Cross = sample(c(1,2), 
                                   size = nrow(irisDataFull), 
                                   replace = TRUE,
                                   prob = c(0.80, 0.20)))
    irisTrain2 <- testTrain2[testTrain2$Cross == 1, ] %>%
      dplyr::select(-Cross)
    irisTest2 <- testTrain2[testTrain2$Cross == 2, ] %>%
      dplyr::select(-Cross)
    output$Train2 <- renderTable(irisTrain2)
    output$Test2 <- renderTable(irisTest2)
    
    output$Subset3 <- renderText('Limited Functionality, Using Fisher Iris Data')
    set.seed = 134
    testTrain3 <- irisDataFull %>%
      dplyr::mutate(Cross = sample(c(1,2), 
                                   size = nrow(irisDataFull), 
                                   replace = TRUE,
                                   prob = c(0.80, 0.20)))
    irisTrain3 <- testTrain3[testTrain3$Cross == 1, ] %>%
      dplyr::select(-Cross)
    irisTest3 <- testTrain3[testTrain3$Cross == 2, ] %>%
      dplyr::select(-Cross)
    output$Train3 <- renderTable(irisTrain3)
    output$Test3 <- renderTable(irisTest3)
    
    # Data Scaling ----
    irisTrain1ZO <- customscale(irisTrain1, irisTrain1, 0, 1)
    output$Scale1TrainZO <- renderTable(irisTrain1ZO)
    irisTest1ZO <- customscale(irisTrain1, irisTest1, 0, 1)
    output$Scale1TestZO <- renderTable(irisTest1ZO)
    irisTrain2ZO <- customscale(irisTrain2, irisTrain2, 0, 1)
    output$Scale2TrainZO <- renderTable(irisTrain2ZO)
    irisTest2ZO <- customscale(irisTrain2, irisTest2, 0, 1)
    output$Scale2TestZO <- renderTable(irisTest2ZO)
    irisTrain3ZO <- customscale(irisTrain3, irisTrain3, 0, 1)
    output$Scale3TrainZO <- renderTable(irisTrain3ZO)
    irisTest3ZO <- customscale(irisTrain3, irisTest3, 0, 1)
    output$Scale3TestZO <- renderTable(irisTest3ZO)
    irisTrain1PM1 <- customscale(irisTrain1, irisTrain1, -1, 1)
    output$Scale1TrainPM1 <- renderTable(irisTrain1PM1)
    irisTest1PM1 <- customscale(irisTrain1, irisTest1, -1, 1)
    output$Scale1TestPM1 <- renderTable(irisTest1PM1)
    irisTrain2PM1 <- customscale(irisTrain2, irisTrain2, -1, 1)
    output$Scale2TrainPM1 <- renderTable(irisTrain2PM1)
    irisTest2PM1 <- customscale(irisTrain2, irisTest2, -1, 1)
    output$Scale2TestPM1 <- renderTable(irisTest2PM1)
    irisTrain3PM1 <- customscale(irisTrain3, irisTrain3, -1, 1)
    output$Scale3TrainPM1 <- renderTable(irisTrain3PM1)
    irisTest3PM1 <- customscale(irisTrain3, irisTest3, -1, 1)
    output$Scale3TestPM1 <- renderTable(irisTest3PM1)
    irisTrain1PM.5 <- customscale(irisTrain1, irisTrain1, -0.5, 0.5)
    output$Scale1TrainPM.5 <- renderTable(irisTrain1PM.5)
    irisTest1PM.5 <- customscale(irisTrain1, irisTest1, -0.5, 0.5)
    output$Scale1TestPM.5 <- renderTable(irisTest1PM.5)
    irisTrain2PM.5 <- customscale(irisTrain2, irisTrain2, -0.5, 0.5)
    output$Scale2TrainPM.5 <- renderTable(irisTrain2PM.5)
    irisTest2PM.5 <- customscale(irisTrain2, irisTest2, -0.5, 0.5)
    output$Scale2TestPM.5 <- renderTable(irisTest2PM.5)
    irisTrain3PM.5 <- customscale(irisTrain3, irisTrain3, -0.5, 0.5)
    output$Scale3TrainPM.5 <- renderTable(irisTrain3PM.5)
    irisTest3PM.5 <- customscale(irisTrain3, irisTest3, -0.5, 0.5)
    output$Scale3TestPM.5 <- renderTable(irisTest3PM.5)
    
    # Design of Expriments
    output$CustDOE <- renderText('Functionality comming in future release')
    
    DefaultDOE <- readr::read_csv('./data/testDesignShiny.csv', 
                                  col_names = TRUE) %>%
      select(-Y) %>%
      mutate('Mean_Square_Test_Error' = NA)
    output$DefaultDOE <- renderTable(DefaultDOE)
    
    output$DOENote <- renderText('Note: Short Designed Experiment only tests 
                                 test/train subset 1. Full Designed Experiment 
                                 tests all test/training subsets as a 
                                 Neural Network hyperparameter.')
    
    rv <- reactiveValues(testDesign = NULL)
    
    
    
    #Select appropriate test design
    observe({
      rv$testDesign <- `if`(input$TestDesignSel == 'Run Short Default Experimental Design',
                            readr::read_csv('./data/testDesignShiny.csv', col_names = TRUE) %>%
                              dplyr::filter(Subset_Split == 1),
                            readr::read_csv('./data/testDesignShiny.csv', col_names = TRUE)
      )
      output$SelectedExperiment <- renderTable(rv$testDesign)
    })
    
    
    observeEvent(input$ActionTestDesign, {
      
      
      # Load Autoencoder datasets ----
      path <- './data/'
      h2o::h2o.init()
      irisDataFullPM.5 <- h2o.importFile(path = paste0(path, 'irisDataFullPM.5','.csv'),
                                         header = TRUE)
      irisDataFullPM1 <- h2o.importFile(path = paste0(path, 'irisDataFullPM1','.csv'),
                                        header = TRUE)
      irisDataFullZO <- h2o.importFile(path = paste0(path, 'irisDataFullZO','.csv'),
                                       header = TRUE)
      irisTest1PM.5 <- h2o.importFile(path = paste0(path, 'irisTest1PM.5','.csv'),
                                      header = TRUE)
      irisTest1PM1 <- h2o.importFile(path = paste0(path, 'irisTest1PM1','.csv'),
                                     header = TRUE)
      irisTest1ZO <- h2o.importFile(path = paste0(path, 'irisTest1ZO','.csv'),
                                    header = TRUE)
      irisTest2PM.5 <- h2o.importFile(path = paste0(path, 'irisTest2PM.5','.csv'),
                                      header = TRUE)
      irisTest2PM1 <- h2o.importFile(path = paste0(path, 'irisTest2PM1','.csv'),
                                     header = TRUE)
      irisTest2ZO <- h2o.importFile(path = paste0(path, 'irisTest2ZO','.csv'),
                                    header = TRUE)
      irisTest3PM.5 <- h2o.importFile(path = paste0(path, 'irisTest3PM.5','.csv'),
                                      header = TRUE)
      irisTest3PM1 <- h2o.importFile(path = paste0(path, 'irisTest3PM1','.csv'),
                                     header = TRUE)
      irisTest3ZO <- h2o.importFile(path = paste0(path, 'irisTest3ZO','.csv'),
                                    header = TRUE)
      irisTrain1PM.5 <- h2o.importFile(path = paste0(path, 'irisTrain1PM.5','.csv'),
                                       header = TRUE)
      irisTrain1PM1 <- h2o.importFile(path = paste0(path, 'irisTrain1PM1','.csv'),
                                      header = TRUE)
      irisTrain1ZO <- h2o.importFile(path = paste0(path, 'irisTrain1ZO','.csv'),
                                     header = TRUE)
      irisTrain2PM.5 <- h2o.importFile(path = paste0(path, 'irisTrain2PM.5','.csv'),
                                       header = TRUE)
      irisTrain2PM1 <- h2o.importFile(path = paste0(path, 'irisTrain2PM1','.csv'),
                                      header = TRUE)
      irisTrain2ZO <- h2o.importFile(path = paste0(path, 'irisTrain2ZO','.csv'),
                                     header = TRUE)
      irisTrain3PM.5 <- h2o.importFile(path = paste0(path, 'irisTrain3PM.5','.csv'),
                                       header = TRUE)
      irisTrain3PM1 <- h2o.importFile(path = paste0(path, 'irisTrain3PM1','.csv'),
                                      header = TRUE)
      irisTrain3ZO <- h2o.importFile(path = paste0(path, 'irisTrain3ZO','.csv'),
                                     header = TRUE)
      
      # Evaluate Test Designs ----
      testDesignRes <- rv$testDesign %>%
        dplyr::select(-Y) %>%
        tibble::add_column(Dropout = TRUE, DPt = NA, Train_MSE = NA, Test_MSE = NA, Train_Time = NA,
                           Notes = NA)
      
      n = nrow(testDesignRes)
      
      for (i in 1:n){
        
        tryCatch({
          h_x <- paste0(testDesignRes$Design_Point[i])
          h_training_frame <- paste0('iris','Train', testDesignRes$Subset_Split[i],
                                     testDesignRes$Data_Scale[i])
          h_validation_frame <- paste0('iris','Test', testDesignRes$Subset_Split[i],
                                       testDesignRes$Data_Scale[i])
          ifelse(testDesignRes$Dropout[i]==TRUE, h_activation <- paste0(testDesignRes$Activation_Function[i], 'WithDropout'),
                 h_activation <- testDesignRes$Activation_Function[i])
          h_hidden <- testDesignRes$Number_Neurons[i]
          h_adaptive_rate <- TRUE
          h_rho <- testDesignRes$Rho[i]
          h_epsilon <- testDesignRes$Epsilon[i]
          h_NAG <- TRUE
          h_input_dropout_rate <- ifelse(testDesignRes$Dropout[i]==TRUE, testDesignRes$Input_DO_Rate[i], NULL)
          h_hidden_dropout_rate <- ifelse(testDesignRes$Dropout[i]==TRUE, testDesignRes$Hidden_DO_Rate[i], NULL)
          h_initial_weight_dist <- testDesignRes$Initial_Weight_Dist[i]
          h_shuffle_train_data <- testDesignRes$Shuffle_Train_Data[i]
          
          modelName <- paste0('DpT: ', testDesignRes$Design_Point[i], '_Subset: ',
                              testDesignRes$Subset_Split[i])
          assign(modelName,
                 h2o.deeplearning(model_id = modelName,
                                  x = h2o.names(get(h_training_frame)),
                                  training_frame = get(h_training_frame),
                                  validation_frame = get(h_validation_frame),
                                  activation = h_activation,
                                  hidden = h_hidden,
                                  adaptive_rate = h_adaptive_rate,
                                  rho = h_rho,
                                  epsilon = h_epsilon,
                                  nesterov_accelerated_gradient = h_NAG,
                                  input_dropout_ratio = h_input_dropout_rate,
                                  hidden_dropout_ratios = h_hidden_dropout_rate,
                                  initial_weight_distribution = h_initial_weight_dist,
                                  shuffle_training_data = h_shuffle_train_data,
                                  #
                                  sparse = FALSE,
                                  ignore_const_cols = FALSE,
                                  score_each_iteration = TRUE,
                                  standardize = FALSE,
                                  epochs = 1000,
                                  train_samples_per_iteration = -2,
                                  target_ratio_comm_to_comp = -2,
                                  loss = 'Quadratic',
                                  distribution = 'AUTO',
                                  score_interval = 5,
                                  score_training_samples = 0,
                                  score_validation_samples = 0,
                                  score_duty_cycle = 0.1,
                                  regression_stop = 1e-08,
                                  stopping_rounds = 5,
                                  stopping_metric = 'MSE',
                                  stopping_tolerance = 1e-08,
                                  max_runtime_secs = 60,
                                  score_validation_sampling = 'Uniform',
                                  diagnostics = TRUE,
                                  fast_mode = TRUE,
                                  force_load_balance = TRUE,
                                  variable_importances = TRUE,
                                  replicate_training_data = TRUE,
                                  export_weights_and_biases = TRUE,
                                  quiet_mode = FALSE,
                                  autoencoder = TRUE))
          
          testDesignRes$Train_MSE[i] <- h2o.mse(get(modelName), train = TRUE)
          testDesignRes$Test_MSE[i] <- h2o.mse(get(modelName), valid = TRUE)
          testDesignRes$Train_Time[i] <- get(modelName)@model$run_time/1000
          testDesignRes$DPt[i] <- i
          print(i)
        },
        error = function(cond){
          testDesignRes$Notes[i] <- 'Exponential Growth'
          i = i+1
        }
        )
      }
      
      output$ResultsTable <- renderTable(testDesignRes)
      
      #Plot MSE Reconstructions and Present top n outliers
      by_Dpt <- testDesignRes %>%
        group_by(Design_Point) %>%
        summarise(AvgTestMSE = mean(Test_MSE),
                  MaxTestMSE = max(Test_MSE),
                  MinTestMSE= min(Test_MSE),
                  StdevTestMSE = sd(Test_MSE),
                  CountTestMSE = n()) %>%
        mutate(AvgMSERank = rank(AvgTestMSE))
      
      
    })
  }
  
  runGadget(ui, server, viewer = browserViewer(browser = getOption('browser')))
}