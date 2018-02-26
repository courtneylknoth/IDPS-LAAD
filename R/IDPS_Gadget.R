#' @title Interactive anomaly detection using autoencoder neural network
#' 
#' @import shiny
#' @import tidyverse
#' @import caret
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
                                                navbarPage(title = 'Data Split',
                                                           tabPanel('Data Subset 1',
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(width = 12,
                                                                               textOutput('Subset1'),
                                                                               titlePanel('Training Subset'),
                                                                               tableOutput('Train1'),
                                                                               titlePanel('Testing Subset'),
                                                                               tableOutput('Test1')
                                                                        )
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel('Data Subset 2',
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(width = 12,
                                                                               textOutput('Subset2'),
                                                                               titlePanel('Training Subset'),
                                                                               tableOutput('Train2'),
                                                                               titlePanel('Testing Subset'),
                                                                               tableOutput('Test2')
                                                                        )
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel('Data Subset 3',
                                                                    fluidPage(
                                                                      fluidRow(
                                                                        column(width = 12,
                                                                               textOutput('Subset3'),
                                                                               titlePanel('Training Subset'),
                                                                               tableOutput('Train3'),
                                                                               titlePanel('Testing Subset'),
                                                                               tableOutput('Test3')
                                                                        )
                                                                      )
                                                                    )
                                                           )
                                                           
                                                )
                                       ),
                                       tabPanel('Scale Data', 
                                                fluidPage(
                                                  navlistPanel('Select Dataset to View',
                                                              id = 'DatasetSel',
                                                    tabPanel('Subset 1 Train [0,1]', 
                                                             tableOutput('Scale1TrainZO')),
                                                    tabPanel('Subset 1 Test [0,1]', 
                                                             tableOutput('Scale1TestZO')),
                                                    tabPanel('Subset 2 Train [0,1]',
                                                             tableOutput('Scale2TrainZO')),
                                                    tabPanel('Subset 2 Test [0,1]',
                                                             tableOutput('Scale2TestZO')),
                                                    tabPanel('Subset 3 Train [0,1]',
                                                             tableOutput('Scale3TrainZO')),
                                                    tabPanel('Subset 3 Test [0,1]',
                                                             tableOutput('Scale3TestZO')),
                                                    tabPanel('Subset 1 Train [-1,1]', 
                                                             tableOutput('Scale1TrainPM1')),
                                                    tabPanel('Subset 1 Test [-1,1]', 
                                                             tableOutput('Scale1TestPM1')),
                                                    tabPanel('Subset 2 Train [-1,1]',
                                                             tableOutput('Scale2TrainPM1')),
                                                    tabPanel('Subset 2 Test [-1,1]',
                                                             tableOutput('Scale2TestPM1')),
                                                    tabPanel('Subset 3 Train [-1,1]',
                                                             tableOutput('Scale3TrainPM1')),
                                                    tabPanel('Subset 3 Test [-1,1]',
                                                             tableOutput('Scale3TestPM1')),
                                                    tabPanel('Subset 1 Train [-0.5,0.5]', 
                                                             tableOutput('Scale1TrainPM.5')),
                                                    tabPanel('Subset 1 Test [-0.5,0.5]', 
                                                             tableOutput('Scale1TestPM.5')),
                                                    tabPanel('Subset 2 Train [-0.5,0.5]',
                                                             tableOutput('Scale2TrainPM.5')),
                                                    tabPanel('Subset 2 Test [-0.5,0.5]',
                                                             tableOutput('Scale2TestPM.5')),
                                                    tabPanel('Subset 3 Train [-0.5,0.5]',
                                                             tableOutput('Scale3TrainPM.5')),
                                                    tabPanel('Subset 3 Test [-0.5,0.5]',
                                                             tableOutput('Scale3TestPM.5'))
                                                  )
                                                ) 
                                       )
                            )
                   ),
                   tabPanel('Hyperparameter DOE',
                            fluidPage(
                              tabsetPanel(
                                tabPanel('Build Custom Designed Experiment',
                                         textOutput('CustDOE')),
                                tabPanel('Default Experimental Design',
                                         fluidPage(
                                           fluidRow(
                                             column(width = 12,
                                                    titlePanel('Default Experimental Design'),
                                                    tableOutput('DefaultDOE'))
                                           )
                                         ))
                              )
                            )
                   ),
                   tabPanel('Test Hyperparameters',
                            textOutput('Tests')
                   ),
                   tabPanel('Plot Outliers',
                            textOutput('Outliers')
                   )
  )
  
  server <- function(input, output){
    
    # Display Intro Material
    output$Intro <- renderUI({
      file = system.file('Intro.rmd', package = "IDPS.LAAD")
      withMathJax(HTML(markdown::markdownToHTML(knitr::knit(file))))
    })
    
    # Data Preperation
    # Load Tables and one hot encode using caret
    output$OneData <- renderText('Functionality currently limited to use of 
                                 Fisher Iris Data Only')
    irisdata <- as.tibble(iris)
    dmy <- caret::dummyVars(' ~ .', data = irisdata)
    irisdataOH <- data.frame(predict(dmy, newdata = irisdata))
    irisDataFull <- as.tibble(irisdataOH)
    
    output$Table <- renderTable(iris)
    
    # Split into Test and Train Subsets
    output$Subset1 <- renderText('Limited Functionality, Using Fisher Iris Data')
    set.seed = 12345
    testTrain1 <- irisDataFull %>%
      dplyr::mutate(Cross = sample(c(1,2), size = nrow(irisDataFull), replace = TRUE,
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
      dplyr::mutate(Cross = sample(c(1,2), size = nrow(irisDataFull), replace = TRUE,
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
      dplyr::mutate(Cross = sample(c(1,2), size = nrow(irisDataFull), replace = TRUE,
                                   prob = c(0.80, 0.20)))
    irisTrain3 <- testTrain3[testTrain3$Cross == 1, ] %>%
      dplyr::select(-Cross)
    irisTest3 <- testTrain3[testTrain3$Cross == 2, ] %>%
      dplyr::select(-Cross)
    output$Train3 <- renderTable(irisTrain3)
    output$Test3 <- renderTable(irisTest3)
    
    # Data Scaling
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
    
    DefaultDOE <- readr::read_csv('./data/testDesignShiny.csv', col_names = TRUE) %>%
      select(-Y) %>%
      mutate('Mean_Square_Test_Error' = NA)
    
    output$DefaultDOE <- renderTable(DefaultDOE)
    
    
    
    output$Tests <- renderText('Tab4')
    
    # Plot MSE Reconstructions and Present top n outliers
    output$Outliers <- renderText('Tab5')
    
  }
  
  runGadget(ui, server, viewer = browserViewer(browser = getOption('browser')))
}