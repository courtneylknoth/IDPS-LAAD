#'
#' @title Interactive anomaly detection using autoencoder neural network
#' 
#' @import shiny
#' @import knitr
#' @import tidyverse
#' @importFrom shinythemes shinytheme
#' 
#' @export

IDPS_Gadget <- function(...){
  
  ui <- navbarPage(title = 'Anomaly Detection Autoencoder',
                   tabPanel('Intro and Instructions', 
                            uiOutput('Intro', inline = TRUE)),
                   tabPanel('Data Preperation',
                            textOutput('DataPrep')),
                   tabPanel('Hyperparameter DOE',
                            textOutput('DOE')),
                   tabPanel('Test Hyperparameters',
                            textOutput('Tests')),
                   tabPanel('Plot Outliers',
                            textOutput('Outliers'))
                   )
  
  server <- function(input, output){
    
    output$IntroText <- renderUI({
      file = system.file('Intro.rmd', package = "IDPS.LAAD")
      withMathJax(HTML(markdown::markdownToHTML(knitr::knit(file))))
    })
    
    
    output$DataPrep <- renderText('Tab2')
    output$DOE <- renderText('Tab3')
    output$Tests <- renderText('Tab4')
    output$Outliers <- renderText('Tab5')
    
  }
  
  runGadget(ui, server, viewer = browserViewer(browser = getOption('browser')))
}