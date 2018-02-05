#'@title Runs an app in my package
#'
#'@description 
#'Runs as shiiny APP
#'
#'@param app_name Character string for a directory in this package
#'@param ... Additional options passed to shinyAppDir
#'
#'@return A printed shiny app
#'
#'@importFrom shiny shinyAppDir
#'
#'@examples
#'\dontrun{ run_my_app('myFirstApp')}
#'
#'@export
run_my_app <- function(app_name,...) {
  
  app_dir <- system.file('apps', app_name, package = 'IDPS.LAAD')
  
  shiny::shinyAppDir(appDir = app_dir, options = list(...))
  
}
