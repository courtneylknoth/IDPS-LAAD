\name{IDP.-LAAD-package}
\docType{package}
\title{
  Intrusion Detection Prevention System Log Autoencoder Anomaly Detector
}
\description{
  IDPS-LAAD is designed to read in a .csv containing the IDPS cyber 
  log. The program will allow users to select what features (variables) in the 
  data they wish to analyze. The program will then automatically prepare the 
  data for ANN analysis. Once the data is prepared for ANN analysis, the 
  IDPS-LAAD will automatically generate a designed experiment to test ANN 
  hyperparameters, and subsequently run ANNs testing each of the 
  hyperparameters. Once the 'optimal' hyperparameters are found the the analytic 
  will analyze the IDPS cyber data and graphically display the results as well 
  as output a user defined top 'n' anomalous observations.
}
\details{
  Currently this package has limited functionality as a shiny gadget. 
}
\author{
Spencer Butt, <spencer.a.butt@gmail.com>.

Maintainer: Spencer Butt <spencer.a.butt@gmail.com>
}
\references{
  \href{http://www.deeplearningbook.org/contents/autoencoders.html}{Deep Learning Book Chapter 14: Autoencoders}
}
\keyword{ package }

\seealso{
  \code{\link[h2o]{h2o.deeplearning}}
}

\examples{
  \dontrun{
    # To launch the gadget, call the function:
    IDPS_Gadget()
  }
}
