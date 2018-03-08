\name{IDPS.LAAD-package}
\alias{IDPS.LAAD}
\alias{IDPS-LAAD}
\docType{package}
\title{
  Intrusion Detection Prevention System Log Autoencoder Anomaly Detector
}
\description{
  IDPS-LAAD is designed to read in a .csv containing the \emph{Intrusion Detection 
  Prevention System} (\strong{IDPS}) cyber log. The program will allow users to 
  select what features (variables) in the data they wish to analyze. The program 
  will then automatically prepare the data for anomaly detection analysis with an 
  \emph{Autoencoder Neural Network} (\strong{ANN}). Once the data is prepared for 
  ANN analysis, the IDPS-LAAD can automatically generate a designed experiment to 
  test ANN hyperparameters, and subsequently run ANNs testing each of the 
  hyperparameters. Once the 'optimal' hyperparameters are found the the analytic 
  will analyze the IDPS cyber data and graphically display the results as well 
  as output a user defined top 'n' anomalous observations.
}
\details{
  Currently this package has limited functionality as a shiny gadget. \cr \cr You 
  \strong{ARE REQUIRED} to first load the library: \cr
  \preformatted{library('IDPS.LAAD')
  }
  \strong{THEN}, launch the gadget \cr
  \preformatted{IDPS_Gadget
  }
  Calling the following without first loading the library 
  \strong{will return an error}:
  \preformatted{IDPS.LAAD::IDPS_Gadget()
  }
}
\author{
Spencer Butt, <spencer.a.butt@gmail.com>.

Maintainer: Spencer Butt <spencer.a.butt@gmail.com>
}
\references{
\itemize{
  \item{\href{http://www.deeplearningbook.org/contents/autoencoders.html}{Deep Learning Book Chapter 14: Autoencoders}}
  \item{\href{http://http://www.deeplearningbook.org/contents/guidelines.html}{Deep Learning Book Chapter 11: Practical Methodology (Hyperparameters)}
  }}
}
\keyword{ package }

\seealso{
  \code{\link[h2o]{h2o.deeplearning}}
}

\examples{
  \dontrun{
    # To launch the gadget, you must first load the library, then call the function:
    library('IDPS.LAAD')
    IDPS_Gadget()
  }
}
