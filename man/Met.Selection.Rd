\name{Met.Selection}
\alias{Met.Selection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Category Selection }
\description{
  The data are classified according to the information loaded in the info file. 
}
\usage{
Met.Selection(datos, externa.inicial)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{   Spectra data frame }
  \item{externa.inicial}{   Not implemented yet }
}
\details{
This application selects the information that will be used in future supervised analyses.  Firstly, the GUI asks which characteristic (different columns of the info file) will be used to classify the samples. Afterwards, the user chooses the different types of samples that will be utilized in the multivariate analyses. So far, the program only allows the selection of four different samples. The ''Category Selection'' application is launched selecting the ''file/ Category Selection'' tab.
Launched with the GUI. Beta version.

}
\value{
   \item{datos }{Spectra data frame}
  \item{... }{Internal values}
  ...
}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
