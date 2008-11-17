\name{Met.peak.detection}
\alias{Met.peak.detection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find Peaks 
 }
\description{
  Find Peaks in the Spectra Data. 
}
\usage{
Met.peak.detection(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
}
\details{
The alignment of peaks is an alternative to binning the spectrum to account for peak shifts. A peak detection graphical application (Preprocessing / Peak Detection) has been developed to control the ''msc.peaks.find'' function from caMassClass library.
The graphical application adjusts the signal-to-noise ratio and the threshold criterion in the peak's detection process and it returns a data frame with the positions and intensities of the detected peaks.
Launched with the GUI. Beta version.

}
\value{
  \item{Peaks }{  Data frame with the detected peaks information}

}
\references{  caMassClass package  \url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/msc.peaks.find.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
\seealso{ \link{Met.peak.detection} }


\keyword{ interface }
