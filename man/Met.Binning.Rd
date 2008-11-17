\name{Met.Binning}
\alias{Met.Binning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Binning }
\description{
  The function 'binning' takes a matrix of spectra and constructs a matrix of intensities of reduced dimensions
 based on an equally spaced mesh of interval breaks. 

}
\usage{
Met.Binning(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{  Spectra data frame }
}
\details{
The most common method of reducing the influence of shifting peaks is the so-called binning or bucketing method, which reduces the spectrum resolution.  Thereby the spectra are integrated within small spectral regions, called ''bins'' or ''buckets''. Subsequent data analysis procedures, which are applied to the binned spectra, are not influenced by peak shifts, as long as these shifts remain within the borders of the corresponding bins. After launching the Binning graphical applications (Preprocessing / Binning), the user can select the bin size. The spectra integration is performed by running the ''binning'' function from the PROcess library. 
Launched with the GUI. Beta version.

}
\references{ caMassClass package  \url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}
 }
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }