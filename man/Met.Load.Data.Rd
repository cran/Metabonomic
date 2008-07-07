\name{Met.Load.Data}
\alias{Met.Load.Data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Data spactra importation}
\description{
  Met.Load.Data loads data spectra txt file for the metabonomic analysis.
}
\usage{
Met.Load.Data()
}
%- maybe also 'usage' for other objects documented here.
\details{
  Met.Load.Data is launched with the GUI. 
  Load a data spectra file where the first column is the chemical shift(in ppm) and the intensities of the
  different spectra are in the followings columns. The data file has to be written without header. 
  In addition you have to load a 'info' text file with the information of the different spectra, where the 
  first column holds the names of each spectrum and the different characteristics are in the followings columns.

Example of info file:

\tabular{rllll}{
\tab Name \tab Category \tab   Disease \tab 	 Sex \cr
\tab 43FC \tab Control  \tab 	Healthy \tab 	F \cr	
\tab 45FC \tab Control  \tab 	Healthy \tab 	F \cr	
\tab 04MC \tab Control  \tab 	Healthy \tab 	M \cr
\tab 54MC \tab Control \tab 	Healthy	\tab 	M \cr
\tab 55FC \tab Control \tab 	Healthy	\tab 	F \cr
\tab 55MC \tab Control	 \tab Healthy	\tab 	M \cr
\tab 09FF \tab Treated	 \tab Smoker	\tab 	F \cr
\tab 09MF \tab Treated	 \tab Smoker	\tab 	M \cr
\tab 53Ff \tab Treated	 \tab Smoker	\tab 	F \cr
\tab 53MF \tab Treated	 \tab Smoker	\tab 	M \cr
\tab 10MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 11FA \tab Treated	 \tab Asthmatic	\tab 	F \cr
\tab 11MA \tab Treated	 \tab Asthmatic	\tab 	F \cr
\tab 42FA \tab Treated	 \tab Asthmatic	\tab 	F \cr
\tab 42MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 46MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 19MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 46FA \tab Treated	 \tab Asthmatic	\tab 	F \cr
\tab 49MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 50MA \tab Treated	 \tab Asthmatic	\tab 	M \cr
\tab 12FE \tab Treated	 \tab Emphysema	\tab 	F \cr
\tab 12ME \tab Treated	 \tab Emphysema	\tab 	M \cr
\tab 37ME \tab Treated	 \tab Emphysema	\tab 	M \cr
\tab 38FE \tab Treated	 \tab Emphysema	\tab 	F \cr
\tab 41ME \tab Treated	 \tab Emphysema	\tab 	M \cr
\tab  . \tab . \tab  .	 \tab . \cr
\tab  . \tab . \tab  .	 \tab . \cr
\tab  . \tab . \tab  .	 \tab . \cr
}
}

\value{
  \item{datos }{Spectra data frame}
  \item{memory.Data }{Internal value}
  \item{memory }{Internal value}
}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
