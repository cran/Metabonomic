\name{Import.data}
\alias{Import.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bruker spectra}
\description{
  Import.data loads the Brucker Spectra for the metabonomic analysis.
}
\usage{
Import.data()
}
%- maybe also 'usage' for other objects documented here.
\details{
Import.data  has been developed to load directly the spectra in the Bruker spectroscopy format. Import.data can also be executed by selecting the 'file/ Import Bruker file' tab of the Metabonomic GUI. The user has to select the raw data (FID file in the Bruker data directory). This application displays the spectrum reference and manages basic operations such as setting the chemical shift of a certain compound (TSP or DSS) to 0 ppm and zero order and first order phase corrections. When the first set of data is loaded, the GUI asks for a new array. After finishing importing all the spectra, the GUI asks for the ''info'' text file with the information of the different spectra,
where the first column holds the names of the spectra and the different characteristics are in the followings columns.
Applications to load other commercial data formats will be added soon.

Example of an info file:
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


