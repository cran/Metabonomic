\name{Baseline.Correction}
\alias{Baseline.Correction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseline Correction (FTICRMS) }
\description{
  Baseline Correction of raw spectra based in the FTICRMS package.
}
\usage{
Baseline.Correction()
}
%- maybe also 'usage' for other objects documented here.
\details{
Baseline correction may be a very essential step to obtain high quality NMR spectra in some cases. Rolling baselines can make it difficult to identify peaks, as well as introduce significant errors into any quantitative measurements. This application based in the FTICRMS package is available for the individual baseline correction. It computes an estimated baseline curve for a spectrum by a method of Rocke and Xi . 
Launched with the GUI. Beta version.
}

\references{FTICRMS package  \url{http://cran.r-project.org/web/packages/FTICRMS/index.html}}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
