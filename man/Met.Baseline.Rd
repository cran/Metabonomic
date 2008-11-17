\name{Met.Baseline}
\alias{Met.Baseline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseline Correction }
\description{
  Baseline Correction of raw spectra
}
\usage{
Met.Baseline(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{Spectra data frame }
}
\details{
Baseline correction may be a very essential step to obtain high quality NMR spectra in some cases. Rolling baselines can make it difficult to identify peaks, as well as introduce significant errors into any quantitative measurements. In order to avoid these influences, the ''Metabonomic'' GUI incorporates an application to reduce this influence in batch mode. The baseline correction is performed using the ''bslnoff'' function, based in the LOESS method, from PROcess library. This graphical application (Preprocessing / Baseline) allows controlling the Bandwidth to be passed to LOESS function until the adjustment is correct. Graphics with the raw spectrum, estimated baseline and baseline-subtracted spectrum are plotted in the R console.
  
Launched with the GUI. Beta version.
}

\references{caMassClass package  \url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }