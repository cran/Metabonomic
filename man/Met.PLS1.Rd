\name{Met.PLS1}
\alias{Met.PLS1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generalized Partial Least Squares 
}
\description{
  Partial least squares is a commonly used dimension reduction technique.  The code in this function uses the extension proposed by Ding and Gentleman, 2004. 

}
\usage{
Met.PLS1(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet}

}
\details{
The PLS graphical application (Metabonomic Analysis / Partial Least Squares / PLS) has been developed with a PLS algorithm based on the extension of the generalized partial least squares model proposed by Ding and Gentleman. This algorithm is implemented using the ''gpls'' function from the ''gpls'' package and it allows one to separate only two classes of samples. 
The graphical application controls the manual or random selection of the samples to build the model, the selection of all the algorithm parameters as the tolerance to the convergence, the number of iterations allowed or the number of PLS components used.  At the end, the results of the validation test will be returned.
Launched with the GUI. Beta version.

}
\references{ 
 gpls package  \url{http://finzi.psych.upenn.edu/R/library/gpls/html/gpls.html}

Ding, B.Y. and Gentleman, R. (2003) .Classification using generalized partial least squares. 

Marx, B.D (1996) .Iteratively reweighted partial least squares estimation for generalized linear regression. Technometrics 38(4): 374-381. 
}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
