\name{Met.kopls}
\Rdversion{1.1}
\alias{Met.kopls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kernel-based orthogonal projections to latent structures (K-OPLS)
}
\description{
An implementation of K-OPLS based on 'kopls' package with e.g. model training, prediction,
     cross-validation and plot tools.
}
\usage{
Met.kopls(datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{Spectra data frame
}
}
\details{
The application (Metabonomic Analysis / Partial Least Squares / Kernel-based Orthogonal PLS) is performed using the functions from the ''kopls'' package. 
Firstly, the function perform a K-OPLS cross-validation for a set of 'Y'-orthogonal components. The function returns a number of diagnostic parameters which can be used to determine the optimal number of model components. 
With the optimal parameters selected ,  the application performs prediction of no-model samples from the K-OPLS model.
The function projects the Y-predictive and Y-orthogonal scores components to predict a value of the response matrix Y.
}
\references{
 Rantalainen M, Bylesjo M, Cloarec O, Nicholson JK, Holmes E and Trygg J. *Kernel-based orthogonal projections to latent structures (K-OPLS)*, _J Chemometrics_ 2007; 21:376-385.
}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
