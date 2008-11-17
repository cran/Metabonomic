\name{Met.LDA}
\alias{Met.LDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Linear Discriminant Analysis}
\description{
 Linear Discriminant Analysis.}
\usage{
Met.LDA(datos, externa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{ Spectra data frame}
  \item{externa}{ Not implemented yet }

}
\details{ 
Linear Discriminant Analysis (LDA) is another common technique for the analysis of metabonomic data. LDA is used to obtain linear discriminant functions, a linear combination of the original variables chosen to maximize the differences of the classes. The linear discriminant function is calculated by the ''lda'' function from the ''MASS'' package. 
The user is guided by the program to perform all the tasks in the proper order. Firstly, a model of LDA is built with part of the samples and the rest of the samples will be utilized to do a validation test. The user can directly choose the samples to make the model or select the number of samples of each class to be chosen in a random selection.
Secondly, the user can select the algorithm to calculate the LDA among ''moment'' for standard estimators of the mean and variance, ''mle'' for a Maximum likelihood Estimation or ''t'' for robust estimates based on a t distribution. Finally, the LDA graphical application returns the results of the validation test and different interactive graphics of the LDA model. 
If the number of different classes is less than three, the interactive graphic will be a plane where the samples used to build the model and the validation samples will be plotted. 
If the number of different classes is more than two, the samples used to build the model and the validations samples will be plotted in interactive cubes. In these interactive plots, the user can select the angle of rotation, the components showed and other graphical parameters.  

Launched with the GUI. Beta version.

}
\references{ MASS package  \url{http://finzi.psych.upenn.edu/R/library/MASS/html/lda.html}}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
