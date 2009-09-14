\name{msc.biomarkers.fill}
\Rdversion{1.1}
\alias{msc.biomarkers.fill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill Empty Spaces in Biomarker Matrix
}
\description{
Fill empty spaces (NA's) in biomarker matrix created by msc.peaks.align.
Transcription of msc.biomarkers.fill function of caMassClass
}
\usage{
msc.biomarkers.fill(X, Bmrks, BinBounds, FillType = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Spectrum data either in matrix format [nFeatures x nSamples] or in 3D array format [nFeatures x nSamples x nCopies]. 
Row names (rownames(X)) store M/Z mass of each row. 
}
  \item{Bmrks}{
biomarker matrix containing one sample per column and one biomarker per row
}
  \item{BinBounds}{
position (mass) of left-most and right-most peak in each bin
}
  \item{FillType}{
how to fill empty spaces in biomarker data?

    * if 0<=FillType<=1 than fill spaces with quantile(probs=FillType). For example: if FillType=1/2 than medium will be used, if FillType=1 than maximum value will be used, if FillType=0.9 than maximum will be used after discarding 10% of 'outliers'
    * if FillType<0 than empty spaces will not be filled and NA's will remain
    * if FillType==2 than X value closest to the center of the bin will be used
    * if FillType==3 empty spaces will be set to zero }
}
\details{
Transcription of masc.biomarkers.fill function of caMassClass
}
\value{
Data in the same format and size as Bmrks
}
\author{
Jarek Tuszynski (SAIC) jaroslaw.w.tuszynski@saic.com
}
\references{caMassClass  \url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}}
\keyword{ interface }

