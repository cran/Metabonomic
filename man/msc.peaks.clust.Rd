\name{msc.peaks.clust}
\Rdversion{1.1}
\alias{msc.peaks.clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clusters Peaks of Mass Spectra
}
\description{
Clusters peaks from multiple protein mass spectra (SELDI) samples. 
Transcription of msc.peaks.clust function of caMassClass

}
\usage{
msc.peaks.clust(dM, S, BinSize = c(0, sum(dM)), tol = 0.97, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dM}{
Distance between sorted peak positions (masses, m/z).
}
  \item{S}{
Peak sample number, used to identify the spectrum the peak come from.
}
  \item{BinSize}{
Upper and lower bound of bin-sizes, based on expected experimental variation in the mass (m/z) values. Size of any bin is measured as (R-L)/mean(R,L) where L and R are masses (m/z values) of left and right boundaries. All resulting bin sizes will be between BinSize[1] and BinSize[2]. Default is c(0,sum(dM)) which ensures that no BinSizes is not being used.
}
  \item{tol}{
gaps bigger than tol*max(gap) are assumed to be the same size as the largest gap. See details.
}
  \item{verbose}{
boolean flag turns debugging printouts on.
}
}
\details{
This is a low level function used by msc.peaks.alignment and not intended to be directly used by many users. However it might be useful for other code developers. It clusters peaks from different samples into bins in such a way as to satisfy constraints in following order:

    * bin sizes are in between BinSize[1] and BinSize[2]
    * no two peaks from the same sample are present in the same bin
    * bins are split in such a way as to minimize bin size and maximize spaces between bins
    * if there are multiple, equally good, ways to split a bin than bin is split in such a way as to minimize number of repeats on each smaller sub-bin 
}
\value{
The output is binary array of the same size as dM and S where left boundaries of each clusters-bin (biomarker) are marked
}
\author{
Jarek Tuszynski (SAIC) jaroslaw.w.tuszynski@saic.com
}
\references{\url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}}
\keyword{ interface }

