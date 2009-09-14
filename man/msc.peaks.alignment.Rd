\name{msc.peaks.alignment}
\Rdversion{1.1}
\alias{msc.peaks.alignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Align Peaks of Mass Spectra into a "Biomarker" Matrix
}
\description{
Align peaks from multiple protein mass spectra (SELDI) samples into a single "biomarker" matrix 
Transcription of msc.peaks.align function of caMassClass
}
\usage{
msc.peaks.alignment(S, M, H, Tag = 0, SampFrac = 0.3, BinSize = c(0.002, 0.008), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
Peak sample number. Unique number of the sample the peak belongs to. Likely to come from Peaks$Spectrum. 
}
  \item{M}{
Peak center mass. Position of the peak on the x-axis. Likely to come from Peaks$Substance.Mass.
}
  \item{H}{
Peak height. Likely to come from Peaks$Intensity.
}
  \item{Tag}{
Peak sample name. Unique name of the sample the peak belongs to. Likely to come from Peaks$Spectrum.Tag. Optional since is used only to set column-names of output data.
}
  \item{SampFrac}{
After peak alignment, bins with fewer peaks than SampFrac*nSamp are removed.
}
  \item{BinSize}{
Upper and lower bound of bin-sizes, based on expected experimental variation in the mass (m/z) values. Size of any bin is measured as (R-L)/mean(R,L) where L and R are masses (m/z values) of left and right boundaries. All resulting bin sizes will all be between BinSize[1] and BinSize[2]. Since SELDI data is often assumed to have +- 3% mass drift than a good bin size is twice that number (0.006). Same as BinSize variable in msc.peaks.clust, except for default. 
}
  \item{\dots}{
Two additional parameters that can be passed to msc.peaks.clust are mostly for expert users fine-tuning the code:

    * tol - gaps bigger than tol*max(gap) are assumed to be the same size as the largest gap. See details.
    * verbose - boolean flag turns debugging printouts on. 
}
}
\details{
Two interfaces were provided to the same function:

    * msc.peaks.alignment is a lower level function with more detailed inputs and outputs. Possibly easier to customize for other purposes than processing SELDI data.
    * msc.peaks.align is a higher level function with simpler interface customized for processing SELDI data. 

This function aligns peaks from different samples into bins in such a way as to satisfy constraints in following order:

    * bin sizes are in between BinSize[1] and BinSize[2]
    * no two peaks from the same sample are present in the same bin
    * bins are split in such a way as to minimize bin size and maximize spaces between bins
    * if there are multiple, equally good, ways to split a bin than bin is split in such a way as to minimize number of repeats on each smaller sub-bin 

The algorithm used does the following:

    * Store mass and sample number of each peak into an array
    * Concatenate arrays from all samples and sort them according to mass
    * Group sets of peaks into subsets (bins). Each subset will consist of peaks from different spectra that have similar mass. That is done by puting all peaks into a single bin and recursively going through the following steps:
          o Check size of the current bin: if it is too small than we are done, if it is too big than it will be split and if it is already in the desired range than it will be split only if multiple peaks from the same sample are present.
          o If bin needs to be split than find the biggest gap between peaks
          o If multiple gaps were found with the same size as the largest gap (or within tol tolerance from it) than minimizes number of multiple peaks from the same sample after cut
          o Divide the bin into two sub-bins: to the left and to the right of the biggest gap
          o Recursively repeat the above four steps for both sub-bins 
    * Store peaks into 2D array (bins by samples)
    * Remove bins with fewer peaks than SampFrac*nSamp 

The algorithm for peak alignment is described as recursive algorithm but the actual implementation uses internal stack, instead in order to increase speed. 
}
\value{

\item{Bmrks}{Biomarker matrix containing one sample per column and one biomarker per row. If a given sample does not have a peak in some bin than NA is inserted.}
\item{BinBounds }{Mass of left-most and right-most peak in the bin}
%% ...
}
\author{
Jarek Tuszynski (SAIC) jaroslaw.w.tuszynski@saic.com
}
\references{\url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}}
\keyword{ interface }
