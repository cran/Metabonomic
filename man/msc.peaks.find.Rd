\name{msc.peaks.find}
\Rdversion{1.1}
\alias{msc.peaks.find}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Peaks of Mass Spectra
}
\description{
Find Peaks in a Batch of Protein Mass Spectra (SELDI) Data. 
Transcription of msc.peaks.find function of caMassClass

}
\usage{
msc.peaks.find(X, SNR = 2, span = c(81, 11), zerothresh = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Spectrum data either in matrix format [nFeatures x nSamples] or in 3D array format [nFeatures x nSamples x nCopies]. Row names (rownames(X)) store M/Z mass of each row
}
  \item{SNR}{
signal to noise ratio (z-score) criterion for peak detection. Similar to SoN variable in isPeak from PROcess package.
}
  \item{span}{
two moving window widths. Smaller one will be used for smoothing and local maxima finding. Larger one will be used for local variance estimation. Similar to span and sm.span variables in isPeak from PROcess package.
}
  \item{zerothresh}{
Intensity threshold criterion for peak detection. Positive numbers in range [0,1), like default 0.9, will be used to calculate a single threshold used for all samples using quantile(X,zerothresh) equation. Negative numbers in range (-1, 0) will be used to calculate threshold for each single sample i using quantile(X[i,],-zerothresh). Similar to zerothrsh variable in isPeak from PROcess package. 
}
}
\details{
Peak finding is done using the following algorithm:
x 	= 	X[j,]
thresh 	= 	if(zerothresh>=0) quantile(X,zerothresh) else quantile(x,-zerothresh)
sig 	= 	runmean(x, span[2])
rMax 	= 	runmax (x, span[2])
rAvr 	= 	runmed (x, span[1])
rStd 	= 	runmad (x, span[1], center=rAvr)
peak 	= 	(rMax == x) & (sig > thresh) & (sig-rAvr > SNR*rStd)

What means that a peak have to meet the following criteria to be classified as a peak:

    * be a local maxima in span[2] neighborhood
    * smoothed sample (sig) is above user defined threshold zerothresh
    * locally calculated z-score (see http://mathworld.wolfram.com/z-Score.html) of the signal is above user defined signal-to-noise ratio 

It is very similar to the isPeak and getPeaks functions from PROcess library (ver 1.3.2) written by Xiaochun Li. For example getPeaks(X, PeakFile, SoN=SNR, span=span[1], sm.span=span[2], zerothrsh=zerothresh, area.w=0.003, ratio=0) would give very similar results as msc.peaks.find the differences include: speed ( msc.peaks.find uses much faster C-level code), different use of signal-to-noise-ratio variable, and msc.peaks.find does not do or use area calculations. 
}
\value{A data frame, in the same format as data saved in peakinfofile, have five components: 

\item{Spectrum.Tag }{sample name of each peak}
\item{Spectrum. }{sample number of each peak}
\item{Intensity}{peak height (intensity)}
\item{Substance.Mass}{x-axis position, or corresponding mass of the peak measured in M/Z, which were extracted from row names of the X matrix.}


}
\author{
Jarek Tuszynski (SAIC) jaroslaw.w.tuszynski@saic.com
}
\references{\url{http://finzi.psych.upenn.edu/R/library/caMassClass/html/00Index.html}

}
\keyword{ interface }
