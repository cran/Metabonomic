msc.peaks.align <-
function (Peaks, SampFrac = 0.3, BinSize = c(0.002, 0.008), ...) 
{
    if (is.character(Peaks)) 
        Peaks = msc.peaks.read.csv(Peaks)
    out = msc.peaks.alignment(Peaks$Spectrum., Peaks$Substance.Mass, 
        Peaks$Intensity, Tag = Peaks$Spectrum.Tag, BinSize = BinSize, 
        SampFrac = SampFrac, ...)
    return(out)
}

