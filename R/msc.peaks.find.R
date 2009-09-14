msc.peaks.find <-
function (X, SNR = 2, span = c(81, 11), zerothresh = 0.9) 
{
    require(caTools)
    if (abs(zerothresh) > 1) 
        stop("Zerothresh (", zerothresh, ") smaller than minus one or larger than one is incorrect - using default ", 
            zerothresh = 0.9)
    GlobalThresh = (zerothresh >= 0 && zerothresh < 1)
    if (GlobalThresh) 
        zerothresh = quantile(X, zerothresh)
    span = span + 1 - span%%2
    if (span[1] < span[2]) {
        tmp = span[1]
        span[1] = span[2]
        span[2] = tmp
    }
    mass = as.numeric(rownames(X))
    SampleNames = colnames(X)
    d = dim(X)
    if (length(d) == 3) {
        nCopy = d[3]
        cNames = NULL
        for (iCopy in 1:nCopy) cNames = c(cNames, paste(SampleNames, 
            "(", as.character(iCopy), ")", sep = ""))
        dim(X) = c(d[1], prod(d)/d[1])
        SampleNames = cNames
    }
    nSamp = ncol(X)
    cnts = numeric(nSamp)
    pNumb = NULL
    pIntn = NULL
    pMass = NULL
    for (j in 1:nSamp) {
        x = X[, j]
        thresh = if (GlobalThresh) 
            zerothresh
        else quantile(x, -zerothresh)
        sig = runmean(x, span[2])
        rMax = runmax(x, span[2])
        rAvr = runmed(x, span[1])
        rStd = runmad(x, span[1], center = rAvr)
        peak = (rMax == x) & (sig > thresh) & (sig - rAvr > SNR * 
            rStd)
        idx = which(peak)
        cnts[j] = length(idx)
        if (cnts[j] > 0) {
            pNumb = c(pNumb, 1:cnts[j])
            pIntn = c(pIntn, rAvr[idx])
            pMass = c(pMass, mass[idx])
        }
    }
    sName = rep(SampleNames, cnts)
    sNumb = rep(1:nSamp, cnts)
    Data = data.frame(sName, sNumb, pNumb, pIntn, pMass)
    colnames(Data) = c("Spectrum.Tag", "Spectrum.", "Peak.", 
        "Intensity", "Substance.Mass")
    return(Data)
}

