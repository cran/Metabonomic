msc.peaks.alignment <-
function (S, M, H, Tag = 0, SampFrac = 0.3, BinSize = c(0.002, 
    0.008), ...) 
{
    nPeak = length(M)
    if (nPeak != length(S) | nPeak != length(H)) 
        stop("msc.peaks.alignment: Unequal size arrays in msc.peaks.align")
    if (length(Tag) == nPeak) {
        mask = !duplicated(S)
        Tg = Tag[mask]
        x = sort(S[mask], index = TRUE)
        sNames = Tg[x[[2]]]
    }
    else sNames = 0
    x = sort(M, index = TRUE)
    M = x[[1]]
    idx = x[[2]]
    S = S[idx]
    H = H[idx]
    nPeak = length(M)
    M1 = c(M[2:nPeak], M[nPeak])
    dM = 2 * (M1 - M)/(M + M1)
    bin = msc.peaks.clust(dM, S, BinSize = BinSize, ...)
    Left = which(bin == 1)
    bin = cumsum(bin)
    nSamp = max(S)
    nFeat = length(Left)
    nul = -10000
    Bmrks = matrix(nul, nFeat, nSamp)
    for (j in 1:nPeak) Bmrks[bin[j], S[j]] = max(Bmrks[bin[j], 
        S[j]], H[j])
    Bmrks[Bmrks == nul] = NA
    BinBounds = c(Left, Left[2:nFeat] - 1, nPeak)
    BinBounds = M[BinBounds]
    dim(BinBounds) = c(nFeat, 2)
    colnames(BinBounds) = c("Left", "Right")
    bin = (BinBounds[, 1] + BinBounds[, 2])/2
    rownames(Bmrks) = paste("M", signif(bin, 6), sep = "")
    if (length(sNames) == ncol(Bmrks)) 
        colnames(Bmrks) = sNames
    if (SampFrac > 0) {
        numCol = apply(!is.na(Bmrks), 1, sum)
        keep = (numCol > SampFrac * ncol(Bmrks))
        Bmrks = Bmrks[keep, ]
        BinBounds = BinBounds[keep, ]
    }
    return(list(Bmrks = Bmrks, BinBounds = BinBounds))
}

