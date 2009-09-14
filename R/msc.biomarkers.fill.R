msc.biomarkers.fill <-
function (X, Bmrks, BinBounds, FillType = 0.9) 
{
    d = dim(X)
    dNames = dimnames(X)
    nFeat = nrow(Bmrks)
    if (is.array(X) & FillType >= 0) {
        dim(X) = c(d[1], prod(d)/d[1])
        if (ncol(Bmrks) != ncol(X)) 
            stop("msc.biomarkers.fill: Unequal number of columns/samples in X(", 
                ncol(Bmrks), ") and Bmrks arrays(", ncol(X), 
                ")")
        mass = as.numeric(dNames[[1]])
        for (i in 1:nFeat) {
            cidx = which(is.na(Bmrks[i, ]))
            n = length(cidx)
            if (n == 0) 
                next
            if (FillType >= 0 & FillType <= 1) {
                ridx = which(mass >= BinBounds[i, 1] & mass <= 
                  BinBounds[i, 2])
                if (length(ridx) > 1) {
                  if (n == 1) 
                    Bmrks[i, cidx[1]] = quantile(X[ridx, cidx[1]], 
                      probs = FillType)
                  else Bmrks[i, cidx] = apply(X[ridx, cidx], 
                    2, quantile, probs = FillType)
                }
                else {
                  if (n == 1) 
                    Bmrks[i, cidx[1]] = X[ridx[1], cidx[1]]
                  else Bmrks[i, cidx] = X[ridx[1], cidx]
                }
            }
            else if (FillType == 2) {
                dis = abs(mass - (BinBounds[i, 1] + BinBounds[i, 
                  2])/2)
                ridx = which.min(dis)
                Bmrks[i, cidx] = X[ridx, cidx]
            }
            else if (FillType == 3) {
                A[is.na(A)] = 0
            }
        }
        dim(X) = d
    }
    if (length(d) == 3 && d[3] > 1) {
        mass = rownames(Bmrks)
        dim(Bmrks) = c(nrow(Bmrks), d[2], d[3])
        dimnames(Bmrks) = list(mass, dNames[[2]], dNames[[3]])
    }
    else colnames(Bmrks) = dNames[[2]]
    return(Bmrks)
}

