msc.peaks.clust <-
function (dM, S, BinSize = c(0, sum(dM)), tol = 0.97, verbose = FALSE) 
{
    repeats = function(x) return(sum(duplicated(x)))
    nS = length(S)
    if (nS != length(dM) & (nS - 1) != length(dM)) 
        stop("Error in msc.peaks.clust: vectors S and dM have to have the same length")
    if (length(BinSize) != 2) 
        BinSize = c(0, 1)
    if (BinSize[1] > BinSize[2]) {
        a = BinSize[1]
        BinSize[1] = BinSize[2]
        BinSize[2] = a
    }
    if (verbose) 
        print("Stack #Gaps [parent_bin ](bin_size) -> [Left_child ](#reps bin_size) + [right_child](#reps bin_size)  gap_chosen")
    mStack = max(20, as.integer(3 * log(nS)))
    nStack = 1
    Stack = matrix(0, 2, mStack)
    Stack[1, nStack] = 1
    Stack[2, nStack] = nS
    bin = numeric(nS)
    while (nStack > 0) {
        from = Stack[1, nStack]
        to = Stack[2, nStack]
        nStack = nStack - 1
        gap = dM[from:(to - 1)]
        size = sum(gap)
        mx = tol * max(gap)
        idx = which(gap >= mx)
        len = length(idx)
        if (len > 1) {
            score = numeric(len)
            for (j in 1:len) {
                Cut = idx[j] + from
                nL = repeats(S[from:(Cut - 1)])
                nR = repeats(S[Cut:to])
                score[j] = nL + nR
            }
            i = which(score == min(score))
            idx = idx[i]
            m = length(idx)
            if (m > 1) {
                k = to - from - 1
                if (idx[1] < k - idx[m]) 
                  idx = idx[1]
                else idx = idx[m]
            }
        }
        Cut = idx[1] + from
        if (from <= Cut - 2) 
            sL = sum(dM[from:(Cut - 2)])
        else sL = 0
        nL = -1
        if (sL > BinSize[2]) {
            nStack = nStack + 1
            Stack[, nStack] = c(from, Cut - 1)
            nL = -2
        }
        else if (sL > BinSize[1]) {
            nL = repeats(S[from:(Cut - 1)])
            if (nL > 0) {
                nStack = nStack + 1
                Stack[, nStack] = c(from, Cut - 1)
            }
        }
        else if (sL == 0) 
            nL = 0
        if (Cut <= to - 1) 
            sR = sum(dM[Cut:(to - 1)])
        else sR = 0
        nR = -1
        if (sR > BinSize[2]) {
            nStack = nStack + 1
            Stack[, nStack] = c(Cut, to)
            nR = -2
        }
        else if (sR > BinSize[1]) {
            nR = repeats(S[Cut:to])
            if (nR > 0) {
                nStack = nStack + 1
                Stack[, nStack] = c(Cut, to)
            }
        }
        else if (sR == 0) 
            nR = 0
        if (nStack == mStack) {
            mStack = trunc(1.5 * mStack)
            Stack = rbind(Stack, matrix(0, 2, mStack - nStack))
        }
        bin[Cut] = 1
        if (verbose) 
            print(sprintf("%5i %5i [%5i %5i](%8.5f) -> [%5i %5i](%5i, %7.4f) + [%5i %5i](%5i, %7.4f)  gap=%6.4f", 
                as.integer(nStack), as.integer(len), as.integer(from), 
                as.integer(to), size, as.integer(from), as.integer(Cut - 
                  1), as.integer(nL), sL, as.integer(Cut), as.integer(to), 
                as.integer(nR), sR, dM[Cut - 1]))
    }
    bin[1] = 1
    bin[nS] = 0
    return(bin)
}

