master.fxn = function(DT, nlvls=3, data.cols=c("x.tSNE", "y.tSNE", "z.tSNE")) {
    tname = function(X) (paste("IBS", X, 1:nlvls, sep="."))
    Lvls = tname("Lvl")
    Grps = tname("Grp")
    Grps.Cfs = tname("AC")
    for (i in 1:nlvls) {
            DT[, c(Lvls[i]):=cluster.fxn(.SD, return.k=TRUE), .SDcols=data.cols, by=c(Lvls[0:(i-1)])]
            DT[, c(Grps[i]):=.GRP, by=c(Lvls[1:i])]
            DT[, c(Grps.Cfs[i]):=cluster.fxn(.SD, return.ac=TRUE), .SDcols=data.cols, by=c(Lvls[0:(i-1)])]
    }
}

cluster.fxn = function(X, return.ac=FALSE, return.k=FALSE) {
    a = agnes(X, metric="euclidean", method="ward", stand=TRUE)
    if (return.ac) return(a$ac)
    if (return.k) return(cutree(a, k=2))
}