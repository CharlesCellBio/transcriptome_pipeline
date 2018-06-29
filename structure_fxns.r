mcorner = function(X) {
    X[1:5, 1:5]
}

sample.path = function(X) {
    paste0("my.", X, ".matrix.tsv")
}

get.sample = function(path, dummy.data=TRUE) {
    if (dummy.data)
    fread(path, check.names=TRUE, select=1:100)
    else (fread(path, check.names=TRUE))
}

read.as.formatted = function(X, ...) {
    sample.path(X) %>%
    get.sample(...) %>%
    format.sample(sample.name=X)
}

vector.to.index = function(X){
        width = nchar(length(X));
        values = seq_along(X);
        syntax = paste0("%0", width, "d");
        paste0("C", sprintf(syntax, values))
}

format.sample = function(X, sample.name="no.sample.name") {
    barcodes = copy(names(X))[-1]
    gene.index = copy(X$Gene)
    index = vector.to.index(barcodes)
    X = data.table(Index=index, Barcode=barcodes, Sample=sample.name, transpose(X[, -"Gene"]))
    setnames(X, c("Index", "Barcode", "Sample", c(gene.index)))
    setkey(X, Index)
    X
}

select.from.sample = function(X) {
    rs = rowSums(X[, c(gene.index)]);
    index = as.logical(rs < quantile(rs, probs=0.85) & rs > quantile(rs, probs=0.1));
    X = X[index]
    X
}


merge.data = function(X) {
    a = rbindlist(X, use.names=TRUE, fill=TRUE, idcol=FALSE);   
    for (j in seq_len(ncol(a))) (
        set(a,which(is.na(a[[j]])),j,0)
    )
    b = vector.to.index(a$Index)
    a[, Index:=b][]
}

normalize.WS.data.fxn1 = function(X) {
    a = X[, c(gene.index)];
    factor = median(rowSums(a))/rowSums(a);
    log2(1 + sweep(a, 1, factor, "*"))
    a = a[, colSums(a)!=0]
}


