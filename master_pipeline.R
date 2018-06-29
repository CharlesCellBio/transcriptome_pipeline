my_pipeline = function(X) {
    X %>%
    data.collection %>%
    data.processing %>%
    data.analysis %>%
    data.relevant.output
    
    data.collection = function(x.name,
    sample.path = NULL, gather.sample=NULL, format.sample=NULL, select.from.sample=NULL, merge.format=NULL, normalize.WS.data=NULL){

    master.structure = function(y.name) {
            lapply(y.name, function(z.name) {
                z.name %>%
                sample.path %>%
                gather.sample %>%
                format.sample(z.name) %>%
                select.from.sample
            }) %>% 
                merge.format %>%
                normalize.WS.data
    }

    if (sample.path == NULL)
        sample.path = function(X) {
            paste0("my.", X, ".matrix.tsv")
            }

    if (gather.sample == NULL)
        gather.sample = function(path, dummy.data=TRUE) {
            if (dummy.data)
                fread(path, check.names=TRUE, select=1:100)
            else (fread(path, check.names=TRUE))
            }

    if (format.sample == NULL)
        format.sample = function(X, sample.name="no.sample.name") {
            barcodes = copy(names(X))[-1]
            gene.index = copy(X$Gene)
            index = vector.to.index(barcodes)
            X = data.table(Index=index, Barcode=barcodes, Sample=sample.name, transpose(X[, -"Gene"]))
            setnames(X, c("Index", "Barcode", "Sample", c(gene.index)))
            setkey(X, Index)
            X
            }

    if (select.from.sample == NULL) {
        select.from.sample = function(X) {
        rs = rowSums(X[, c(gene.index)]);
        index = as.logical(rs < quantile(rs, probs=0.85) & rs > quantile(rs, probs=0.1));
        X = X[index]
        X
        }
    }
    
    if (merge.data == NULL)
        merge.data = function(X) {
        a = rbindlist(X, use.names=TRUE, fill=TRUE, idcol=FALSE);   
        for (j in seq_len(ncol(a))) (
            set(a,which(is.na(a[[j]])),j,0)
        )
        b = vector.to.index(a$Index)
        a[, Index:=b][]
        }

    if (normalize.data == NULL)
        normalize.WS.data.fxn1 = function(X) {
        a = X[, c(gene.index)];
        factor = median(rowSums(a))/rowSums(a);
        log2(1 + sweep(a, 1, factor, "*"))
        a = a[, colSums(a)!=0]
        }
    }

    master.structure(x.name)
}

data.processing = function(raw_data) {
    master.structure = function(X) {
        raw_data %>%
        PCA %>%
        tSNE %>%
        cluster %>%
        IBS
    }

    PCA = function(X) {
        prcomp(TS19, center=T, scale=T)
    }
    
    return master.structure(raw_data)
}

#    lapply(x.name, function(y.name) {
#        y.name %>%
#        sample.path %>%
#        gather.sample %>%
#        format.sample(y.name) %>%
#        select.from.sample
#    }) %>% 
#        merge.format %>%
#        normalize.WS.data
#}

   
}





pcaDT <- prcomp(TS19, center=T, scale=T);
# top 100PCs, as [barcode, PCs]
keyPCAs <- pcaDT[["x"]][,1:100];
#use top 100 PCAs, in standard [pca, barcode]
#NOTE: original pipeline was a little confusing at this point, but after seeing tSNE output, the following way makes the most sense.
#tSNE
######works faster when using a matrix as keyPCAs than it does using keyPCAs as a data.table
tsneDT<- Rtsne(keyPCAs, dims = 3, theta=0.0, check_duplicates = FALSE, pca = FALSE);
#make master list of every cell's: x,y,z dimensions, and PCA values of the top 100PCAs.
master.table <- cbind(tsneDT$Y, keyPCAs);
colnames(master.table)[1:3]<- c("x.tsne", "y.tsne", "z.tsne");