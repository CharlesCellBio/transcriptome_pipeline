library(tsne, data.table, magrittr, clusterone...)

my_pipeline = function(x.name) {

    my_pipeline_structure = function(X){
	    X %>%
	    data.collection %>%
	    data.processing %>%
	    data.analysis %>%
	    data.relevant.output
    }

    data.collection = function(x.name,
    sample.path = NULL, gather.sample=NULL, format.sample=NULL, select.from.sample=NULL, merge.format=NULL, normalize.WS.data=NULL){

	    data.collection.structure = function(y.name) {
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

        return(data.collection.structure(x.name))
    }

    data.processing = function(raw_data) {
        data.processing.structure = function(X) {
            raw_data %>%
            PCA %>%
            tSNE %>%
            IBS.cluster
        }

        PCA = function(X) {
            prcomp(TS19, center=T, scale=T)[["x"]][,1:100]
        }
    
        tSNE = function(X){
            Rtsne(X, dims = 3, theta=0.0, check_duplicates = FALSE, pca = FALSE)
        }
    
        IBS.cluster = function(DT, nlvls=3, data.cols=c("x.tSNE", "y.tSNE", "z.tSNE")) {

            cluster_best_method = function(X){
                dissimilarity.metrics = c("binary", "euclidean", "manhattan"); names(dissimilarity.metrics) = dissimilarity.metrics;
                clustering.methods = c("ward", "complete", "single", "average"); names(clustering.methods) = clustering.methods;
                v.clusters = c();
                a.clusters = list();
                for (i in dissimilarity.metrics)(
                    for (j in clustering.methods) {
                        a.clusters[[i]][[j]] = agnes(ct, metric=i, method=j, stand=T);
                        tcm = agnes(ct, metric=i, method=j, stand=T)$ac;
                        names(tcm) = paste(i, j, sep=".");
                        v.clusters = c(v.clusters, tcm);})
            }

            cluster.fxn = function(DT, return.ac=FALSE, return.k=FALSE) {
                a = agnes(X, metric="euclidean", method="ward", stand=TRUE)
                if (return.ac) return(a$ac)
                if (return.k) return(cutree(a, k=2))
            }

            IBS = function(DT){
                tname = function(DT) (paste("IBS", X, 1:nlvls, sep="."))
                Lvls = tname("Lvl")
                Grps = tname("Grp")
                Grps.Cfs = tname("AC")
                for (i in 1:nlvls) {
                    DT[, c(Lvls[i]):=cluster.fxn(.SD, return.k=TRUE), .SDcols=data.cols, by=c(Lvls[0:(i-1)])]
                    DT[, c(Grps[i]):=.GRP, by=c(Lvls[1:i])]
                    DT[, c(Grps.Cfs[i]):=cluster.fxn(.SD, return.ac=TRUE), .SDcols=data.cols, by=c(Lvls[0:(i-1)])]
                }
            }

            return(IBS(X))
        }

        return(data.processing.structure(raw_data))
    }

    return(my_pipeline_structure(x.name))

}
