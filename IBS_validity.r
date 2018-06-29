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
