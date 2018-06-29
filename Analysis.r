#row.names(mt) <- mt$rn;
##t$rn <- NULL;
#mt <- data.matrix(mt);
ct = fread("tial19.csv", data.table=F, select=1:4);
row.names(ct) = ct$rn;
ct$rn = NULL
ct = data.matrix(ct)
#find best IBS/cluster method... this is the ibs.validity.R file.
dissimilarity.metrics = c("binary", "euclidean", "manhattan"); names(dissimilarity.metrics) = dissimilarity.metrics;
clustering.methods = c("ward", "complete", "single", "average"); names(clustering.methods) = clustering.methods;
v.clusters = c();
a.clusters = list();
for (i in dissimilarity.metrics)(
        for (j in clustering.methods) {
            a.clusters[[i]][[j]] = agnes(ct, metric=i, method=j, stand=T);
            tcm = agnes(ct, metric=i, method=j, stand=T)$ac;
            names(tcm) = paste(i, j, sep=".");
            v.clusters = c(v.clusters, tcm)})
print(paste("Method with highest agglomerative coefficient is",  v.clusters[order(v.clusters)][length(v.clusters)], sep=" "));
#can and should write a script to automatically use that method... someghing like, >agnes(meothod = {prior_to(".") in >names(max(v.clusters))}, metric = after(".") in >names(max(v.clusters))
#will use metric="binary" and method="ward" for now
#should also like to work out how to call hc = a.clusters$binary$ward
hc = agnes(ct, metric="binary", method="ward", stand=T);
plot(hc, xax.pretty=T, main="Dendogram of binary/Ward clustering", which.plots=2);
###Level = list();
###for (i in 1:8) (Level[[i]] = cutree(hc, k = 2^i));
###pltree(hc, cex = 0.6, hang = -1, main = "Dendrogram of agnes"); 
#should use"binary" for the distance measurement and "ward.D" for the clustering method... but makes the dendogram unreadable. "euclidean" for the distance measurement and "complete" for the clustering method made a nicer dendogram. also using a 20-cell sample helped.
#d = dist(ct, method="euclidean");
#cl1 = hclust(d, method="ward.D");
#cm = agnes(ct, metric="binary", method="ward", stand=TRUE, diss=FALSE)
###***** by clustering cells based on tSNE data, we won't get reproducable clusters, because tSNE coordinates are different every time.
###***** also, a drawn out line of cell clusters progressing through differentiation is probably only indicative that the most significant PCAs are defining mitotic vs G phase cells, as IBS will naturally draw cluster periodically through a line of points, and tSNE naturally creates lines of points based on larger differences through its dimensionality reduction..
#pltree(cm, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
#for (i in 1:2)
#ibs1 = cutree(cl1, k=2)
#mt = mt %>% mutate(IBS1 = ibs1)
