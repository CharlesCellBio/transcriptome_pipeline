#This script aims to implement "functional programing" style.
#
#The majority of the code is focused on wrapping the analyses we would like to perform into neatly packaged functions that we can run later at will.
#The script that is "run" is thus greatly condensed, organized and interchangeable.
#
#New functions can be easily built and swapped in over time to modify particular aspects of the analysis, and the result will have always have identical/comparable outputs.
#This should save time in 3 ways.
###Instead of writing entirely new scripts, new functions can be built and easily inserted, and output will always be identical/comparable.
###Collaboration is simplified. So long as the new function is built to input and output data in the same format, all code should run seamlessly.
###Problems in the script can be easily isolated and fixed, as errors can be narrowed down to the particular function of interest, and modified. So long as the input and output format remain the same, the rest of the script can remain untouched.
### Readability and organization is greatly increased, as the Master functions can be written in the most intuitive way, and Slave functions can encompass all non-easily interpreted code.
#
#
#This scipt/model can be broken down into 3 components: Master functions, slave functions, and the "Run" script.
#intuitive
#Slave functions are the functional components of the script. They are the particular operations we would like to use in analyzing the data, wrapped neatly into functions. Names should be brief and intuitive. Ideally, the name of the Slave function should be similar to the argument within the Master function that it will be used for.
#Master functions are groupings of the slave functions. The master functions each represent a fundamental part of any pipeline. Readability is paramount in their construction, using basic operations on intuitively named arguments. The arguments are the Slave functions. In this way, a Master function can be run, while specifying which slave functions are utilized at each part. All messy code should be wrapped in the slave functions.
#The "Run" script is the implementation of the functions that we have created. It is a brief and simple run of the Master functions. This is the only section of the script that when run through the IDE will cause data to be accessed or processed. This section can be left out and run separately, so that we can specify any specific arguments (ie Slave functions) that we would like to use for the particular run as we work.
#







###Master functions###
collect.organize4 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn1, format.sample=format.sample.fxn4, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(y.name) {
        sample.names = y.name;
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}

collect.organize4.2 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn1, format.sample=format.sample.fxn4, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(y.name) {
        sample.names = y.name;
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}


collect.organize4.3 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn1, format.sample=format.sample.fxn4, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(y.name) {
        sample.names = y.name;
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}


big.collect.organize4 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn2, format.sample=format.sample.fxn4, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(y.name) {
        sample.names = y.name;
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}


collect.organize5 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn1, format.sample=format.sample.fxn3, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(sample.names) {
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}

big.collect.organize5 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn2, format.sample=format.sample.fxn3, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(sample.names) {
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}

collect.organize5 = function(x.name,
    sample.paths=sample.path.fxn1, gather.sample=gather.sample.fxn1, format.sample=format.sample.fxn4, select.from.sample=select.from.sample.fxn1, merge.format=merge.format.fxn1, normalize.WS.data=normalize.WS.data.fxn1)    
{
    lapply(x.name, function(sample.names) {
        sample.names %>%
        sample.paths %>%
        gather.sample %>%
        format.sample(sample.names) %>%
        select.from.sample}) %>% 
        merge.format %>%
        normalize.WS.data
}

analyze.data2 = function(dt, 
    format.for.analysis=format.for.analysis.fxn1, PCA=PCA.fxn1, tSNE=tSNE.fxn1)
{
    format.for.analysis(dt) %>%
    PCA %>%
    tSNE
}

cluster.cells1 = function(dt,
    grouping=IBS.fxn4, clustering=clustering.fxn2)
{
    dt %>%
    clustering %>%
    grouping
}

cluster.cells2 = function(X,
    cluster=clustering.fxn2, assign.cluster=IBS.fxn1)
{
    X %>%
    assign.cluster
}

cluster.cells3 = function(X){
    X %>%
    clustering.fxn2 %>%
    IBS.fxn1
}

cluster.cells4 = function(dt,
    grouping=IBS.fxn4, clustering=clustering.fxn2)
{
    dt %>%
    clustering %>%
    grouping
}













###Slave functions###

#Initial workspace functions
ws.samples = function(X) {
    readline(prompt("What are the sample names, as they are differentiated in their file names?: "))
}

ws.packages.efficient = function(X) {
update.packaes();
rp = c("cluster", "data.table", "magrittr", "tsne");
ip = names(installed.packages()[, "Package"]);
install.packages(rp
    [rp %in% ip]
    , dependencies=TRUE);
library(rp);
}

ws.packages.safe = function(X) {
    rp = c("cluster", "data.table", "magrittr", "tsne");
    update.packages();
    install.packages(rp);
    library(rp);
}

mcorner = function(dt) (
    dt[1:5, 1:5]
)


#Collection functions
sample.path.fxn1 = function(X) {
    paste0("my.", X, ".matrix.tsv")
}

gather.sample.fxn1 = function(X) {
    fread(X, check.names=T, select=1:100)
}

gather.sample.fxn2 = function(X) {
    fread(X, check.names=T)
}

format.sample.fxn1 = function(X, sample.names=sample.names) {
        width = nchar(ncol(X));
        values = 1:(ncol(X)-1);
        syntax = paste0("%0", width, "d");
        cell.index = sprintf(syntax, values);
        cell.names=paste0("C", cell.index, ".", sample.names);
    setkey(setnames(data.table(Cells=cell.names, Sample=sample.names, transpose(X[,-1])), c("Cells", X$Gene)))
}

format.sample.fxn2 = function(X) {
        width = nchar(ncol(X));
        values = 1:(ncol(X)-1);
        syntax = paste0("%0", width, "d");
        cell.index = sprintf(syntax, values);
        cell.names=paste0("C", cell.index);
    setkey(setnames(data.table(Cells=cell.names, transpose(X[,-1])), c("Cells", X$Gene)))
}

format.sample.fxn3 = function(X, z.name) {
        width = nchar(ncol(X));
        values = 1:(ncol(X)-1);
        syntax = paste0("%0", width, "d");
        cell.index = sprintf(syntax, values);
        cell.names=paste0("C", cell.index, ".", z.name);
    setkey(setnames(data.table(Cells=cell.names, transpose(X[,-1])), c("Cells", X$Gene)))
}

format.sample.fxn4 = function(X) {
        genes = X$Gene
        width = nchar(ncol(X));
        values = 1:(ncol(X)-1);
        syntax = paste0("%0", width, "d");
        cell.index = sprintf(syntax, values);
        cell.names=paste0("C", cell.index, ".", sample.names);
        data.table(Cells=cell.names, Sample=sample.names, key=Cells)[, X$Gene:=transpose(X[,-1])][]
}

 #data.table(matrix(1:100, 10))[, Cells:=letters[1:10]][]

select.from.sample.fxn1 = function(dt) {
    rs = rowSums(dt[, -c(1:2)]);
    index = as.logical(rs < quantile(rs, probs=0.85) & rs > quantile(rs, probs=0.1));
    data.table(dt[index])
}

merge.format.fxn1 = function(X) {
    a = rbindlist(X, use.names=TRUE, fill=TRUE, idcol=FALSE);
    a[is.na(a)] = 0;
    setkey(a, Cells);
    a
}

merge.fxn1 = function(X) {
    a = setkey(rbindlist(X, use.names=TRUE, fill=TRUE, idcol=FALSE), Cells);
    a[is.na(a)] = 0;
    a
}

format.fxn2 = function(DT) {
    for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}


normalize.WS.data.fxn1 = function(X) {
    a = X[,-c(1:2)];
    factor = median(rowSums(a))/rowSums(a);
    b = log2(1 + sweep(a, 1, factor, "*"));
    c = b[, colSums(b)!=0]
    d = data.table(Cells=X$Cells, c, key="Cells");
#    row.names(d) = d[["Cells"]]
}


#Analysis functions
format.for.analysis.fxn1 = function(dt) {
    #data.table input, matrix output
    a =copy(dt[, -1]) %>%
    data.matrix;
    row.names(a) = copy(dt[["Cells"]]);
    a
}



#format.for.analysis.fxn3 = function(DT) {
    #data.table input, matrix output
#    a = DT %>%
#    copy %>%
#    row.
#    row.names(a) = a$Cells;
#    as.data.frame(a)
#    a = data.matrix(copy(DT[,-1]));
#    row.names(a) = names;
#    a
#    names = copy(a$Cells;
#    a = data.matrix(DT[, -1]);
#    row.names(a) = copy(dt[["Cells"]]);
#    a
#}

PCA.fxn1 = function(dm) {
    prcomp(dm, center=T, scale=T)[["x"]][,1:100]
}

PCA.fxn2 = function(dm) {
    a = prcomp(dm, center=T, scale=T);
    screeplot(a, type="lines",col=length(a$sdev));
    a[["x"]][,1:100]
}

PCA.fxn3 = function(DT) {
    data.table(Cells=DT$Cells, prcomp(DT[,-1], center=T, scale=T)[["x"]][,1:100], key=c("Cells"));
}

PCA.fxn4 = function(DT) {
    row.names(DT) = DT$Cells;

}

tSNE.fxn1 = function(DT) {
    tsne(dm, k = 3) %>%
    data.table %>%
    setnames(c("x.tSNE", "y.tSNE", "z.tSNE"))
}


#Cluster functionss
clustering.fxn1 = function(dt, metric="euclidean", method="ward", k=2) {
    agnes(copy(dt), metric=metric, method=method, stand=TRUE) %>%
    cutree( k=k)
}

clustering.fxn2 = function(dt) {
    dt %>%
    copy %>%
    agnes(metric="euclidean", method="ward", stand=TRUE) %>%
    cutree( k=2)
}

clustering.fxn3 = function(dt) {
    agnes(copy(dt), metric="euclidean", method="ward", stand=TRUE) %>%
    cutree(k=2)
}

clustering.fxn4 = function(X) {
    cutree(agnes(X, metric="euclidean", method="ward", stand=TRUE), k=2)
}

#another possible way to accomplish the pass instead of the return() function is assign(x, y, envir=sys.frame(which=-1)) or assign(x, y, envir=.GlobalEnv)
clustering.fxn5 = function(X, find.k=FALSE, find.coefficient=FALSE) {
    a = agnes(X, metric="euclidean", method="ward", stand=TRUE)
    if (find.k) return(a$ac)
    if (find.coefficient) return(cutree(a, k=2))
}

possible.pass.fxn = function(X, ...) {
    std.args = list(metric="euclidean", method="ward", stand=TRUE)
    args.to.pass = list(...) %in% list(metric="euclidean", method="ward", stand=TRUE)
    fin.arg.list = list(..., std.args[args.to.pass]) 
    fin.arg.list
    #possible assign(X) needed
}




IBS.fxn3 = function(X, nlevels=8, clustering=clustering.fxn2) {
    a = setkey(data.table(Cells=TS19$data$Cells, X));
    Lvl = paste("IBS Lvl", 1:nlevels, sep=" ");
    for (i in 1:nlevels) {
        if (i ==1) (
            a[, Lvl[i]:=clustering(.SD), .SDcols=c("x.tSNE", "y.tSNE", "z.tSNE")])
        else (
            a[, Lvl[i]:=clustering.fxn2(.SD), .SDcols=c("x.tSNE", "y.tSNE", "z.tSNE")][, by=Lvl[1:(i-1)]])
    };
    a
}

IBS.fxn4 = function(X, nlevels=8, clustering=clustering.fxn2) {
    a = copy(X$Cells);
    Lvl = paste("IBS Lvl", 1:nlevels, sep=" ");
    for (i in 1:nlevels) {
        if (i == 1) (
            a[, Lvl[i]:=clustering(X[, c("x.tSNE", "y.tSNE", "z.tSNE")])])
        else (
            a[, Lvl[i]:=clustering.fxn2(X[, c("x.tSNE", "y.tSNE", "z.tSNE"), by=Lvl[1:(i-1)]])])
    };
    a
}

IBS.fxn5 = function(X, cluster.fxn=clustering.fxn1, nlvls=8) {
    Lvls = paste0("Lvl", 1:nlvls);
    a = setkey(copy(X));
    for (i in 1:nlvls) {
        if (i == 1) (
            a[, Lvls[i]:=cluster.fxn(x.tSNE, y.tSNE, z.tSNE)]
        )
        else (
            a[, Lvls[i]:=cluster.fxn(x.tSNE, y.tSNE, z.tSNE), by=c(Lvls[1:(i-1)])]
        )
    }
    a
}

IBS.fxn6 = function(X, nlvls=8) {
    Lvls = paste0("Lvl", 1:nlvls);
    a = copy(X);
    for (i in 1:nlvls) {
        if (i == 1) (
            a[, Lvls[i]:=clustering.fxn3(.(x.tSNE, y.tSNE, z.tSNE))]
        )
        else (
            a[, Lvls[i]:=clustering.fxn3(.(x.tSNE, y.tSNE, z.tSNE)), by=c(Lvls[1:(i-1)])]
        )
    }
    a
}

IBS.fxn7 = function(X, nlvls=8) {
    Lvls = paste0("Lvl", 1:nlvls);
    a = copy(X);
    for (i in 1:nlvls) {
        if (i == 1) (
            a[, Lvls[i]:=clustering.fxn3(.SD), .SDcols=list(x.tSNE, y.tSNE, z.tSNE)]
        )
        else (
            a[, Lvls[i]:=clustering.fxn3(.SD), .SDcols=list(x.tSNE, y.tSNE, z.tSNE), by=c(Lvls[1:(i-1)])]
        )
    }
    a
}

IBS.fxn8 = function(X) {
    Lvls = paste0("Lvl", 1:8);
    a = copy(X);
    for (i in 1:8) {
        if (i == 1) (
            a[, Lvls[i]:=cutree(agnes(.SD, metric="euclidean", method="ward", stand=TRUE), k=2), .SDcols=list(x.tSNE, y.tSNE, z.tSNE)]
        )
        else (
            a[, Lvls[i]:=cutree(agnes(.SD, metric="euclidean", method="ward", stand=TRUE), k=2), .SDcols=list(x.tSNE, y.tSNE, z.tSNE), by=(Lvls[1:(i-1)])]
        )
    }
    a
}





#clustering.fxn(data, method, metric, k)
#IBS.fxn(data, clustering.fxn, nlevels)



#DT[, grp := .GRP, by=x]



#b[, Lvl3:=a[i, cutree(agnes(.SD[ .(xyz), on=grp2]))]]

#######setkey() on all TS19 data.tables immediately. Also go in and name the PCA and make sure its sorted, as well as the tSNE output.




###Suggested_Script###
#workspace/precomputational
## didnt work...
##  lnames = readline(prompt("What are the sample names?: "));
##  print2 = readline(prompt("Save the results as .csv files? (TRUE/FALSE): "));
#lnames=c("cardiac", "head", "trunk", "we");
#print2 = FALSE;
#process data
#TS19 = list();
#TS19$data = collect.organize4(lnames);
#TS19$analysis = analyze.data2(TS19$data);
#TS19$cluster = cluster.cells1(TS19$analysis);
#optional, print components
#if (print2 == TRUE) (lapply(TS19, 
#    (function(X) fwrite(X, file=paste0("TS19.", X, ".csv")))
#    ));
#
#
#possible?
#a[, Lvl1:= agnes(x,y,z)]
#a[, Lvl2:=agnes(.xyz), by=Lvl1]
#a[, Grp2:=.GRP, by=.(Lvl1, Lvl2)]
#a[, Lvl3:=agnes(.xyz), by=Grp2]
#a[, Grp3:=.GRP, by=.(Lvl3, Grp2)]
#a[, Lvl4:=agnes(.xyz), by=Grp3]
#a[, Grp4:=.GRP, by=.(Lvl4, Grp3)]
#
#
#
#
#
#unkeyed columns... note the fact that its not in .() as a keyed column would be.
#subassing an unkeyed column.
# subassign by reference
#flights[hour == 24L, hour := 0L]
#
#unkeyed columns cont.
#delete an unkeyed column by reference
#flights[, c("delay") := NULL]
#or we can skip the BS... when there is only one column...
#flights[, delay := NULL]
#
#
#setkey(flights, origin)
#head(flights)
#    year month day dep_delay arr_delay carrier origin dest air_time distance hour
# 1: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18
# 2: 2014     1   1        -5       -17      AA    EWR  MIA      161     1085   16
# 3: 2014     1   1       191       185      AA    EWR  DFW      214     1372   16
# 4: 2014     1   1        -1        -2      AA    EWR  DFW      214     1372   14
# 5: 2014     1   1        -3       -10      AA    EWR  MIA      154     1085    6
# 6: 2014     1   1         4       -17      AA    EWR  DFW      215     1372    9

## alternatively we can provide character vectors to the function 'setkeyv()'
#********** setkeyv(flights, "origin") # useful to program with

#Once you key a data.table by certain columns, you can subset by querying those key columns using the .() notation in i. Recall that .() is an alias to list().

#flights[.("JFK")].... when referencing a row index on a keyed column, we can use .()... 

#On single column key of character type, you can drop the .() notation and use the values directly when subsetting, like subset using row names on data.frames.
#flights["JFK"]              ## same as flights[.("JFK")]

#We can subset any amount of values as required
#flights[c("JFK", "LGA")]    ## same as flights[.(c("JFK", "LGA"))]

#Subset all rows using key columns where first key column origin matches “JFK” and second key column dest matches “MIA”
#flights[.("JFK", "MIA")]

#Subset all rows where just the second key column dest matches “MIA”
#flights[.(unique(origin), "MIA")]
#The value provided for the second key column “MIA” has to find the matching values in dest key column on the matching rows provided by the first key column origin. We can not skip the values of key columns before. Therefore we provide all unique values from key column origin.

#How can I set keys on both origin and dest columns?
#setkey(flights, origin, dest)
## or alternatively
# setkeyv(flights, c("origin", "dest")) # provide a character vector of column names
#***notice the "v" at the end of setkeyv!!!

#On the result obtained above, use chaining to order the column in decreasing order.
#flights[.("LGA", "TPA"), .(arr_delay)][order(-arr_delay)]
#... in other words, the DT[][] works on the RESULT of the first one.

#replace values within a keyed column
#head(flights)
#    year month day dep_delay arr_delay carrier origin dest air_time distance hour
# 1: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9
# 2: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11
# 3: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19
# 4: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7
# 5: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13
# 6: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18
#
#flights[, sort(unique(hour))]
#  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#
#We see that there are totally 25 unique values in the data. Both 0 and 24 hours seem to be present. Let's go ahead and replace 24 with 0, but this time using key.
#
#setkey(flights, hour)
#key(flights)
# [1] "hour"
#flights[.(24), hour := 0L]
#key(flights)
# NULL
#modifying values in a key column removes the key from that column
#
#using the keyby= argumnet within DT["a"..., keyby=b] will result in the product having its key switched to keyby=, and it will be ordered. We can USE the original keys, within the function, but the output will have the new key.
#Get the maximum departure delay for each month corresponding to origin = "JFK". Order the result by month
#setkey(flights, origin, dest)
#key(flights)
# [1] "origin" "dest"
#ans <- flights["JFK", max(dep_delay), keyby = month]
#head(ans)
#    month   V1
# 1:     1  881
# 2:     2 1014
# 3:     3  920
# 4:     4 1241
# 5:     5  853
# 6:     6  798
#key(ans)
# [1] "month"
#
#subset combinations of keyed row groups
#Subset only the last matching row of all the rows where origin matches “LGA”, “JFK”, “EWR” and dest matches “XNA”
#flights[.(c("LGA", "JFK", "EWR"), "XNA"), mult = "last"]
#    year month day dep_delay arr_delay carrier origin dest air_time distance hour
# 1: 2014     5  23       163       148      MQ    LGA  XNA      158     1147   18
# 2:   NA    NA  NA        NA        NA      NA    JFK  XNA       NA       NA   NA
# 3: 2014     2   3       231       268      EV    EWR  XNA      184     1131   12
#The query “JFK”, “XNA” doesn't match any rows in flights and therefore returns NA.
#
#this is why a keyed DT[.("A", "B")] syntax is stupid fast (also DT["A"] I believe...)
#Here's a very simple illustration. Let's consider the (sorted) numbers shown below:
#1, 5, 10, 19, 22, 23, 30
#Suppose we'd like to find the matching position of the value 1, using binary search, this is how we would proceed - because we know that the data is sorted.
#Start with the middle value = 19. Is 1 == 19? No. 1 < 19.
#Since the value we're looking for is smaller than 19, it should be somewhere before 19. So we can discard the rest of the half that are >= 19.
#Our set is now reduced to 1, 5, 10. Grab the middle value once again = 5. Is 1 == 5? No. 1 < 5.
#Our set is reduced to 1. Is 1 == 1? Yes. The corresponding index is also 1. And that's the only match.
#A vector scan approach on the other hand would have to scan through all the values (here, 7).
#https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reference-semantics.html talks more about deep copy shit
#
#
#
#for assigning multiple columns with := we must use the "Character":=.(list) format... 
#more correctly... LHS takes a **character vector** of column names and RHS a **list of values**.
#syntacticly... LHS := RHS
#RHS just needs to be a list, irrespective of how its generated (e.g., using lapply(), list(), mget(), mapply() etc.). This form is usually easy to program with and is particularly useful when you don't know the columns to assign values to in advance.
#ie
#(a)
#DT[, c("colA", "colB", ...) := list(valA, valB, ...)]
#but for single column we can do
#DT[, colA := valA]
#
#There are two ways to use the := operator
#DT[, c("colA", "colB", ...) := list(valA, valB, ...)]
#or
#(b)
#DT[, `:=`(colA = valA, # valA is assigned to colA
#          colB = valB, # valB is assigned to colB
#          ...)]
#
#(??)(b) is handy if you would like to jot some comments down for later.
#
#examples...
#flights[, `:=`(speed = distance / (air_time/60), # speed in mph (mi/h)
#               delay = arr_delay + dep_delay)]   # delay in minutes
#or
#flights[, c("speed", "delay") := list(distance/(air_time/60), arr_delay + dep_delay)]
#
#***********************
#actually use this script for replacing NA and shit in the pipeline!!
## get all 'hours' in flights
#flights[, sort(unique(hour))]
#  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
#We see that there are totally 25 unique values in the data. Both 0 and 24 hours seem to be present. Let's go ahead and replace 24 with 0.
#– Replace those rows where hour == 24 with the value 0
#Replace those rows where hour == 24 with the value 0
# subassign by reference
#flights[hour == 24L, hour := 0L]
#
#adding an extra [] afterwards like in  DT[][] will print the result, whereas DT[,:=] does not
#
#DT[a > 4, b := c] is different from DT[a > 4][, b := c].
#The first expression updates (or adds) column b with the value c on those rows where a > 4 evaluates to TRUE. X is updated by reference, therefore no assignment needed.
#The second expression on the other hand updates a new data.table that's returned by the subset operation. Since the subsetted data.table is ephemeral (it is not assigned to a symbol), the result would be lost; unless the result is assigned, for example, as follows: ans <- DT[a > 4][, b := c]. 
#ie
#ans <- DT[a > 4][, b := c]. 
#What is the difference between flights[hour == 24L, hour := 0L] and flights[hour == 24L][, hour := 0L]?
#Hint: The latter needs an assignment (<-) if you would want to use the result later.
#in other words.......
#remember. DT[] is like the output of a function. It will print the result, and it wont be saved
#whereas DT[, :=] does NOT print the result and is an UPDATE to DT, essentially its a clean way of writing DT<-DT[].
#so when we apply a function on DT[] will be lost, as its just an output, not a save. what we would have to do is FUN(DT) probably, but not FUN(DT[]) because it would be lost.
#i assume that this is the difference, where in a data frame, you can do FUN(DF[index]), but since DT[] is used for operations, maybe not. and maybe FUN(DT[index, with=FALSE]) would work... idk.
#we can add the printout of an update/reference, with an extra [] like in DT[,:=...][]
#
#
#
#MMMMMMOOOOOOOOOONNNNNNEEEEEEEEEYYYYY
#How can we add a new column which contains for each orig,dest pair the maximum speed?
#flights[, max_speed := max(speed), by = .(origin, dest)]
#or with by= as a character vector
#We could have also provided by with a character vector as we saw in the Introduction to data.table vignette, e.g., by = c("origin", "dest").
#
#!!!!!!!!!!
#the reason why we can do agnes with many output values and also sum with a single output value along with by....
#is because the output is determined on the group. then that output is recycled until it fills the length of the group. so because agnes has an output the same size as the group, it is a 1:1, but because sum is one value it is repeated to fill the group
#For each group, max(speed) is computed, which returns a single value. That value is recycled to fit the length of the group. Once again, no copies are being made at all. flights data.table is modified in-place.
#
#lapply and useful for the functional programing...
#******notice we still have to wrap ... a= c("a", "b", "c"); DT[, c(a):=...]
#Note that since we allow assignment by reference without quoting column names when there is only one column as explained in Section 2c, we can not do out_cols := lapply(.SD, max). That would result in adding one new column named out_col. Instead we should do either c(out_cols) or simply (out_cols). Wrapping the variable name with ( is enough to differentiate between the two cases.
#
#basic idea is, lapply returns a list of multiple value vectors. and each vector of values will be a new column. each new column is the output of the function on a specific column, not all columns. it creates as many new columns as are in .SDcols.
#How can we add two more columns computing max() of dep_delay and arr_delay for each month, using .SD?
#in_cols  = c("dep_delay", "arr_delay")
#out_cols = c("max_dep_delay", "max_arr_delay")
#flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
#head(flights)
#    year month day dep_delay arr_delay carrier origin dest air_time distance hour    speed max_speed
# 1: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9 413.6490  526.5957
# 2: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11 409.0909  526.5957
# 3: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19 423.0769  526.5957
# 4: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7 395.5414  517.5000
# 5: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13 424.2857  526.5957
# 6: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18 434.3363  518.4507
#    max_dep_delay max_arr_delay
# 1:           973           996
# 2:           973           996
# 3:           973           996
# 4:           973           996
# 5:           973           996
# 6:           973           996
#
#
#
# RHS gets automatically recycled to length of LHS
#flights[, c("speed", "max_speed", "max_dep_delay", "max_arr_delay") := NULL]
#
#so if we are doing functional programing with DT and we want to preserve the original AND speed things up, then we create the data table DT... then in the function we use outputs from the original DT with DT[]. the only time it would fuck up the original is if we performed an update function on it, ie one that doesnt show the output, like DT[, :=]
#Let's say we would like to create a function that would return the maximum speed for each month. But at the same time, we would also like to add the column speed to flights. We could write a simple function as follows:
#foo <- function(DT) {
#  DT[, speed := distance / (air_time/60)]
#  DT[, .(max_speed = max(speed)), by = month]
#}
#ans = foo(flights)
#head(flights)
#    year month day dep_delay arr_delay carrier origin dest air_time distance hour    speed
# 1: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9 413.6490
# 2: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11 409.0909
# 3: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19 423.0769
# 4: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7 395.5414
# 5: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13 424.2857
# 6: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18 434.3363
#head(ans)
#    month max_speed
# 1:     1  535.6425
# 2:     2  535.6425
# 3:     3  549.0756
# 4:     4  585.6000
# 5:     5  544.2857
# 6:     6  608.5714

#the copy() thing...
#copy() makes a deep copy
#an object that stores column names of a DT will be updated as column names are updated in DT.
#DTn = names(DT);
#DTn;
# [1] "a" "b" "c"
#DT[, d:=3L];
#DTn;
# [1] "a" "b" "c" "d"


#in_cols  = c("dep_delay", "arr_delay")
#out_cols = c("max_dep_delay", "max_arr_delay")
#flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]



IBS.fxn9 = function(X, nlevels=8, clustering=clustering.fxn9) {
    a = copy(X$Cells);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    Lvl = paste("IBS Lvl", 1:nlevels, sep=" ");
    for (i in 1:nlevels) {
        if (i == 1) (
            a[, Lvl[i]:=clustering(X[, c("x.tSNE", "y.tSNE", "z.tSNE")])])
        else (
            a[, Lvl[i]:=clustering.fxn2(X[, c("x.tSNE", "y.tSNE", "z.tSNE"), by=Lvl[1:(i-1)]])])
    };
    a
}

########### should rewrite everything to apply these key rules, where .....setkey(a, "B"); a[.("B")]..... and 







#this works
# Cs = paste0("C", 1:10)
#  a[, Cs[4]:=letters[7:16]] %>% setkeyv(Cs[2:4])
####okay... the basic a[...] component of the script does work when implemented manually, as
####a[, Lvls[1]:=clustering.fxn4(.SD), .SDcols=ctSNE]; setkeyv(a, c(Lvls[1]))
####a[, c(Lvls[2]):=clustering.fxn4(.SD), .SDcols=ctSNE, by=c(Lvls[1])]; setkeyv(a, c(Lvls[1:2]))
####a[, c(Lvls[3]):=clustering.fxn4(.SD), .SDcols=ctSNE, by=c(Lvls[1:2])]; setkeyv(a, c(Lvls[1:2]))


#works for when the nlvls=3 argument is set. but the issue is that the cells are too dissimilar. the function works.
IBS.fixed = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    for (i in 1:nlvls) {
        if (i == 1) (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
            setkeyv(c(Lvls[1:i]))
        )
        else (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:(i-1)])] %>%
            setkeyv(c(Lvls[1:i]))
        )
    }
    a
}

clustering.fxn6 = function(X, find.k=FALSE, find.coefficient=FALSE) {
    a = agnes(X, metric="euclidean", method="ward", stand=TRUE)
    assign(c.ac, a$ac, envir=sys.frame(which=-1))
    cutree(a, k=2)
}

IBS.4.24 = function(DT, number.of.iteration=3, cluster.fxn=clustering.fxn6, core.function=core.IBS.fxn)
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE")
    tname = function(X, Y) (paste("IBS", X, 1:number.of.iterations, Y, sep=" "))
    Lvls = tname("Level")
    Grps = tname("Group")
    Grps.Cfs = tname("Group", "Coefficients")
    for (i in 1:number.of.iterations)
        core.function()
    


core.IBS.fxn = function(DT, cluster.fxn=clustering.fxn5, mkey=Y, ...) (
    DT[, c(mkey[i]):=cluster.fxn(.SD, ...), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
)
#works great. make sure to set nlvls=3 for small data sets.
gIBS.fixed = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    for (i in 1:nlvls) {
        if (i == 1) (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
            setkeyv(c(Lvls[1:i]))
        )
        else (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:(i-1)])] %>%
            setkeyv(c(Lvls[1:i]))
        )
    }
    a[, c(ctSNE):=NULL][]
}

#this function clusters data into 2 groups based on coordinate information, then the function is performed again on each of those clusters... (4 clusters...8..16..)
#cluster.fxn is a custom function providing the essential work ie clustering assignments
#IBS stands for iterative binary splitting, and it is the way the cluster function is applied to the data
#coordinate columns are the columns of the data that is being analyzed.
IBS.fxn = function(DT, number.of.iterations=8) {
    Lvls = paste0("Level_", 1:number.of.iterations);
    coordinate.columns = c("x.col", "y.col", "z.col");
    for (i in 1:number.of.iterations) {
        if (i == 1) (
            DT[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=coordinate.columns] %>%
            setkeyv(c(Lvls[1:i]))
        )
        else (
            DT[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=coordinate.columns, by=c(Lvls[1:(i-1)])] %>%
            setkeyv(c(Lvls[1:i]))
        )
    }
}

#IBS.fxn.4.23 = function(DT, number.of.iterations=8, coordinate.colums=c("x.col", "y.col", "z.col"), cluster.fxn=clustering.fxn4, get.Levels=TRUE, get.Groups=TRUE, get.coefficients=FALSE) {
#    tname = function(X, Y) paste("IBS", X, 1:number.of.iterations, Y, sep=" ")
#    Lvls = tname("Level")
#    Grps = tname("Group")
#    Lvls.Cfs = tname("Level", "Coefficients")
#    Grps.Cfs = tname("Group", "Coefficients")
#    mclusters = list(Lvls, Grps, Lvls.Cfs, Grps.Cfs)
#    Lkey = Lvls[1:i]
#possible for (i in 0:number of iterations)... by=Lkey, keyby=Lkey
#    fxn.key = lkeys[]
#    if (get.Groups)
#    mkey =mnames[1:i]
#    if (get.Levels)
#        mkey = mclusters$Lvls
#        for (i in 1:number.of.iterations) {
#            DT[, c(mkey[i]):=cluster.fxn(.SD), .SDcols=coordinate.columns, by=.(mkey[-1]), keyby=c(Lkey)]
#        }
#    if (get.Groups)
#        for (i in 1:number.of.iterations) {
#            rec = Lvls[1:(i-1)]
#            DT[, Grps[i]:=.GRP, by=rec]
#            }
#                DT[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=coordinate.columns, by=c(Lvls[1:(i-1)])] %>%
#                setkeyv(c(Lvls[1:i]))
#            )
#        }
#}


recap = function(X, cluster.fxn=clustering.fxn5, number.of.iterations=3) {
    tname = function(X, Y) (paste("IBS", X, 1:number.of.iterations, Y, sep=" "))
    Lvls = tname("Level")
    Grps = tname("Group")
    Lvls.Cfs = tname("Level", "Coefficients")
    Grps.Cfs = tname("Group", "Coefficients")
    for (i in 1:number.of.iterations) {
        if (get.Levels) {
            mkey = Lvls
            DT[, c(mkey[i]):=cluster.fxn(.SD, find.k=TRUE), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
        }
        if (get.Groups) {
            mkey = Grps
            DT[, c(mkey[i]):=.GRP, by=.(mkey[1:i])]
        }
        if (get.Coefficients) {
            mkey = Lvls.Cfs
            DT[, mkey[i]:=cluster.fxn(.SD, find.coefficient=TRUE), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
        }
    }
}

core.IBS.fxn = function(DT, cluster.fxn=clustering.fxn5, mkey=Y, ...) (
    DT[, c(mkey[i]):=cluster.fxn(.SD, ...), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
)

recap2 = function(X, number.of.iterations=3) {
    tname = function(X, Y=NULL) (paste("IBS", X, 1:number.of.iterations, Y, sep=" "))
    Lvls = tname("Level")
    Grps = tname("Group")
    Grps.Cfs = tname("Group", "Coefficients")
    for (i in 1:number.of.iterations) {
        core.IBS.fxn(X, mkey=Lvls, find.k=TRUE)
        X[, c(Grps[i]):=.GRP, by=.(Lvls[1:i]), keyby=c(key(X), Grps[i])]
        core.IBS.fxn(X, mkey=Grps.Cfs, find.coefficient=TRUE)
    }
}

recap3 = function(X, number.of.iterations=3) {
    tname = function(X, Y=NULL) (paste("IBS", X, 1:number.of.iterations, Y, sep=" "))
    Lvls = tname("Level")
    Grps = tname("Group")
    Grps.Cfs = tname("Group", "Coefficients")
    for (i in 1:number.of.iterations) {
        core.IBS.fxn(X, mkey=Lvls, find.k=TRUE)
    }
    for (i in 1:number.of.iterations) {
        X[, c(Grps[i]):=.GRP, by=.(Lvls[1:i])]
    }
    for (i in 1:number.of.iterations) {
        core.IBS.fxn(X, mkey=Grps.Cfs, find.coefficient=TRUE)
    }
}

recap4 = function(X, number.of.iterations=3, cluster.fxn=clustering.fxn5) {
    tname = function(X, Y=NULL) (paste("IBS", X, 1:number.of.iterations, Y, sep=" "))
    Lvls = tname("Level")
    Grps = tname("Group")
    Grps.Cfs = tname("Group", "Coefficients")
    for (i in 1:number.of.iterations) {
        DT[, c(Lvls[i]):=cluster.fxn(.SD, find.k=TRUE), by=.(Lvls[0:(i-1)]), keyby=c(Lvls[1:i])]
    }
    for (i in 1:number.of.iterations) {
        DT[, c(Grps[i]):=.GRP, by=c(Lvls[1:i])]
    }
    nkey = copy(key(X))
    for (i in 1:number.of.iterations) {
        DT[, c(Grps.Cfs[i]):=cluster.fxn(.SD, find.coefficient=TRUE), by=.(Lvls[0:(i-1)]), keyby=c(nkey, )]
    }
}



#possible.index.name.fxns = function(X) {
#assign(Lvls, tname("Level"), envir = sys.frame(which = -1))
#assign(Lvls, tname("Level"), envir = .GlobalEnv)
#}
# 
#                    DT[, c(mkey[i]):=cluster.fxn(.SD, find.k=TRUE), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
#        }
#        if (get.Groups) {
#            mkey = Grps
#            DT[, c(mkey[i]):=.GRP, by=.(mkey[1:i])]
#        }
#        if (get.Coefficients) {
#            mkey = Lvls.Cfs
#            DT[, mkey[i]:=cluster.fxn(.SD, find.coefficient=TRUE), .SDcols=c(coordinate.columns), by=.(mkey[0:(i-1)]), keyby=c(mkey[1:i])]
#        }
#    }
#}



#trial.fxn = 
#    for (i in 1:number.of.iteration)
#        if (get.Levels)
#        if (get.Groups)
#        if (get.Coefficients)

#trialfxn = 
#Lkey = Lvls[1:i]
#    if (get.Levels)
#        for (i in 0:number.of.iterations) {
#            DT[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=coordinate.columns, by=.(Lkey[-i]), keyby=c(Lkey)]
#        }



#trial = function(X) {
#    a = 2
#    for (i in 1:4)
#        if (i == 1) (a = 3)
#        print(X + a)
#}

#trial2 = function(X) {
#    for (i in 1:4)
#    print(i)
#}


###works up to a certain level i think...
IBS.fixed2 = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
        setkeyv(c(Lvls[1]));
    a[, c(Lvls[2]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1])] %>%
        setkeyv(c(Lvls[1:2]));
    a[, c(Lvls[3]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:2])] %>%
        setkeyv(c(Lvls[1:3]));
    a[, c(Lvls[4]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:3])] %>%
        setkeyv(c(Lvls[1:4]));
    a[, c(Lvls[5]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:4])] %>%
        setkeyv(c(Lvls[1:5]));
    a[, c(Lvls[6]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:5])] %>%
        setkeyv(c(Lvls[1:6]));
    a[, c(Lvls[7]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:6])] %>%
        setkeyv(c(Lvls[1:7]));
    a[, c(Lvls[]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:7])] %>%
        setkeyv(c(Lvls[1:8]));
    a
    }

#IDK if this works...
IBS.fixed3 = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
        setkeyv(c(Lvls[1:1]));
    for (i in 2:nlvls) (
    a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[(i-1)])] %>%
        setkeyv(c(Lvls[1:i]))
    )
    a
    }

#did not produce as many groups as IBS.fixed2 or IBS.fixed.
IBS.fixed4 = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
        setkeyv(c(Lvls[1:1]));
    for (i in 1:nlvls) (
        if (i == 1) (
            a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
            setkeyv(c(Lvls[1:1]))
        )
        else (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[(i-1)])] %>%
            setkeyv(c(Lvls[1:i]))
        )
    )
    a
    }

IBS.fixed5 = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    for (i in 1:nlvls) (
        if (i == 1) (
            a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
            setkeyv(c(Lvls[1:1]))
        )
        else (
            a[, c(Lvls[i]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[(i-1)])] %>%
            setkeyv(c(Lvls[1:i]))
        )
    )
    a
    }

#shows how many cells are in each IBS cluster. shows that some are drastically bigger than others
IBS.analysis = function(cluster) {
    cluster[, .N, by=c(Lvls[1:5])][order(N)]
}


IBS.fixed6 = function(DT, nlvls=8, cluster.fxn=clustering.fxn4) {
    a = copy(DT)
    Lvls = paste0("Level_", 1:nlvls);
    ctSNE = c("x.tSNE", "y.tSNE", "z.tSNE");
    a[, c(Lvls[1]):=cluster.fxn(.SD), .SDcols=ctSNE] %>%
        setkeyv(c(Lvls[1]));
    a[, c(Lvls[2]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1])] %>%
        setkeyv(c(Lvls[1:2]));
    a[, c(Lvls[3]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:2])] %>%
        setkeyv(c(Lvls[1:3]));
    a[, c(Lvls[4]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:3])] %>%
        setkeyv(c(Lvls[1:4]));
    a[, c(Lvls[5]):=cluster.fxn(.SD), .SDcols=ctSNE, by=c(Lvls[1:3])] %>%
        setkeyv(c(Lvls[1:5]));
    a
    }
