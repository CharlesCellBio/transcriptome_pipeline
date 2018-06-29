#create an index that will be used for reffering to each of the drop-Seq samples (UMI matrices: genes x cell-barcodes) that were collected.
lnames<- c("cardiac", "head", "trunk", "we");
#initiate a loop which will
## 1) read the files containing raw UMI matrices of each sample (matrices already contain cell barcodes in row1 and gene names on column1)
## 2) select "good data" (cells which aren't likely the product of excessive dropout or a readout of more than one cell)
## 3) merge "good data" from each sample into a single object
#
#
#
#
# insert question.. do you want to display histogram... with cutoff tail highlighted...
#for fast computation of the histogram, perform it on a sample of the data.
### histogram sample should be...dt[order(rowSum)][seq(500, 1:length(DT))]
## also.. i think the entire data.table should recieve the key, then be bound by the cell column..? or if the key is set to all, it should make a full complete merge, right???
for (i in lnames){
#read the data into a data.table object, the first column will contain cell barcodes and is named "Cells", all other column names are genes.
#the next 4 lines will:
### 1) use the new data.table object type. 
### 2) transpose the data to contain "instances" in rows and "variables" in columns. This seems to be the prefered input format for a number of the statistical and R functions that I've come across, including our downstream PCA and tSNE functions.
### 3) maintain the appropriate "numeric" and "character" classes wile reading/transposing
### 4) assign sample-specific suffixes to avoid deleting cells with duplicate names between samples. Not sure if the original pipeline did this, but mine did for a while.
#could also be written this way, but much slower, though it may use less total ram at any given point.
#tDT = data.table("Cells"=as.character(fread(paste0("my.", i, ".matrix.tsv"), header=F, nrows=1, drop=1)), transpose(fread(paste0("my.", i, ".matrix.tsv"), drop=1)), key="Cells");
#colnames(tDT)[-1] = c(fread(paste0("my.", i, ".matrix.tsv"), select=1))[[1]];
#this may be faster, I couldn't tell.
###ttDT = fread(paste0("my.", i, ".matrix.tsv");
###tDT = data.table("Cells"=colnames(ttDT)[-1], transpose(ttDT[,-1]));
###colnames(tDT)[-1] = ttDT$Gene;
#
tDT = fread(paste0("my.", i, ".matrix.tsv"));
names = tDT$Gene;
tDT = data.table("Cells"=paste("TS19", colnames(tDT)[-1], i, sep="."), transpose(tDT[,-1]));
colnames(tDT)[-1] = names;
#print(paste("tdt", i, sep="."));
# i still have a couple of questions about how the 3-NM subsetting is being implemented. 
## I think a left-skewed, 2-normal mixture model might be a more accurate description of the situation. Hopefully we can talk about it in person.
## I found a fairly new package, MixAK, which deals with normal mixture modeling. It looks like it creates, graphs, and measuring the confidence of the models all within R, so it may be nice to implement. I haven't spent enough time with it to understand how it works yet, but utilizing its functionality at this point may be something to consider in the future.
#
#### i would eventually like to use the mixAK package to further analyze the dynamics of the 3NMM within R, and locally subset the data using its output, but I'm not comfortable with its functions yet.
#
#### The currently used method of selecting good data exports data to JMP for 3NM implementation and selects defines gooda data as...
#######... [p.05(log10(1/3NM)) < good_data < p.95(log10(3/3NM))]...
####....and essentially results in a selection of cells analagous to [p0.10 < good_cells < p0.85] of a standard/single normal probability distribution for every sample.
#### For brevity sake, and for ease of implementing modifica toins on this section at a later date, this script merely selects/defines good data as...
####### ... p0.10 < good_cells < p0.85 ...
#
#### By doing this, the current script
###### 1) keeps the good_data selection/indexing step within R (as I would like to evenually do)
###### 2) keeps the selection/indexing step isolated so it can be easily replaced/found/modified/studied
###### 3) simplifies the code, as its not permanent at this point
###### 4) mimics the current 3NM/JMP output enough to allow my pipeline's downstream results to be comparable to those in the current pipeline while I finish writing the code.
#
#### The actual JMP-3NM statistics for each sample (aka the JMP datatable output used in the current pipeline for indexing the good data) were saved and can be accessed via (using R syntax)...
###### ... "C:\\Users\\moran\\Documents\\myPipeline\\Step1\\S1 work area\\S1workingwd\\my.jmp.cardiac.normal3dt.csv"...
####... where "cardiac" is replaced with "head", "trunk", and "we" accordingly.
#
#create an index to select the "good data"
rs = rowSums(tDT[,-1]);
index = as.logical(rs < quantile(rs, probs=0.85) & rs > quantile(rs, probs=0.1));
#***
#***
#***
#**********i dont think the DT[] syntax allows full outer merge, so merge() is actually necessary, but double check.
#***
#***
#***
# merge good data into a single object
##** double check.. we did make sure that each sample had its own suffix, but there may be identical barcodes even within the samples. might be a good idea to do the suffix=__ argument or include something earlier for possible duplicates. try unique().
if (i == lnames[1]) (TS19<- tDT[index])
else (TS19 = rbindlist(list(TS19, tDT[index]), use.names=T, fill=T, idcol=NULL))};
#housekeeping
rm(list=ls()[ls()!="TS19"]); 
#use sample/dummy data for now
TS19 = TS19[sample(1:nrow(TS19), 500)]
#reorder and as a matrix
#we can reorder the cell barcodes alphabetically although it does nothing for sample grouping using TS19 = TS19[order(Cells)]
row.names(TS19) <- TS19$Cells;
TS19$Cells <- NULL;
setcolorder(TS19, order(colnames(TS19)));
TS19 <- data.matrix(TS19);
#replace NA values with 0
TS19[is.na(TS19)] = 0;
#normalize values
factor <- median(rowSums(TS19))/rowSums(TS19);
TS19 <- log2(1 + sweep(TS19, 1, factor, "*"));
# remove genes with 0 expression
TS19 = TS19[,colSums(TS19)!=0];
#run PCA analysis
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
print(corner(master.table));
