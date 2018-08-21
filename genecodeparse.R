require(rtracklayer)
gtf <- readGFF("C:/Users/tenr2y/Desktop/Genecode/gencode.v28lift37.annotation.gtf", version=2L)
s1 <- sapply(strsplit(unlist(gtf["gene_id"]), split='.', fixed=TRUE), function(x) (x[1]))
gtf["gene_id"] <- s1
write.csv(gtfs, file = "C:/Users/tenr2y/Desktop/Genecode/genecodexon.csv",row.names=FALSE)
require(data.table)
gtf <- as.data.table(gtf)
gtfs <- gtf[type == 'exon']

gtfs
