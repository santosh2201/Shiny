require(rtracklayer)
require(data.table)

gtf <- readGFF("C:/Users/tenr2y/Desktop/Genecode/gencode.v28lift37.annotation.gtf", version=2L)

format_gene <- sapply(strsplit(unlist(gtf["gene_id"]), split='.', fixed=TRUE), function(x) (x[1]))
gtf["gene_id"] <- format_gene

gtf <- as.data.table(gtf)
gtfgene <- gtf[type == 'gene']
gtftrans <- gtf[type == 'transcript']
gtfexon <- gtf[type == 'exon']
write.csv(gtfgene, file = "C:/Users/tenr2y/Desktop/Genecode/genecode_gene1.csv",row.names=FALSE)
write.csv(gtftrans, file = "C:/Users/tenr2y/Desktop/Genecode/genecode_trans1.csv",row.names=FALSE)
write.csv(gtfexon, file = "C:/Users/tenr2y/Desktop/Genecode/genecode_exon1.csv",row.names=FALSE)

