BiocManager::install( "DESeq2" )

suppressMessages(library("DESeq2"))
suppressMessages( library("ggplot2") )
library('recount')

browseVignettes('recount')

load("rse_gene.RData")

rse_gene <- rse_gene

# We set up differentially expressed gene selection criterion here. 
pThr        <- 0.01   # for adj. p-value threshold (<= 0.01)
logFCThr    <- 1      # for log2 fold-change (at least 2 fold)
baseMeanThr <- 20     # for average expression level (at least 20 counts).
cpmThr      <- 1      # for copy-per-million (at least 1 cpm). 

# examine all data component
head( assay( rse_gene ) )
rowData( rse_gene )
colData( rse_gene )

## View GEO ids
colData(rse_gene)$geo_accession

## Extract the sample characteristics
geochar <- lapply(split(colData(rse_gene), seq_len(nrow(colData(rse_gene)))), geo_characteristics)

## Note that the information for this study is a little inconsistent, so we
## have to fix it.
geochar <- do.call(rbind, lapply(geochar, function(x) {
  for (name in colnames(x)) {
    return(x)
}}))

## We can now define some sample information to use
sample_info <- data.frame(
  run = colData(rse_gene)$run,
  group = ifelse(grepl("Autism", colData(rse_gene)$title), "Autism", "Control"),
  tissue = geochar$tissue
)

## Scale counts by taking into account the total coverage per sample
rse <- scale_counts(rse_gene)

##### Details about counts #####

## Scale counts to 40 million mapped reads. Not all mapped reads are in exonic
## sequence, so the total is not necessarily 40 million.
colSums(assays(rse)$counts) / 1e6

## Compute read counts
rse_read_counts <- read_counts(rse_gene)
## Difference between read counts and number of reads downloaded by Rail-RNA
colSums(assays(rse_read_counts)$counts) / 1e6 -
  colData(rse_read_counts)$reads_downloaded / 1e6


## Add sample information for DE analysis
colData(rse)$group <- sample_info$group
colData(rse)$tissue <- sample_info$tissue

dds <- DESeqDataSet(rse, ~ tissue + group)

## Perform DE analysis
dds <- DESeq(dds, test = "LRT", reduced = ~tissue, fitType = "local")

# create DESeq data set, using paired design.
dds <- DESeqDataSet(rse_gene, design = ~ read_count_as_reported_by_sra + group)
