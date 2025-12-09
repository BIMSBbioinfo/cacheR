## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ---- echo = FALSE------------------------------------------------------------
suppressPackageStartupMessages({
        library(cacheR) 
        library(DESeq2) 
        library(dplyr) 
        library(readr) 
})

## -----------------------------------------------------------------------------
work_dir <- file.path(tempdir(), "cacheR_rnaseq_vignette") 
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE) 

counts_dir <- file.path(work_dir, "counts") 
dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE) 

cache_dir <- file.path(work_dir, "cache") 
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

## -----------------------------------------------------------------------------

set.seed(1) 

genes <- paste0("Gene", sprintf("%03d", 1:500)) 

simulate_sample <- function(sample_id, n_genes = length(genes)) { 
    data.frame( 
        gene_id = genes, 
        count = rpois(n_genes, lambda = sample(10:1000, 1)), stringsAsFactors = FALSE ) 
} 
        
sample_ids <- paste0("sample", 1:4) 

for (sid in sample_ids) { 
    tab <- simulate_sample(sid) 
    readr::write_tsv(tab, file.path(counts_dir, paste0(sid, "_counts.tsv")) ) 
} 
sample_table <- data.frame( sample_id = sample_ids, condition = rep(c("A", "B"), each = 2), stringsAsFactors = FALSE )


## -----------------------------------------------------------------------------

read_counts_from_dir <- function(count_dir, sample_table) { 
files <- list.files(count_dir, pattern = "_counts\\.tsv$", full.names = TRUE) 
if (length(files) == 0L) { 
        stop("No count files found in: ", count_dir) 
} 
sample_ids <- gsub("_counts\\.tsv$", "", basename(files)) 
counts_list <- lapply(files, function(f) { 
        readr::read_tsv(f, show_col_types = FALSE) 
}) 
merged <- counts_list[[1]][, "gene_id", drop = FALSE] 
for (i in seq_along(counts_list)) { 
        merged[[sample_ids[i]]] <- counts_list[[i]]$count } 
        merged <- merged[, c("gene_id", sample_table$sample_id), drop = FALSE] 
        merged 
}


## -----------------------------------------------------------------------------
cached_read_counts <- cacheR:::cacheFile( cache_dir = cache_dir ) %@% function(count_dir, sample_table) { 
    read_counts_from_dir(count_dir, sample_table) 
}


## -----------------------------------------------------------------------------

# First run: reads from disk and caches 


## -----------------------------------------------------------------------------
counts1 <- cached_read_counts(counts_dir, sample_table)

## ---- eval=TRUE---------------------------------------------------------------
counts2 <- cached_read_counts(counts_dir, sample_table) 

identical(counts1, counts2)

## -----------------------------------------------------------------------------
new_id <- "sample5" 
sample_table2 <- bind_rows( sample_table, data.frame(sample_id = new_id, condition = "A") ) 
tab_new <- simulate_sample(new_id) 
readr::write_tsv(tab_new, file.path(counts_dir, paste0(new_id, "_counts.tsv")) ) 
counts3 <- cached_read_counts(counts_dir, sample_table2) 

dim(counts1)

## -----------------------------------------------------------------------------
dds_from_counts <- cacheR:::cacheFile( cache_dir = cache_dir ) %@% function(counts_tbl, sample_table) { 
       
        mat <- as.matrix(counts_tbl[, sample_table$sample_id]) 
        
        rownames(mat) <- counts_tbl$gene_id 
        
        coldata <- as.data.frame(sample_table) 
        
        rownames(coldata) <- coldata$sample_id

        DESeqDataSetFromMatrix( 
                countData = mat, 
                colData   = coldata, 
                design    = ~condition 
        ) 
        
}



## -----------------------------------------------------------------------------

dds <- dds_from_counts(counts3, sample_table2)
dds

# standard DESeq2 object


## -----------------------------------------------------------------------------

run_deseq <- cacheR:::cacheFile( cache_dir = cache_dir ) %@% function(dds) { 
        DESeq(dds) 
        } 
dds_fit <- run_deseq(dds)


## -----------------------------------------------------------------------------

res <- results(dds_fit, contrast = c("condition", "B", "A")) 

head(res[order(res$padj), ])


## -----------------------------------------------------------------------------

GLOBAL_DIR <- counts_dir 

hardcoded_fun <- cacheR:::cacheFile( cache_dir = cache_dir ) %@% function() {

        # Using a global variable as directory path 
        n1 <- length(list.files(GLOBAL_DIR)) 
        n2 <- length(dir(GLOBAL_DIR)) 
        c(literal = n1, global = n2) 
}

# First call: computes and caches 

hardcoded_fun()

# Add another file to the directory 

extra <- simulate_sample("extra") 

readr::write_tsv(extra, file.path(GLOBAL_DIR, "extra_counts.tsv") )

# Second call: cache is invalidated and recomputed 

hardcoded_fun()


## -----------------------------------------------------------------------------
sessionInfo()

