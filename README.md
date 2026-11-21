# TB NanoString Shiny App

This Shiny app explores the TB NanoString gene expression dataset using three dimension reduction methods: PCA, UMAP, and NMF. It also applies k-means clustering and compares the resulting clusters with TB status using a cross-tabulation and Adjusted Rand Index (ARI).

## Files
- `app.R`: Shiny application code.
- `TBnanostring.rds`: Gene expression dataset used by the app.

## Requirements

Install the required R packages:

```r
install.packages(c("shiny", "tidyverse", "uwot", "mclust"))
BiocManager::install("NMF")
```

If needed, install BiocManager first with `install.packages("BiocManager")`.

## Running the app

1. Download or clone this repository.
2. Open the project folder in RStudio.
3. Make sure `app.R` and `TBnanostring.rds` are in the same directory.
4. Run the following command in R:

```r
shiny::runApp()
```

The app allows users to select PCA, UMAP, or NMF, change the number of k-means clusters, and view the resulting plot, cluster assignments, and ARI.
