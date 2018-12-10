Post 1: Analyzing Co-Occurance Networks of Les Misérables Characters
================

``` r
library(SOMbrero)
data(lesmis)
summary(lesmis)
```

    ## IGRAPH 3babff7 U--- 77 254 -- 
    ## + attr: layout (g/n), id (v/n), label (v/c), value (e/n)

``` r
plot(lesmis,vertex.size=0)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
library(igraph)
library(dplyr)
library(ggplot2)

# Read in CSV files with edge and node attributes
original_edgelist <- read.csv("data/les_mis_edges.csv", stringsAsFactors = FALSE)
#original_nodelist <- read.csv("goltzius_nodes.csv", stringsAsFactors = FALSE)
```
