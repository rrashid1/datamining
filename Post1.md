Post 1: Community Detection on Real-World Networks
================

Objective
=========

In this post, I will go through a graph problem called "Community Detection". Community detection is the problem of finding community structure in a graph and a very important problem in the area of graph theory and social network analysis. As per Wikipedia, "a network is said to have community structure if the nodes of the network can be easily grouped into (potentially overlapping) sets of nodes such that each set of nodes is densely connected internally." For example, people living in a same area interacts more often and forms a community. Similarly, as the concept of area is vague in social networks, it is often desirable to know

Data
====

To study the problem, I choose an widely known network with several community For ease of understanding I will use a relatively smaller set of network data. The data is widely available all over internet. I took some inspiration from: <https://bost.ocks.org/mike/miserables/>.

The data consists of 77 characters from the novel Les Miserable by Victor Hugo. Each edge represents whether the two characters appears in a same chapter. The corresponding weight of the edge represents how many times the occurrences occurred.

``` r
library(tidyverse)
library(igraph)
library(statnet)
library(tidygraph)
library(ggraph)
require(gridExtra)
library(igraph)
library(dplyr)
library(ggplot2)
library(plyr)
```

Rading Graph
------------

We read the network from the CSV files and compute some basic statistics such as Degree of each node, and Betweenness Centrality. We are given a community of each character in the vertices file that we use as the optimal community assignment. Later we discuss how to determine communities.

``` r
original_vertices <- read.csv("data/les_mis_vertices.csv", stringsAsFactors = FALSE)
original_edgelist <- read.csv("data/les_mis_edges.csv", stringsAsFactors = FALSE)

names <- unique(c(original_edgelist$from, original_edgelist$to))

g <- graph_from_data_frame(d=original_edgelist, vertices = original_vertices, directed=TRUE)

V(g)$Degree <- centralization.degree(g)$res
V(g)$Betweenness <- centralization.betweenness(g)$res
V(g)$Community <- V(g)$Community
V(g)$Group <- V(g)$Community

V(g)$r <- pmax(0.04, log(V(g)$Degree)/10) #Radius of node Just for visualization purpose
```

Graph Visualization
-------------------

After reading the graph, we can plot the graph. Here, I am using *ggraph* package to plot graphs instead of using igraph directly. I am coloring the nodes based on their assigned communities. The size of the nodes are varied by their degree. The following graph clearly depicts the community which can be understood by the color and the way they are positioned. Note that we used force based lay-outing algorithm which tends to show real-world graphs very well.

``` r
plot_graph <- function(g, color_by="Community", show_text=F){
  gGraph <- as_tbl_graph(g)
  V(gGraph)$Group <- factor(get.data.frame(g, what = "vertices")[,color_by])
  set.seed(12)
  p <-ggraph(gGraph, layout = "fr", maxiter = 1000) + theme_graph() +
  geom_node_circle(aes(colour = factor(Group), fill = factor(Group)), show.legend = F) +
  geom_edge_link(aes(width = weight), alpha = 0.2, show.legend = F) + 
  scale_edge_width(range = c(0.2, 1)) + ggtitle(paste("Colored by", color_by))
  
  if(show_text)
    p = p + geom_node_text(aes(label = name), repel = TRUE, show.legend = F, size=4)
  
  return(p)
}

plot_graph(g, "Community", T)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-3-1.png)

Node coloring by degree
-----------------------

In the previous graph, we colored the nodes based on their assigned community. If we are given a very large graph, more often it does not have any assigned community. The task of the data mining expert in this situation is to determine community structure from the network. One might wonder that, what if we assign communities based on the degree of each node. In the following figure, I plot a graph colored by the degree of the network. As seen from the figure, we does not get any discernible information from the network visualization based on graph color only. Therefore, the simple approach of graph coloring with *degree* does not yield good result. Later in this post, I will show some approaches that work.

``` r
plot_graph(g, "Degree", T)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-4-1.png)

The characters have many inter-connections. Its easy to notice that the three main characters Marius, Valjean, and Cosette form a triangle. Most of the communities have many triangles, i.e., if one character A knows a character B, and B knows another character C, then it is highly likely that A knows C. Whenever this happens, a community seems to form. Therefore, triangles are a key feature of social networks and communities.

Degree Distribution
===================

Now lets look into the degree distribution of the network. It is widely known that real world networks tend to follow power law degree distribution. As the Les Miserable seems to a real network, we expect it to follow power law degree distribution. Indeed, from the figure below, we can observe that the network exhibit power law degree distribution. We plot the figure in log-log scale and we can observe that the points tend to form a straight line.

We can see that one of the node of the graph has many connections (18 connections to be exact) and there are many nodes (17 to be precise) that only have 1 nodes which is a characteristics we find in real life.

``` r
G.degree.histogram <- as.data.frame(table(V(g)$Degree))
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])

ggplot(G.degree.histogram, aes(x = Var1, y = Freq)) + geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)", breaks = c(1, 2, 4, 8, 16, 32), trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)", breaks = c(1, 2, 4, 8, 16, 32), trans = "log10") +
  ggtitle("Degree Distribution (log-log)") + 
  theme_minimal()
```

![](Post1_files/figure-markdown_github/unnamed-chunk-5-1.png)

Using Adjacency Matrix to Visualize and Determine Clusters
==========================================================

Adjacency matrix is another representation of graphs and can be very useful to identify critical structures such as community. In this section I try some representation of adjacency matrix to determine community structures.

The following code snippets generates a adjacency matrix representation of the graph. I am using *geom\_raster* frequently used for heat maps to represent adjacency matrix.

``` r
node_list <- get.data.frame(g, what = "vertices")

all_nodes <- sort(node_list$name)

edge_list <- get.data.frame(g, what = "edges") %>%
  inner_join(node_list %>% select(name, Community), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, Community), by = c("to" = "name")) %>%
  mutate(group = ifelse(Community.x == Community.y, Community.x, NA) %>% factor())

plot_data <- edge_list %>% mutate(to = factor(to, levels = all_nodes), from = factor(from, levels = all_nodes))

matrix_plot <- function(plot_data, arr_var="name"){
  plot_data <- plot_data[,c("to", "from", "group")]
  df1 <- data.frame(to=plot_data$from, from=plot_data$to, group=plot_data$group)
  df2 <- data.frame(to=node_list$name, from=node_list$name, group=node_list$Community)
  plot_data <- unique(rbind(plot_data, df1, df2))
  name_order <- (node_list %>% arrange_(arr_var))$name
  plot_data <- plot_data %>% mutate(to = factor(to, levels = name_order), from = factor(from, levels = name_order))

  p <- ggplot(plot_data, aes(x = from, y = to, fill = group))
    p <- p + geom_raster() +
      theme_minimal() + scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE) +
      theme(axis.text.x = element_text(angle = 270, hjust = 0, size = 8),
            axis.text.y = element_text(size = 8),
            aspect.ratio = 1, legend.position = "none")
    return(p)
}
```

The effectiveness of the adjacency matrix is shown when the vertices of the graphs are kept close together. First we show what happens when the vertices are sorted based on their name instead of their structural properties.

``` r
matrix_plot(plot_data, "name")
```

![](Post1_files/figure-markdown_github/unnamed-chunk-7-1.png)

When name is used such as the figure above, we can't really see any structure, everything is mostly random. Now, the following code keeps the vertices of the assigned community together.

``` r
matrix_plot(plot_data, "Community")
```

![](Post1_files/figure-markdown_github/unnamed-chunk-8-1.png)

As we can see from the above figure, using community to keep the vertices closer, we can clearly see a structure of communities. Clearly, the assigned community provides better visualization and understanding.

What if we don't have the communities in our graph as is the case for most of the graph networks available online. Can we use some other network measures such as Degree or Betweenness Centrality? The following code generates the adjacency matrix using Degree and Betweenness centrality.

``` r
matrix_plot(plot_data, "Degree")
```

![](Post1_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
matrix_plot(plot_data, "Betweenness")
```

![](Post1_files/figure-markdown_github/unnamed-chunk-9-2.png)

As evident from the figure, although it is useful, the adjacency matrix does not clearly shows community structures. Therefore, we need some more ways to obtain community structure.

Algorithms to determine community
=================================

Fortunately, there are many algorithms to determine community structures. In *igraph* package, we have fast greedy, multilevel, walk trap etc. (You can read more about it here: <https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph>). In the following code snippets, I am evaluating the available modularity of the community detection algorithms.

``` r
g <- as.undirected(g)
V(g)$fastgreedy_comm <- membership(fastgreedy.community(g))
V(g)$multilevel_comm <- membership(multilevel.community(g, weights = E(g)$weight))
V(g)$walktrap_comm <- membership(walktrap.community(g))
V(g)$edge_comm <- membership(edge.betweenness.community(g))
V(g)$spinglass_comm <- membership(spinglass.community(g))
V(g)$infomap_comm <- membership(infomap.community(g))

node_list <- get.data.frame(g, what = "vertices")
```

fastgreedy\_comm
================

``` r
grid.arrange(plot_graph(g, "fastgreedy_comm"), matrix_plot(plot_data, "fastgreedy_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-11-1.png)

multilevel\_comm
================

``` r
grid.arrange(plot_graph(g, "multilevel_comm"), matrix_plot(plot_data, "multilevel_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-12-1.png)

walktrap\_comm
==============

``` r
grid.arrange(plot_graph(g, "walktrap_comm"), matrix_plot(plot_data, "walktrap_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-13-1.png)

edge\_comm
==========

``` r
grid.arrange(plot_graph(g, "edge_comm"), matrix_plot(plot_data, "edge_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-14-1.png)

spinglass\_comm
===============

``` r
grid.arrange(plot_graph(g, "spinglass_comm"), matrix_plot(plot_data, "spinglass_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-15-1.png)

infomap\_comm
=============

``` r
grid.arrange(plot_graph(g, "infomap_comm"), matrix_plot(plot_data, "infomap_comm"), ncol=2)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-16-1.png)

Evaluation
----------

As clearly seen from the above figures, although the community detection algorithms produces much more clear representation of graphs, there is not clear winner. Therefore, there is no one size fits all solution. It depends on the nature of the problem to use the appropriate community detection algorithm. However, for most of the time the *fastgreedy\_comm* algorithm works well to present an initial idea of community and extremely fast. Therefore, if the network is very large, this algorithm is generally preferable.

Conclusion
==========

In this post, we explored how graphs can be used to get essential insights to have a greater understanding. Specially, we experimented with the layout of the graph along with their adjacency matrix representation. We saw that simple graph properties does not lead to great community structures. However, there are many community detection algorithms that can be used for community detection algorithm. We generated networks and adjacency matrices for many of these algorithms.

Future Work and Exercise
========================

In future, we can experiment with even larger network to determine community structures. We can analyze the size and distribution of community structures. Typically, in large networks we might see the size of the communities are distributed as a power law distribution, which can not be observed for such a small network we studied here.

As an exercise, interested readers are encouraged to generate and export their Facebook network to determine communities. What communities do you expect to see? Family, school friends, university friends, colleagues? Which algorithm worked best to describe your community?

References
==========

<http://www-personal.umich.edu/~mejn/netdata/>
