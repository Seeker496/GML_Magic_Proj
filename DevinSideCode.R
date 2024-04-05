library(tidyverse)
library(igraph)




#A similar func to the main file, but instead it makes 1 edge connection for everything that has 
WRof55 <- function (c1,c2){
  temp <- DowntoDecks %>% filter(.[[c1]] > 0 & .[[c2]] > 0) %>% count(won)
  if(temp %>% as.matrix() %>% length != 4) { 
    # The matrix is the wrong size if they have no connection
    return(0)
  } else if (temp[[2,2]]/(temp[[2,2]] + temp[[1,2]]) >= 0.55) { 
    # Add an edge between any cards that have a pairwise wr >= 55%
    return(1)
  }
  return(0) # Return 0 otherwise
}



LinkGraph <- sapply(2:ncol(DowntoDecks), \(x) 
                    mapply(WRof55, x, 2:ncol(DowntoDecks), 
                           SIMPLIFY = T))

WR55Graph <- as.matrix(LinkGraph, "adjacency") |> 
  graph_from_adjacency_matrix(mode = "undirected")

plot(WR55Graph, layout = layout_with_drl,
     vertex.label = NA, vertex.size = 4)


