# TopologyNLP

```{r}
bottleNeckManhattan_15 <- pamx
bottleNeckMahnattan_15CLUSTERS <- as.data.frame(bottleNeckManhattan_15$silinfo)
```


```{r}
evaluateClusters <- function(dataframe,i){
    return(row.names(subset(dataframe,widths.cluster == i)))
}

clusteringScore <- function(dataframe){
  df <- data.frame(matrix(0L,nrow = length(unique(dataframe$widths.cluster)), ncol = 5))
  colnames(df) <- topics
  rownames(df) <- unique(dataframe$widths.cluster)
  for (i in rownames(df)){
    clusterVector <- evaluateClusters(dataframe,i)
    for (value in clusterVector){
      relBook <- gutenberg_metadata %>% filter(gutenberg_id == value)
      category <- relBook$gutenberg_bookshelf
      for (genre in colnames(df))
        if (grepl(genre,category)){
          df[i,genre] <- df[i,genre] + 1
        }
    }
  }
  normalized <- normalize.rows(df)
  colnames(normalized) <- topics
  rownames(normalized) <- unique(dataframe$widths.cluster)
  return(as.data.frame(normalized))
}
```

```{r}
clusteringScore(bottleNeckMahnattan_15CLUSTERS)
```

