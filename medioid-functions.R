

library(tidytext)
library(knitr)
library(dplyr)
library(stringr)
library(tokenizers)
library(gutenbergr)
library(rapportools)
library(textstem)
library(text2vec)
library(stopwords)
library(TDA)
library(philentropy)
library(lsa)
library(quanteda)
library(qlcMatrix)
library(irlba)
library(tm)
library(stringi)
library(utf8)
library(cluster)
library(gofastr)
library(vegetarian)



# ----> Persistence Diagram Functions

clustering_PD_wasserstein <- function(numLines, collection){
  similarity_matrix <- matrix(ncol = length(collection), nrow = length(collection))
  diag(similarity_matrix) <- 0
  for (i in 1:length(collection)){
    for (j in 1:length(collection)){
      if (j > i){
        book_DTM <- compute_DTM(collection[j],numLines)
        other_book_DTM <- compute_DTM(collection[i],numLines)
        if (!is.null(book_DTM) && !is.na(other_book_DTM)){  
          this_book <- make_distance_matrix(book_DTM)
          other_book <- make_distance_matrix(other_book_DTM)

          diag_this <- ripsDiag(X = this_book,
                                maxdimension = 1,
                                maxscale = 100,
                                dist = "arbitrary", 
                                library = "Dionysus",
                                printProgress = FALSE, 
                                location=TRUE)
          diag_other <- ripsDiag(X = other_book,
                                 maxdimension = 1,
                                 maxscale = 100,
                                 dist = "arbitrary", 
                                 library = "Dionysus",
                                 printProgress = FALSE, 
                                 location=TRUE)

          wasserstein_distance <- wasserstein(diag_this[["diagram"]], 
                                              diag_other[["diagram"]], 
                                              p = 1, 
                                              dimension = 1)

          similarity_matrix[i,j] <- wasserstein_distance
        }
      }
      else {
        similarity_matrix[i,j] <- 0
      }
    }
  }
  
  main_matrix <- similarity_matrix + t(similarity_matrix)
  return_matrix <- as.data.frame(main_matrix)
  colnames(return_matrix) <- collection
  rownames(return_matrix) <- collection
  return(return_matrix)
  
}

clustering_PD_bottleneck <- function(numLines, collection){
  similarity_matrix <- matrix(ncol = length(collection), nrow = length(collection))
  diag(similarity_matrix) <- 0
  for (i in 1:length(collection)){
    for (j in 1:length(collection)){
      if (j > i){
        book_DTM <- compute_DTM(collection[j],numLines)
        other_book_DTM <- compute_DTM(collection[i],numLines)
        if (!is.null(book_DTM) && !is.na(other_book_DTM)){  
          this_book <- make_distance_matrix(book_DTM)
          other_book <- make_distance_matrix(other_book_DTM)

          diag_this <- ripsDiag(X = this_book,
                                maxdimension = 1,
                                maxscale = 100,
                                dist = "arbitrary", 
                                library = "Dionysus",
                                printProgress = FALSE, 
                                location=TRUE)
          diag_other <- ripsDiag(X = other_book,
                                 maxdimension = 1,
                                 maxscale = 100,
                                 dist = "arbitrary", 
                                 library = "Dionysus",
                                 printProgress = FALSE, 
                                 location=TRUE)
                                 
          bottleDist <- bottleneck(diag_this[["diagram"]], 
                                   diag_other[["diagram"]], 
                                   dimension = 1)

          similarity_matrix[i,j] <- bottleDist
        }
      }
      else {
        similarity_matrix[i,j] <- 0
      }
    }
  }
  
  main_matrix <- similarity_matrix + t(similarity_matrix)
  return_matrix <- as.data.frame(main_matrix)
  colnames(return_matrix) <- collection
  rownames(return_matrix) <- collection
  return(return_matrix)
  
}


# --> Helper Functions

evaluate_clusters <- function(dataframe,i){
  return(row.names(subset(dataframe,widths.cluster == i)))
}

clustering_score <- function(dataframe){
  df <- data.frame(matrix(0L,nrow = length(unique(dataframe$widths.cluster)), ncol = 5))
  colnames(df) <- topics
  rownames(df) <- unique(dataframe$widths.cluster)
  for (i in rownames(df)){
    cluster_vector <- evaluate_clusters(dataframe,i)
    for (value in cluster_vector){
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

get_category <- function(ID){
  book <- gutenberg_metadata %>% filter(gutenberg_id == ID)
  return(book$gutenberg_bookshelf[1])
}

return_scores <- function(dataframe){
  clusters <- clustering_score(as.data.frame(dataframe$silinfo))
  df <- data.frame("medoidID" = row.names(dataframe$medoids))
  df <- cbind(df,clusters)
  df <- df %>% mutate("medoidCategory" = "temp")
  for (i in 1:nrow(df)){
    df$medoidCategory[i] <- get_category(df$medoidID[i])
  }
  return(df)
}
