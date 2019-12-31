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



topics <- c("Children's Fiction", "Humor", "Science Fiction", "Adventure", "Biographies")
set.seed(471)

gutenberg_filtered <- gutenberg_metadata %>% 
  filter(is.na(author)==FALSE) %>% 
  filter(is.na(title)==FALSE) %>% 
  filter(has_text == TRUE) %>%  
  filter(rights == "Public domain in the USA.") %>% 
  filter(language == "en")

children <- isolate_IDs("Children's Fiction")
humor <- isolate_IDs("Humor")
scifi <- isolate_IDs("Science Fiction")
adventure <- isolate_IDs("Adventure")
biographies <- isolate_IDs("Biographies")

book_indexing <- c(length(children), length(humor), length(scifi), length(adventure), length(biographies))
min_index <- min(book_indexing)
#In order for the data to be roughly proportional across the categories, we find the minimum number of IDs and then sample that number for each category.

children <- sample(children, min_index, replace=FALSE, prob=NULL)
humor <- sample(humor, min_index, replace=FALSE, prob=NULL)
scifi <- sample(scifi, min_index, replace=FALSE, prob=NULL)
adventure <- sample(adventure, min_index, replace=FALSE, prob=NULL)
biographies <- sample(biographies, min_index, replace=FALSE, prob=NULL)

#vector with IDs of all relevant books.
book_list <- c(children, humor, scifi, adventure, biographies)

#-------> Functions
isolate_IDs <- function(category){
  subset_bookshelf <- gutenberg_filtered[grep(category, gutenberg_filtered$gutenberg_bookshelf),]
  subset_bookshelf <- as.data.frame(subset_bookshelf)
  subset_ids <- subset_bookshelf$gutenberg_id
  print(subset_ids)
  return(subset_ids)
}

compute_DTM <- function(IDNum, lineLimit){
  book <- gutenberg_download(IDNum, strip=TRUE)
  book <- transmute(book, gutenberg_id = gutenberg_id, text = iconv(book$text,"UTF-8","UTF-8",sub=''))
  book <- unnest_tokens(book,input="text",output="Paragraph",token="paragraphs")
  book <- book[50:(50+lineLimit),]
  book <- book %>% filter(!is.na(book$Paragraph))
  book <- VCorpus(VectorSource(book$Paragraph))
  book <- tm_map(book, stripWhitespace)
  book <- tm_map(book, content_transformer(tolower))
  book <- tm_map(book, removePunctuation)
  book <- tm_map(book, removeWords, stopwords("english"))
  book <- tm_map(book, stemDocument, language = "english")  
  dtm <- DocumentTermMatrix(book)
  dtm <- weightTfIdf(dtm)
  dtm <- removeSparseTerms(dtm, 0.95)
  dtm <- filter_documents(dtm, min=0.0000000000000001)
  
  if ((dim(dtm)[1]) > 1 && dim(dtm)[2] > 1){
    dtmLSA <- lsa(dtm, dims=dimcalc_share())$tk
    return(dtmLSA)
  } else {
    return(NULL)
  }
}

make_distance_matrix <- function(distance_matrix){
  cosine_dist_matrix <- cosSparse(distance_matrix)
  diag(cosine_dist_matrix) <- 0
  for(row in 2:nrow(cosine_dist_matrix)) {
    cosine_dist_matrix[row, row-1] <- 0
    cosine_dist_matrix[row-1, row] <- 0 
  }
  for (row in 1:nrow(cosine_dist_matrix)) {
    for(col in 1:nrow(cosine_dist_matrix)) {
      cosine_dist_matrix[row,col] = abs(cosine_dist_matrix[row,col])*1000000000000000000
    }
  }
  return(as.matrix(cosine_dist_matrix))
}

main <- function(num_lines){
  main_data_frame <- data.frame(matrix(ncol = 8, nrow = 0))
  columns <- c("bookID", "num_holes", "category", "avg_birth_loc", "birth_loc_stdev", "avg_death_loc", "death_loc_stdev", "avg_length_life")
  colnames(main_data_frame) <- columns
  
  for (category in topics){
    book_IDs <- isolate_IDs(category)
    for (value in book_IDs) {
      book_DTM <- compute_DTM(value,num_lines)
      print(value)
      if (!is.null(book_DTM)){  
        book_distance_matrix <- make_distance_matrix(book_DTM)
        Diag <- ripsDiag(X = book_distance_matrix,
                         maxdimension = 1,
                         maxscale = 100,
                         dist = "arbitrary", 
                         library = "Dionysus",
                         printProgress = FALSE, 
                         location=TRUE)
        
        num_holes <- summary.diagram(Diag[["diagram"]])$n
        #print(num_holes)
        temp_data <- data.frame(value, num_holes, category, mean(Diag$birthLocation), sd(Diag$birthLocation), mean(Diag$deathLocation), sd(Diag$deathLocation), mean(Diag$deathLocation - Diag$birthLocation))
        names(temp_data) <- c("bookID", "num_holes", "category", "avg_birth_loc", "birth_loc_stdev", "avg_death_loc", "death_loc_stdev", "avg_length_life")
        main_data_frame <- rbind(main_data_frame,temp_data)
      }
      else {
        #print("0 here")
        temp_data <- data.frame(value, 0, category, 0, 0, 0, 0, 0)
        names(temp_data) <- c("bookID", "num_holes", "category", "avg_birth_loc", "birth_loc_stdev", "avg_death_loc", "death_loc_stdev", "avg_length_life")
        main_data_frame <- rbind(main_data_frame,temp_data)
      }
    }
  }
  return(main_data_frame)
}

compare_statistics <- function(data_frame){
  df <- data.frame(matrix(0, ncol = 5, nrow = 5))
  colnames(df) <- topics
  rownames(df) <- topics
  for (row in rownames(df)){
    for (col in colnames(df)){
      genre_row_table <- data_frame %>% filter(category == row)
      genre_col_table <- data_frame %>% filter(category == col)
      significance <- t.test(genre_row_table$avg_death_loc, genre_col_table$avg_death_loc)$p.value
      df[row,col] <- significance
    }
  }
  return(df)
}
