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

gutenberg_filtered <- gutenberg_metadata %>% 
  filter(is.na(author)==FALSE) %>% 
  filter(is.na(title)==FALSE) %>%
  filter(is.na(gutenberg_bookshelf) == FALSE) %>% 
  filter(has_text == TRUE) %>%  
  filter(rights == "Public domain in the USA.") %>% 
  filter(language == "en")

View(gutenberg_filtered)
