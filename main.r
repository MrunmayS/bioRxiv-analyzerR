library(stringr)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(NLP)

extract <- function(filename) {
  dataset = read.delim(filename, 
                       header = FALSE, 
                       sep = '\t', 
                       quote = '', 
                       dec = ' ', 
                       stringsAsFactors = FALSE)
  
  dataset <- as.data.frame(dataset)
  row_index <- seq_len(nrow(dataset)) %% 2
  details <- dataset[row_index == 1,]
  abstract <- dataset[row_index == 0,]
  abstract <- as.data.frame(abstract)
  details <- as.data.frame(details)
  
  doi <- extract_doi(details)
  author <- extract_author(details)
  title <- extract_title(details)
  
  article <- data.frame(doi, title, author, abstract)
  colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")
  
  return(article)
}

extract_doi <- function(details_col){
  pattern_doi <- "(\\w+):\\s(\\d+)[^ab  c](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
  doi <- str_match(details_col[, 1],pattern_doi)
  doi <- as.data.frame(doi[, 1])
  return(doi)
  
}

extract_author <- function(details_col) {
  pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
  author_name <- str_match(details_col[,1], pattern_author)
  author <- as.data.frame(author_name[, 1])
  return(author)
}

extract_title <- function(details_col){
  pattern_title <- "(?<=\")(.*?)(?=\")"
  title <- str_match(details_col[, 1], pattern_title)
  title <- as.data.frame(title[, 1])
  return(title)
  
}



