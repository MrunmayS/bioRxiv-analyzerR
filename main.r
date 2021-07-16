library(stringr)
library(dplyr)
library(stringi)

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
  
  pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
  pattern_doi <- "(\\w+):\\s(\\d+)[^ab  c](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
  pattern_title <- "(?<=\")(.*?)(?=\")"
  
  doi <- str_match(details[, 1],pattern_doi)
  doi <- as.data.frame(doi[, 1])
  
  author_name <- str_match(details[,1], pattern_author)
  author <- as.data.frame(author_name[, 1])
  
  title <- str_match(details[, 1], pattern_title)
  title <- as.data.frame(title[, 1])
  
  article <- data.frame(doi, title, author, abstract)
  colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")
  
  return(article)
}

df <- extract('list.txt')

