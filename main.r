library(stringr)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(NLP)
<<<<<<< HEAD
library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(pals)
library(dplyr)
library(stringr)
library(dplyr)
library(stringi)
=======
>>>>>>> parent of 173cce9 (tried making topic models)

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
  pattern_title <- "(?<-\")(.*?)(?=\")"
  title <- str_match(details_col[, 1], pattern_title)
  title <- as.data.frame(title[, 1])
  return(title)
  
}

<<<<<<< HEAD
dfn <-extract('list.txt')

load("data_common_words.RData")
 
calculateDTM <- function(dataframe) {
  Corpus = VCorpus(VectorSource(dataframe))   
  
  toSpace = content_transformer(
    function (x, pattern)
      gsub(pattern, " ", x))
  processedCorpus <- tm_map(Corpus, toSpace, "/")
  processedCorpus <- tm_map(Corpus, toSpace, "@")
  processedCorpus <- tm_map(Corpus, toSpace, "#")
  
  processedCorpus <- tm_map(processedCorpus, content_transformer(tolower))
  processedCorpus <- tm_map(processedCorpus, removeNumbers)
  processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("english"))
  processedCorpus <- tm_map(processedCorpus, removeWords, data_common_words)
  processedCorpus <- tm_map(processedCorpus, stripWhitespace)
  
  
  dtm <- TermDocumentMatrix(processedCorpus)
  return(dtm)
}

dtm <- calculateDTM(df$abstract)

VCorpus <- VCorpus(VectorSource(df$Abstract))   
VCorpus <- Corpus(DataframeSource(df$Abstract))
processedCorpus <- tm_map(VCorpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes <- TRUE)
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("english"))
processedCorpus <- tm_map(processedCorpus, removeWords, data_common_words)
processedCorpus = tm_map(processedCorpus, stripWhitespace)
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)


sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

K <- 10
topicModel <- LDA(DTM, K)
  

tmResult <- posterior(topicModel)
attributes(tmResult)
nTerms(DTM)   
betas <- tmResult$terms  
dim(beta)  
thetas <- tmResult$topics 

rowSums(theta)[1:10] 
terms(topicModel, 10)

topics <- tidy(topicModel, matrix = "beta")
topics
  
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()  
=======


>>>>>>> parent of 173cce9 (tried making topic models)
