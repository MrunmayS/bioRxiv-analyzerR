library(stringr)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(NLP)
library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(pals)
library(Rcpp)
library(igraph)

extract <- function(filename) {
  dataset <- read.delim(filename,
    header = FALSE,
    sep = "\t",
    quote = "",
    dec = " ",
    stringsAsFactors = FALSE
  )

  dataset <- as.data.frame(dataset)
  row_index <- seq_len(nrow(dataset)) %% 2
  details <- dataset[row_index == 1, ]
  abstract <- dataset[row_index == 0, ]
  abstract <- as.data.frame(abstract)
  details <- as.data.frame(details)

  doi <- extract_doi(details)
  author <- extract_author(details)
  title <- extract_title(details)

  article <- data.frame(doi, title, author, abstract)
  colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")

  return(article)
}

extract_doi <- function(details_col) {
  pattern_doi <- "(\\w+):\\s(\\d+)[^ab  c](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
  doi <- str_match(details_col[, 1], pattern_doi)
  doi <- as.data.frame(doi[, 1])
  return(doi)
}

extract_author <- function(details_col) {
  pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"
  author_name <- str_match(details_col[, 1], pattern_author)
  author <- as.data.frame(author_name[, 1])
  return(author)
}

extract_title <- function(details_col) {
  pattern_title <- "(?<=\")(.*?)(?=\")"
  title <- str_match(details_col[, 1], pattern_title)
  title <- as.data.frame(title[, 1])
  return(title)
}

extract_mutation <- function(abstract) {
  pattern_mutation <- "([a-z]{2})\\d+"
  mutation <- str_match(abstract[, 1], pattern_mutation)
  mutation <- as.data.frame(mutation[, 1])
}


dfn <- extract("citations.txt")
abstrct <- dfn$Abstract
abstrct <- as.data.frame(abstrct)
load("data_common_words.RData")


calculatedtm <- function(dataframe) {
  corpus <- VCorpus(VectorSource(dataframe))
  tospace <- content_transformer(
    function(x, pattern) {
      gsub(pattern, " ", x)
    }
  )
  processedcorpus <- tm_map(corpus, tospace, "/")
  processedcorpus <- tm_map(processedcorpus, tospace, "@")
  processedcorpus <- tm_map(processedcorpus, tospace, "#")


  processedcorpus <- tm_map(processedcorpus, content_transformer(tolower))
  processedcorpus <- tm_map(processedcorpus, removeNumbers)
  processedcorpus <- tm_map(processedcorpus, removeWords, stopwords("english"))
  processedcorpus <- tm_map(processedcorpus, removeWords, data_common_words)
  processedcorpus <- tm_map(processedcorpus, stripWhitespace)


  dtm <- TermDocumentMatrix(processedcorpus)
  return(dtm)
}

dtm <- calculatedtm(dfn$Abstract)

calculatefreq <- function(dtm) {
  dtm_matrix <- as.matrix(dtm)
  v <- sort(rowSums(dtm_matrix), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)

  return(d)
}
freqtable <- calculatefreq(dtm)

View(freqtable)
makewordcloud <- function(x) {
  wordcloud(
    words = x$word,
    freq = x$freq,
    min.freq = 1,
    max.words = 250,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}

makewordcloud(freqtable)

makebarplot <- function(x) {
  barplot(x[1:10, ]$freq,
    names.arg = x[1:10, ]$word,
    col = "lightgreen", main = "Top 10 most frequent words",
    ylab = "Word frequencies", xlab = "Words"
  )
}
makebarplot(freqtable)


maketopicmodel <- function(x){
  docs <- VCorpus(VectorSource(x))   
  
  # Text transformation
  toSpace <- content_transformer(
    function (x, pattern)
      gsub(pattern, " ", x))
  docs1 <- tm_map(docs, toSpace, "/")
  docs1 <- tm_map(docs, toSpace, "@")
  docs1 <- tm_map(docs, toSpace, "#")
  docs1 <- tm_map(docs1, content_transformer(tolower))
  docs1 <- tm_map(docs1, removeNumbers)
  docs1 <- tm_map(docs1, removeWords, stopwords("english"))
  docs1 <- tm_map(docs1, removeWords, data_common_words)
  docs1 <- tm_map(docs1, stripWhitespace)
  
  DTM <- DocumentTermMatrix(docs1, control = list(bounds = list(global = c(5, Inf))))
  
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  
  K <- 10
  topicModel <- LDA(DTM, K)
  tmResult <- posterior(topicModel)
  attributes(tmResult)
  nTerms(DTM)   
  betas <- tmResult$terms  
  dim(beta)  
  thetas <- tmResult$topics 
  
  rowSums(thetas)[1:10] 
  terms(topicModel, 10)
  
  topics <- tidy(topicModel, matrix = "beta")
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
  
  
}

createtopics <- function(x, K= 10){
  docs <- VCorpus(VectorSource(x))   
  
  # Text transformation
  toSpace <- content_transformer(
    function (x, pattern)
      gsub(pattern, " ", x))
  docs1 <- tm_map(docs, toSpace, "/")
  docs1 <- tm_map(docs, toSpace, "@")
  docs1 <- tm_map(docs, toSpace, "#")
  docs1 <- tm_map(docs1, content_transformer(tolower))
  docs1 <- tm_map(docs1, removeNumbers)
  docs1 <- tm_map(docs1, removeWords, stopwords("english"))
  docs1 <- tm_map(docs1, removeWords, data_common_words)
  docs1 <- tm_map(docs1, stripWhitespace)
  
  DTM <- DocumentTermMatrix(docs1, control = list(bounds = list(global = c(5, Inf))))
  
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  
  topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
  tmResult <- posterior(topicModel)
  attributes(tmResult)
  nTerms(DTM)   
  betas <- tmResult$terms  
  dim(beta)  
  thetas <- tmResult$topics 
  
  rowSums(thetas)[1:10] 
  terms(topicModel, 10)
  
  topics <- tidy(topicModel, matrix = "beta")
  
}

text_link <- function(topics){
  my_adj_list <- topics %>% filter(beta > 0.025)
  names(my_adj_list) <- c('from', 'to', 'weight')
  class(my_adj_list)
  dim(my_adj_list)
  net <- graph.data.frame(my_adj_list, directed = FALSE)
  orig_mar <- par()$mar
  par(mar=rep(.1, 4))
  
  plot(net, layout = layout_components(net), edge.width = E(net)$weight)
}
topics <- createtopics(dfn$Abstract)
text_link(topics)
