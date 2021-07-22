dataset = read.delim('citations.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

library(stringr)
library(dplyr)
library(stringi)

dataset2 <- as.data.frame(dataset)
odd_row <- seq_len(nrow(dataset2)) %% 2
odd_col <- dataset2[odd_row == 1,]
abstract1 <- dataset2[odd_row == 0,]
abstract <- as.data.frame(abstract1)
odd <- as.data.frame(odd_col)

pattern_doi <- "(\\w+):\\s(\\d+)[^ab  c](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)[^abc](\\d+)"
doi <- str_match(odd[, 1],pattern_doi)
doi2 <- as.data.frame(doi[, 1])

pattern_author <- "([A-Z][a-z]+)\\s([A-Z]*(\\.*))\\w+\\s(\\w+\\s*\\w*)(\\..)"

author_name <- str_match(odd[,1], pattern_author)

author <- as.data.frame(author_name[, 1])

pattern_title <- "(?<=\")(.*?)(?=\")"
title <- str_match(odd[, 1], pattern_title)
title <- as.data.frame(title[, 1])

article <- data.frame(doi2, title, author, abstract)
colnames(article) <- c("DOI Link", "Title", "Author", "Abstract")
View(article)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(pals)

docs = VCorpus(VectorSource(article$Abstract))   

toSpace = content_transformer(
  function (x, pattern)
    gsub(pattern, " ", x))
docs1 = tm_map(docs, toSpace, "/")
docs1 = tm_map(docs, toSpace, "@")
docs1 = tm_map(docs, toSpace, "#")

docs1 = tm_map(docs1, content_transformer(tolower))
docs1 = tm_map(docs1, removeNumbers)
docs1 = tm_map(docs1, removeWords, stopwords("english"))
docs1 = tm_map(docs1, removeWords, data_common_words)
docs1 = tm_map(docs1, stripWhitespace)


dtm = TermDocumentMatrix(docs1)
m = as.matrix(dtm)
v = sort(rowSums(m), 
         decreasing = TRUE)
d = data.frame(word = names(v),
               freq = v)
head(d, 10)
common_words = load("data_common_words.RData")

wordcloud(words = d$word, 
          freq = d$freq,
          min.freq = 1, 
          max.words = 250,
          random.order = FALSE,
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))


barplot(d[1:10, ]$freq, names.arg = d[1:10, ]$word,
        col ="lightgreen", main ="Top 10 most frequent words",
        ylab = "Word frequencies", xlab="Words")
library('medrxivr')

x= mx_api_content(
  from_date = "2020-06-01",
  to_date = as.character(Sys.Date()),
  clean = TRUE,
  server = "medrxiv",
  include_info = FALSE
)
saveRDS(x, file='list.Rda')

newdata <- readRDS('list.Rda')


VCorpus <- VCorpus(VectorSource(dataset$Abstract))
VCorpus <- Corpus(DataframeSource(datasetf$Abstract))
processedCorpus <- tm_map(VCorpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes <- TRUE)
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("english"))
processedCorpus <- tm_map(processedCorpus, removeWords, data_common_words)
processedCorpus <- tm_map(processedCorpus, stripWhitespace)
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)


sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

K <- 10
topicModel <- LDA(DTM, K)


tmresult <- posterior(topicModel)
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
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()
  
