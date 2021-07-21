library('medrxivr')
library('quanteda')
library('readtext')

ndata <- mx_api_content(
  from_date = "2020-07-13",
  to_date = as.character(Sys.Date()),
  clean = TRUE,
  server = "medrxiv",
  include_info = FALSE
)
load(data_common_words.RData)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(NLP)
docs = VCorpus(VectorSource(ndata$abstract))   

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