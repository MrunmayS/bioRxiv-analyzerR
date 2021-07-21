df <- read.delim("mutation2pubtatorcentral.sample.txt",
    header = FALSE,
    sep = "\t",
    quote = "",
    dec = " ",
    stringsAsFactors = FALSE
)

saveRDS(df, file = "cellline2pubtator.Rda")


celline <- readRDS("cellline2pubtator.Rda")

mutation <- as.data.frame(df$V3)

pattern_mutation <- "(([a-z]{2})\\d+)||(([a-z]{1}\\.[^-\\s])(.*?)[\\s])"
mutation1 <- str_match(mutation[,1], pattern_mutation)



library(stringr)
library(dplyr)
library(stringi)


VCorpus <- VCorpus(VectorSource(df$Abstract))
VCorpus <- Corpus(DataframeSource(df$Abstract))
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