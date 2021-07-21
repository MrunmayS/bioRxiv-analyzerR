# bioRxiv-analyzerR

### The package aims to extract data from the citations downloaded from the citations manager of bioRxiv, which is a pre-print server for biology research

## Prerequisites

The following packages must be installed before hand for the program to work

- stringr
- dplyr
- stringi
- tm
- SnowballC
- wordcloud
- RColorBrewer
- NLP
- topicmodels
- tidytext
- reshape2
- ggplot2
- pals
- Rcpp
- igraph

This could be done by running:

<pre>
<code>install.packages("stringr", "dplyr", "stringi", "tm" , "SnowballC", "wordcloud", "RColorBrewer", "NLP", "topicmodels", "tidytext", "reshape2", "ggplot2", "Rcpp", "igraph")</code>

</pre>

# To Extract the Data from the text file

Use the <strong>extract</strong>function by passing in the name of the file in form of the string.

<pre> <code>
df <- extract("citations.txt")
</code> </pre>

It will save everything into the assigned variable

# To calculate DTM(Document-Term Matrix)

<pre> <code>
dtm <- calculatedtm(df$Abstract)
</code> </pre>

Pass in the Abstract coloumn from the dataframe you created to calculate the DTM with common words removed.

# To calculate Frequency Table

<pre> <code>
freqtable <- calculatefreq(dtm)
</code> </pre>

To make a frequency table pass in the dtm found before and into the function

# To make a word cloud and bar plot

Frequncy table made in the previous function has been used for this

<pre> <code>
makewordcloud(freqtable)

makebarplot(freqtable)
</code> </pre>

# To make topic model graph

Just pass in the abstract coloumn and the function will do the job with topics = 10

<pre> <code>
maketopicmodel(df$Abstract)
</code> </pre>

# To create topics

To create K number of topics pass in

<pre> <code>
topics <- createtopics(dfn$Abstract, K)
</code> </pre>

K is set to 10 by default

# To make links using topics

Pass in the topics, in the funtion to make network of linked topics

<pre> <code>
text_link(topics)
</code> </pre>
