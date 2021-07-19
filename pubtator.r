df<-  read.delim('mutation2pubtatorcentral.sample.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

saveRDS(df, file='cellline2pubtator.Rda')


dfnew <- readRDS('cellline2pubtator.Rda')



library(stringr)
library(dplyr)
library(stringi)


pattern <- 