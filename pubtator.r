df<-  read.delim('cellline2pubtatorcentral.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

saveRDS(df, file='cellline2pubtator.Rda')

dfnew <- readRDS('cellline2pubtator.Rda')
