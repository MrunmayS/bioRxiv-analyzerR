library(stringr)
library(dplyr)
library(stringi)

dataset = read.delim('list.txt', 
                     header = FALSE, 
                     sep = '\t', 
                     quote = '', 
                     dec = ' ', 
                     stringsAsFactors = FALSE)

dataset <- as.data.frame(dataset)
odd_row <- seq_len(nrow(dataset2)) %% 2
odd_col <- dataset2[odd_row == 1,]
abstract1 <- dataset2[odd_row == 0,]
abstract <- as.data.frame(abstract1)
odd <- as.data.frame(odd_col)