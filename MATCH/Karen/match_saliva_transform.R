setwd("R:/")
result=read.table("merged_saliva.txt", header=TRUE, stringsAsFactors = FALSE, sep = "\t")

result$date=sapply(strsplit(result$timestamp, " "), head, 1)
