library(wordcloud)
forCloud <- readLines("F:/EfUA/Risikobaseret tilsyn/Materiale til Magnus/Vigtige/Projektinitieringsdokument_tekst.txt")
forCloud <- trimws(forCloud)
forCloud <- do.call(paste, as.list(forCloud))
forCloud <- tolower(forCloud)
forCloud <- trimws(forCloud)
forCloud <- gsub("\t", "", forCloud)
forCloud2 <- strsplit(forCloud, "\\ ")[[1]]
forCloud2 <- gsub("\\,", "", forCloud2)
forCloud2 <- gsub("\\'", "", forCloud2)
forCloud2 <- gsub("\\.", "", forCloud2)
forCloud2 <- sub("^\\(", "", forCloud2)
forCloud2 <- sub("^\\-", "", forCloud2)
forCloud2 <- sub("\\)$", "", forCloud2)
forCloud2 <- sub("\\:$", "", forCloud2)
forCloud2 <- sub("\\;$", "", forCloud2)
forCloud2 <- forCloud2[!grepl("[[:digit:]]", forCloud2)]

index <- which(names(table(forCloud2)) == "acadre") - 1
exNames <- names(table(forCloud2))[1:index]

forCloud2 <- forCloud2[!(forCloud2 %in% exNames)]
forCloud2 <- sub("cpr-", "cpr-nummer", forCloud2)
forCloud2 <- sub("cpr-nummeret", "cpr-nummer", forCloud2)
forCloud2 <- sub("risikobaserede", "risikobaseret", forCloud2)


counts <- table(forCloud2)
counts <- counts[order(counts, decreasing = T)]

colfunc <- colorRampPalette(c("blue", "orange1"))
cols <-  colfunc(100)

set.seed(070484)
pdf("F:/EfUA/Risikobaseret tilsyn/Materiale til Magnus/R/Output/PID_wordcloud.pdf")
wordcloud(names(counts[1:100]), counts[1:100], colors = cols, random.color = T)
dev.off()