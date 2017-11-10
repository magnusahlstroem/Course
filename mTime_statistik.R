burwick <- medline("C:/Users/MGAH/Downloads/pubmed_result.txt")
abstracts <- lapply(burwick, function(x) list(EDAT = x$EDAT, PMID = x$PMID, AB = x$AB))

abstracts <- abstracts[sapply(abstracts, function(x) !is.null(x[["AB"]]))]
roughscreen <- abstracts[sapply(abstracts, function(x) grepl("[Ss][Aa][Ff][Ee][Tt][Yy]", x$AB))]

mTimeOkt <- readLines("H:/mTime_stastistik/Oktober.txt")
mTimeOkt <- trimws(mTimeOkt)
mTimeOkt <- mTimeOkt[grepl("^Difference \\(.*", mTimeOkt)]
mTimeOkt <- sub("^.*\\)\\ ", "", mTimeOkt)
mTimeOkt <- gsub("\\,", "\\.", mTimeOkt)
mTimeOkt <- strsplit(mTimeOkt, "\\ ")
mTimeOkt <- unlist(mTimeOkt)
mTimeOkt <- as.numeric(mTimeOkt)[-length(mTimeOkt)]
mTimeOkt <- mTimeOkt[-which(mTimeOkt == -7.4)]