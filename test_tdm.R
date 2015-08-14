

library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),readerControl = list(reader = readReut21578XMLasPlain))

library(rbenchmark)

benchmark(tm::DocumentTermMatrix(reuters),
          Narrative::tdmGenerator(1, reuters))

docs <- unlist(lapply(reuters, function(x) x[[1]]))
Narrative::tdm_generator(docs,1)

temp <- Narrative::tdmGenerator(1,reuters)
sum(temp[1,])

# SystemRequirements: C++11

# wd<-"C://dev//Datalab//Narrative_work/"
# load(paste(wd,"Agency Text Mining//Data Matching//matched_materials.RData",sep=""))

docs <- unlist(lapply(corp.clean, function(x) x[[1]]))
Narrative::tdm_generator(docs[1:10],1)
temp <- Narrative::tdm_generator(docs[1:20],1)
mat <- tapply(temp$count, list(temp$doc, temp$term), sum, na.rm = T)
tdm <- tm::as.DocumentTermMatrix(mat, weighting = tm::weightTf)

N = 20
Narrative::tdmGenerator(corp.clean[1:N])

N = 300
benchmark(tm::DocumentTermMatrix(corp.clean[1:N]),
          Narrative::tdmGenerator(1, corp.clean[1:N]))


N = 300
benchmark(tm::DocumentTermMatrix(corp.clean[1:N], control = list(tokenize = BigramTokenizer)),
          Narrative::tdmGenerator(2, corp.clean[1:N]))



### BIGRAM TOKENIZER IN TM / NLP

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

benchmark(tm::DocumentTermMatrix(reuters, control = list(tokenize = BigramTokenizer)),
          Narrative::tdmGenerator(2, reuters))

## MIKE CODE

crttdm <- function(corpus){
  docs <- do.call(rbind,lapply(names(corpus), function(x) cbind(as.data.frame(table(strsplit(as.String(corpus[[x]][[1]])," "))), x)))
  docs$Var1 <- as.vector(docs$Var1)
  docs$x <- as.vector(docs$x)
  
  term_matrix <- tapply(docs$Freq, list(docs$x, docs$Var1), sum, na.rm = T)
  term_matrix[is.na(term_matrix)] <- 0
  as.DocumentTermMatrix(term_matrix, weighting = weightTf)
}



benchmark(crttdm(reuters), DocumentTermMatrix(reuters))



#### TM TESTING

tflist <- parallel::mclapply(unname(content(reuters)), termFreq)  # find term frequencies for each document
tflist <- lapply(tflist, function(y) y[y>0])  # removes class names

v <- unlist(tflist)
i <- names(v)

allTerms <- sort(unique(as.character(i)))

i <- match(i, allTerms)
j <- rep(seq_along(reuters), sapply(tflist, length))

as.TermDocumentMatrix(slam::simple_triplet_matrix(i = i, j = j, v = as.numeric(v), nrow = length(allTerms), ncol = length(reuters)), weighting = weightTf)



##


library(doParallel)

test_func <- function(corpus){
  doParallel::registerDoParallel()  # register parallel backend
  foreach(i = corpus, .options.snow=list(preschedule=TRUE)) %dopar% {
    Narrative::tdm_creator(Narrative::ngram_generator(i[[1]],2))
  }
}

docs <- test_func(reuters)
i <- do.call(c,lapply(docs, names))

ngrams <- lapply(reuters,function(x) Narrative::tdm_creator(Narrative::ngram_generator(x[[1]],1)))
ngrams2 <- parallel::mclapply(unname(content(reuters)), termFreq)

terms <- sort(unique(as.character(i)))
match(i, terms)
rep(seq_along(reuters), sapply(docs, length))

system.time(test_func(reuters))
system.time(parallel::mclapply(unname(content(reuters)), termFreq))

benchmark(test_func(reuters),
          parallel::mclapply(unname(content(reuters)), termFreq))

###
N <- 40
docs <- parallel::mclapply(unname(content(corp.clean[1:N])), termFreq)

i <- do.call(c,lapply(docs, names))
terms <- sort(unique(as.character(i)))

matches <- match(i, terms)

matches <- foreach(a = i, .combine = "c") %dopar% {
  match(a, terms)
}


system.time(matches <- foreach(a = i, .combine = "c") %do% {
  match(a, terms)
})
system.time(matches <- foreach(a = i, .combine = "c") %dopar% {
  match(a, terms)
})
system.time(match(i, terms))



### ngram generator test

library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),readerControl = list(reader = readReut21578XMLasPlain))

library(rbenchmark)

docs <- unlist(lapply(reuters, function(x) unname(content(x))))
ngrams <- Narrative::ngram_generator(docs, 1, delim = ' ')

generatetdm <- function(x){
  docs <- unlist(lapply(x, function(x) gsub("[\r\n]", " ", content(x))))
  ngrams <- Narrative::ngram_generator(docs, 1, delim = ' ')
  
  v <- unlist(ngrams)
  i <- names(v)
  
  allTerms <- sort(unique(as.character(i)))
  i <- match(i, allTerms)
  j <- rep(seq_along(x), sapply(ngrams, length))
  docs <- as.character(meta(x, "id", "local"))
  
  tdm <- slam::simple_triplet_matrix(i, j, as.numeric(v),nrow = length(allTerms), 
                                     ncol = length(x), dimnames = list(Terms = allTerms, Docs = docs))
  
  as.TermDocumentMatrix(tdm, weighting = weightTf)
}

benchmark(generatetdm(reuters),
          TermDocumentMatrix(reuters))

tdm.a <- generatetdm(reuters)
tdm.b <- TermDocumentMatrix(reuters,control=list(wordLengths=c(1,Inf)))
tdm.a
tdm.b

inspect(tdm.a[1:10,])
inspect(tdm.b[1:10,])

inspect(tdm.a[(nrow(tdm.a)-10):nrow(tdm.a),])
inspect(tdm.b[(nrow(tdm.b)-10):nrow(tdm.b),])

# wd<-"C://Users//324240//Desktop//Narrative_work/"
# load(paste(wd,"Agency Text Mining//Data Matching//matched_materials.RData",sep=""))

N <- 160
benchmark(generatetdm(corp.clean[1:N]),
          TermDocumentMatrix(corp.clean[1:N]))






