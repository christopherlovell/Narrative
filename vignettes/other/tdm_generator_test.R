## Tdm generator testing and benchmarking

pacman::p_unlock(lib.loc = pacman::p_path())  # remove libloc file that prevents Narrative compilation


library(tm)
library(RWeka)
library(rbenchmark)

# test corpus
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                     readerControl = list(reader = readReut21578XMLasPlain))


## test stuff ##
docs <- unlist(lapply(reuters,function(x) paste(as.character(x),collapse=" ")))

#Rcpp::sourceCpp("src/ngram_generator.cpp")
reshape2::acast(Narrative::tdm_generator(docs,2),term~doc,value.var = "count",fill=0)

head(reshape2:::melt.list(tdm_generator(docs,2)))
## END ##

length <- 4
func <- function(corpus) NGramTokenizer(corpus, Weka_control(min=length,max=length))

benchmark(Narrative::tdmGenerator(length,reuters),
          TermDocumentMatrix(reuters, control = list(tokenize = func)))

system.time(Narrative::tdmGenerator(length,reuters))
system.time(TermDocumentMatrix(reuters, control = list(tokenize = func)))

# agencies test corpus
wd<-"C://dev//Datalab//Narrative"
load(file=paste(wd,"//inst//extdata//Agencies//matched_materials.RData",sep=""))

test_corpus <- corp.clean[1:5]

length <- 1

benchmark(Narrative::tdmGenerator(length,test_corpus),
          TermDocumentMatrix(test_corpus, control = list(tokenize = func)))

system.time(Narrative::tdmGenerator(length,test_corpus))
system.time(TermDocumentMatrix(test_corpus, control = list(tokenize = func)))

## multiple lengths test

Narrative::tdmGenerator(c(1,2),test_corpus)


