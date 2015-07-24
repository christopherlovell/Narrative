#' Narrative corpus manipulation functions
#' 
#' @name Narrative
#' @docType package
NULL

#'Display startup message
.onAttach <- function(libname,pkgname) {
  packageStartupMessage("Narrative v0.2")
}

#'Add field to meta data
#'
#'Add field to meta data
#'@param corpus corpus object
#'@param vector vector of values to add to corpus. must be of equal length to corpus
#'@param tag meta data tag where values are to be added
addToMetaData <- function(corpus,vector,tag){
  
  if(length(corpus) != length(vector)){
    stop("Please provide a vector of equal length to your corpus")
  }
  
  i<-1
  while(i <= length(vector)){
    NLP::meta(corpus[[i]],tag,type="indexed")<-vector[[i]]
    i<-i+1
  }
  
  return(corpus)
}

#' Character Count
#' 
#' Count the number of characters in each document in a given corpus, and return as a vector
#' @param corp Corpus object
characterCount <- function(corp){
  return(unlist(lapply(corp, function(x) sum(base::nchar(x[[1]])))))
}

#' Word Count
#' 
#' Count the number of words in each document in a given corpus, and return as a vector
#' @param x DocumentTermMatrix or TermDocumentMatrix object, from the tm package.
wordCount <- function(x){
  if(class(x)[1]=="DocumentTermMatrix"){
    return(slam::row_sums(x))
  }else if(class(x)[1]=="TermDocumentMatrix"){
    return(slam::col_sums(x))
  }else{
    stop("x is not a document term matrix or a term document matrix")
  }
}

#'Annotate corpus
#'
#'Apply annotations to documents.
#'@param corpus tm Corpus object
#'@param annotator specify "sentence" or "word" tokenisation, or provide your own annotator function
narrativeAnnotator <- function(corpus, annotator="sentence", metadata = T){
  if(annotator=="sentence"){
    annotator.function=openNLP::Maxent_Sent_Token_Annotator
  }else if(annotator=="word"){
    annotator.function=openNLP::Maxent_Word_Token_Annotator
  }else{
    annotator.function = annotator
  }
  
  annotator <- function(text, lang = "en"){
    text <- as.String(text)
    sentences <- NLP::annotate(text, annotator.function(language = "en"))
    if(length(sentences) > 0){
      return(text[sentences])  
    }else{
      return(text)
    }
  }
  
  text <- lapply(corpus, function(x) x$content)
  docs.annotated <- lapply(text, annotator)
  corpus.annotated <- tm::Corpus(tm::VectorSource(docs.annotated))
    
  if(metadata){
    md <- Narrative::extractMetaData(corpus = corpus)
    corpus.annotated <- Narrative::loadMetaData(corpus.annotated, metadata = md)
  }
  
  return(corpus.annotated)
}

#'Annotated corpus search
#'
#'@param corpus Corpus object
#'@param terms character vector of search terms you wish to search for 
#'@param width neighbouring sentences to be included
annotatorSearch<-function(annotated.corpus, terms, width = 0){  
  # regex matching sentences and return index
  matches <- lapply(annotated.corpus, function(x) which(base::grepl(terms, x[[1]])))
  
  # add width to matched sentence index
  if(width>0){
    matches <- lapply(matches,function(x) append(x, c(x + width, x - width)))
    matches <- lapply(matches,function(x) unique(x))  # remove duplicate sentences
  }
    
  # grab corresponding sentences in to nested list
  matched.sentences <- base::data.frame()
  i <- 1
  while(i <= length(annotated.corpus)){
    matched.sentences <- append(matched.sentences, list(annotated.corpus[[i]][[1]][matches[[i]]]))
    i <- i + 1
  }
  
  return(matched.sentences)
}

#' Financial Narratives Sentiment
#' 
#' #Calculate sentiment of documents in a corpus using the sentiment calculator from Financial Narratives.
#' @param corpus corpus object
#' @param dict.positive Text document containing positive words to be matched
#' @param dict.negative Text document containing negative words to be matched
#' @param normalisation.meta document level corpus meta data field containing value to normalise over.
#corpusSentiment <- function(corpus,dict.positive,dict.negative,normalisation.meta=NULL){
corpusSentiment <- function(tdm, dict.positive, dict.negative, normalisation.meta = NULL){
  
  if(class(tdm)[1]!="TermDocumentMatrix"){
    stop("Please provide a valid TermDocumentMatrix object to tdm")
  }else if(class(dict.positive) != "character" | class(dict.negative) != "character"){
    stop("Please provide valid character vectors to dict.positive and dict.negative")
  }
  
  if(is.null(normalisation.meta)){  # check for normalisation.meta. If NULL, warn user that we're going to calculate word counts by default and assign to meta data
    readline(prompt="No normalisation meta data provided. Narrative will automatically generate word counts for each document\
             to normalise against. If you wish to provide you own normalisation field, exit now. Otherwise, press [enter] to continue")
    normalisation.meta <- Narrative::wordCount(t(tdm))
  }
  
  tdm.terms <- tm::Terms(tdm)
  
  dict.positive.filter <- dict.positive[dict.positive %in% tdm.terms]
  dict.negative.filter <- dict.negative[dict.negative %in% tdm.terms]
  
  tdm.positive <- tdm[dict.positive.filter,]
  tdm.negative <- tdm[dict.negative.filter,]
  
  positive.vector <- base::colSums(as.matrix(tdm.positive))
  negative.vector <- base::colSums(as.matrix(tdm.negative))
  
  sentiment <- ((positive.vector-negative.vector) / normalisation.meta)
  
  return(base::cbind(sentiment, positive.vector, negative.vector))
}

#'Generate Term Document Matrices 
#'
#'Generate appropriate order term document matrices for a given list of terms or numeric values.
#'@param length character vector of terms, or a numeric vector of lengths, for which to create corresponding term document matrices
#'@param corpus corpus object
tdmGenerator <- function(length, corpus, control_params = c()){
  if(class(length)=="character"){
    lengths<-unique(unlist(lapply(length,function(x) length( unlist(base::strsplit(gsub(' {2,}',' ',x),' ') )))))
  }else if(class(length)=="numeric"){
    lengths<-length
  }else{
    stop("please provide a character vector of terms, or a numeric vector of lengths.")
  }

  tdm<-lapply(unique(lengths),.nGramTokenizerGenerator,corpus=corpus,control_params=control_params)
  
  return(as.TermDocumentMatrix(do.call(base::rbind,tdm),weighting=tm::weightTf,control = control_params))
}

#'(private) N-gram Term-Document/Document-Term Matrix Generator
#'
#'Generates a TermDocumentMatrix or a DocumentTermMatrix object for a given corpus at ngram length
#'@param length ngram length to create matrix for
#'@param corpus corpus object to transform
.nGramTokenizerGenerator <- function(length, corpus, control_params = c()){
  docs <- unlist(lapply(corpus,function(x) paste(as.character(x), collapse=" ")))
  reshape2::acast(.tdm_generator(docs,length), term~doc, value.var = "count", fill=0)  # C++ tdm generator function call 
}

#'Sort a Term Document Matrix by term weight
#'
#'Returns a nested list of terms for each document, sorted by weight.
#'@param x document term or term document matrix object to be weighted
weightSort <- function(x){
  if(class(x)[1]=="DocumentTermMatrix"){
    x <- t(x)
  }else if(class(x)[1]=="TermDocumentMatrix"){
    x <- x
  }else{
    stop("x is not a document term matrix or a term document matrix")
  }
  
  docs<-vector("list")
  i<-1
  while(i<=ncol(x)){  # return top weighted terms for a given doc
    docs[[i]]<-as.matrix(x[,i])[order(as.matrix(x[,i]),decreasing = T),]
    i<-i+1
  }
  names(docs)<-dimnames(x)$Docs
  docs
}

#'Logical concatenation
#'
#'Concatenates rows of a search result matrix  according to a logical condition.
#'@param search matrix of search results. Each column of results for a different term.
#'@param logic value
#'#'\itemize{
##'  \item{AND - if all terms matched, return count}
##'  \item{OR - return all term counts}
##' }
#'@param normalisation either a character vector indicating to return a logical vector of matches, 
#'or a function specifying the normalisation of matching values. For example, if normalisation = sum, for an "OR"
#'logical match all matches of each search term in each document will be summed and returned. If normalisation = min, 
#'for an "AND" logical match the number of pairs of terms is returned.
logicalMatch <- function(search, logic, FUN = NULL){
  if(logic=="AND"){
    logic.function=all
  }else if(logic=="OR"){
    logic.function=any
  }
  matches.logical<-apply(search>0,1,logic.function)
  
  if(is.null(FUN)){
    return(matches.logical)
  }
  
  and.match<-vector(length=nrow(search))
  i<-1
  while(i<=length(matches.logical)){
    if(matches.logical[i]){
      and.match[i]<-FUN(search[i,])
    }else{
      and.match[i]<-0
    }
    i<-i+1
  }
  names(and.match)<-names(matches.logical)
  return(and.match)
}


