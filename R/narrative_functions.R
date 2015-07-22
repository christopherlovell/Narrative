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
narrativeAnnotator <- function(corpus,annotator="sentence",meta=T){
  if(annotator=="sentence"){
    annotator.function=openNLP::Maxent_Sent_Token_Annotator
  }else if(annotator=="word"){
    annotator.function=openNLP::Maxent_Word_Token_Annotator
  }else{
    stop("Please provide a valid annotator type")
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
  corpus.annotated <- tm::Corpus(VectorSource(docs.annotated))
    
  if(meta){
    for(name in names(NLP::meta(corpus[[1]]))){
      corpus.annotated <- Narrative::addToMetaData(corpus.annotated, vector = NLP::meta(corpus,tag=name), tag = name)
    }
  }
  
  return(corpus.annotated)
}

#'Search Context Analyser
#'
contextAnalyser<-function(corpus, terms, search.vector = NA, width=0){  
  # filter out docs that return search
  if(!is.na(search.vector)){
    corpus.search<-corpus[search.vector>0]  
  }else{
    corpus.search <- corpus
  }
  
  # regex matching sentences and return index
  matches<-lapply(corpus.search,function(x) which(base::grepl(terms,x[[1]])))
  
  # add width to matched sentence index
  if(width>0){
    matches<-lapply(matches,function(x) append(x,c(x+width,x-width)))
    matches<-lapply(matches,function(x) unique(x))
  }
    
  # grab corresponding sentences in to nested list
  matched.sentences<-base::data.frame()
  i<-1
  while(i<length(corpus.search)){
    matched.sentences<-append(matched.sentences,list(corpus.search[[i]][[1]][matches[[i]]]))
    i<-i+1
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
corpusSentiment <- function(tdm, corpus, dict.positive, dict.negative, normalisation.meta = NULL){
  
  if(class(corpus)[2]!="Corpus"){
    stop("Please provide a valid Corpus object to corpus")
  }else if(class(tdm)[1]!="TermDocumentMatrix"){
    stop("Please provide a valid TermDocumentMatrix object to tdm")
  }else if(class(dict.positive)!="character" | class(dict.negative)!="character"){
    stop("Please provide valid character vectors to dict.positive and dict.negative")
  }
  
  # check for normalisation.meta. If NULL, warn user that we're going to calculate word counts by default and assign to meta data
  if(is.null(normalisation.meta)){
    readline(prompt="No normalisation meta data provided. Narrative will automatically generate word counts for each document\
             to normalise against. If you wish to provide you own normalisation field, exit now. Otherwise, press [enter] to continue")
    v.word<-Narrative::wordCount(t(tdm))
    corpus<-Narrative::addToMetaData(corpus,v.word,"count.word")
    normalisation.meta="count.word"
    rm(v.word)
  }else if(!(normalisation.meta %in% names(meta(corpus[[1]])) )){
    # if exists, check that it exists in Corpus. If not, stop.
    stop("Please provide a normalisation meta data field that exists in the corpus")
  }  
  
  tdm.terms<-tm::Terms(tdm)
  
  dict.positive.filter<-dict.positive[dict.positive %in% tdm.terms]
  dict.negative.filter<-dict.negative[dict.negative %in% tdm.terms]
  
  tdm.positive<-tdm[dict.positive.filter,]
  tdm.negative<-tdm[dict.negative.filter,]
  
  positive.vector<-base::colSums(as.matrix(tdm.positive))
  negative.vector<-base::colSums(as.matrix(tdm.negative))
  
  sentiment<-((positive.vector-negative.vector)/unlist(NLP::meta(corpus,tag=normalisation.meta)))
  
  return(base::cbind(sentiment,positive.vector,negative.vector))
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
  reshape2::acast(tdm_generator(docs,length), term~doc, value.var = "count", fill=0)  # C++ tdm generator function call 
}

#'Corpus Proportion
#'
#'Given a matrix of frequency scores return the proportion of documents that matched each term 
#'in the matrix.
#'@param mat matrix of frequency scores
corpusProportion <- function(mat){
  apply(mat,2,function(x){  # loop over term count matrix and return matched documents as a proportion of document count
    tab<-as.data.frame(table(x))  # frequency counts as data frame
    sum(tab[tab$x!=0,]$Freq)/sum(tab$Freq)  # frequency counts greater than zero over number of documents
  })
}

#'Weight a Term Document Matrix
#'
#'Returns a nested list of terms for each document, sorted by weight.
#'@param corpus corpus object to be weighted
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

#'Aggregated Time Series Objects by Meta Data
#'
#'Returns an xts time series object aggregated over the specified time window.
#'NAs are implicitly removed during creation of the xts object
#'@param corpus corpus to plot over
#'@param meta_field document level meta data field over which to aggregate
#'@param meta_time document level meta data field that defines time
#'@param time_aggregate time resolution to aggregate over. Can be one of the following:
#'\itemize{
##'  \item{none - just returns un aggregated data, with NAs filtered}
##'  \item{daily}
##'  \item{weekly}
##'  \item{monthly}
##'  \item{quarterly}
##'  \item{yearly}
##' }
#'@param aggregate_function *optional* function to apply to aggregated fields. default=sum
timeSeriesNarrative <- function(corpus,
                                meta_time,
                                time_aggregate,
                                meta_field,
                                meta_normalisation_field=F,
                                aggregate_function=sum){
  
  scores<-data.frame(do.call(c,meta(corpus,meta_time)),unlist(meta(corpus,meta_field)))
  scores<-na.omit(scores)
  names(scores)<-c("date","score")
  xts.scores<-xts::xts(scores$score,order.by=scores$date)
  
  if(time_aggregate=="none"){return(xts.scores) # apply no aggregation, and therefore no normalisation
  }else if(time_aggregate=="daily"){apply_aggregate<-xts::apply.daily
  }else if(time_aggregate=="weekly"){apply_aggregate<-xts::apply.weekly
  }else if(time_aggregate=="monthly"){apply_aggregate<-xts::apply.monthly
  }else if(time_aggregate=="quarterly"){apply_aggregate<-xts::apply.quarterly
  }else if(time_aggregate=="yearly"){apply_aggregate<-xts::apply.yearly
  }else{stop("No time aggregate value provided")}
  
  if(meta_normalisation_field==T){
    norm.aggregate<-apply_aggregate(xts.scores,length)
  }else if(meta_normalisation_field==F){
    norm.aggregate=1
  }else{
    normalisation<-data.frame(do.call(c,NLP::meta(corpus,meta_time)),unlist(NLP::meta(corpus,meta_normalisation_field)))
    normalisation<-na.omit(normalisation)
    names(normalisation)<-c("date","normalisation")
    xts.normalisation<-xts::xts(normalisation$normalisation,order.by=normalisation$date)
    norm.aggregate<-apply_aggregate(xts.normalisation,sum)
  }
  
  return(apply_aggregate(xts.scores,aggregate_function)/norm.aggregate)
}

#'(DEPRECATED) Normalisation Generator
#'
#'Generates a data frame of common normalisation metrics 
#'(term, character, word and document counts) for a given corpus
#'over aggregated time periods.
#'@param corpus corpus object
#'@param meta_time document level meta data field that defines time
#'@param time_aggregate time resolution to aggregate over. Can be one of the following:
#'\itemize{
##'  \item{none - just returns un aggregated data, with NAs filtered}
##'  \item{daily}
##'  \item{weekly}
##'  \item{monthly}
##'  \item{quarterly}
##'  \item{yearly}
##' }
timeNormalise <- function(corpus,meta_time,time_aggregate,date.sequence=NULL){
  if(class(date.sequence)=="NULL"){
    date.range<-range(do.call(c,NLP::meta(corpus,meta_time)))
    
    date.sequence<-seq(date.range[1],
                       date.range[2],
                       # take "ly" characters off time_aggregate value
                       substr(time_aggregate,1,nchar(time_aggregate)-2))
  }
  
  term.count<-data.frame()
  i=1
  while(i<=length(date.sequence)){
    print(i)
    # test to ensure we don't exclude final date from filter
    if(i==(length(date.sequence)-1)){
      # less than *or equal* to last date
      temp.corp<-NLP::tm_filter(corpus,FUN = function(x) tm::meta(x,meta_time)>=date.sequence[i] & NLP::meta(x,meta_time)<=date.sequence[i+1])
    }else{
      temp.corp<-NLP::tm_filter(corpus,FUN = function(x) tm::meta(x,meta_time)>=date.sequence[i] & NLP::meta(x,meta_time)<date.sequence[i+1])
    }
    
    dtm.temp<-tm::DocumentTermMatrix(temp.corp)
    #terms.count<-nTerms(dtm.temp)
    #words.count<-sum(wordCount(dtm.temp))
    #characters.count<-sum(characterCount(temp.corp))
    term.count<-rbind(term.count,
                      data.frame(tm::nTerms(dtm.temp),
                                 sum(Narrative::wordCount(dtm.temp)),
                                 sum(Narrative::characterCount(temp.corp)),
                                 length(temp.corp),
                                 date.sequence[i+1]
                      ))
    i<-i+1
  }
  rm(temp.corp)
  dimnames(term.count)[[2]]<-c("terms.n","words.n","characters.n","docs.n","date")
  return(term.count)
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
logicalMatch <- function(search,logic,normalisation="logical"){
  if(logic=="AND"){
    logic.function=all
  }else if(logic=="OR"){
    logic.function=any
  }
  matches.logical<-apply(search>0,1,logic.function)
  
  if(is.character(normalisation) & typeof(normalisation)=="character"){
    return(matches.logical)
  }
  
  and.match<-vector(length=nrow(search))
  i<-1
  while(i<=length(matches.logical)){
    if(matches.logical[i]){
      and.match[i]<-normalisation(search[i,])
    }else{
      and.match[i]<-0
    }
    i<-i+1
  }
  names(and.match)<-names(matches.logical)
  return(and.match)
}


