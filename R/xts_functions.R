#' Narrative xts functions
#' 
#' @name Narrative
#' @docType package
NULL

#'Apply function over corpus
#'
#'Apply function over corpus within specified time periods, and return an aggregated xts object.
#'For example, the following function calculates the number of unique terms in a given corpus: 
#'\code{FUN<-function(x){nTerms(DocumentTermMatrix(x))}}
#'\code{corpusAggregate} splits the supplied corpus in to time bins, then applies the function to each
#'individual bin.
#'@param corpus corpus object over which to aggregate
#'@param meta_time meta data field containing time information
#'@param time_aggregate period over which to aggregate. Can be one of the following: \{day,week,month,quarter,year\}
#'@param FUN function to apply to corpus time bins
#'@param ... optional arguments to FUN
corpusAggregate <- function(corpus,meta_time,time_aggregate,FUN,...){
  date.range<-range(do.call(c,NLP::meta(corpus,meta_time)))
  date.sequence<-seq(date.range[1],date.range[2],time_aggregate)
  
  temp.df<-data.frame()
  i<-1
  while(i<length(date.sequence)){
    temp.corpus<-NULL
    temp.corpus<-tm::tm_filter(corpus,function(x){
      NLP::meta(x,meta_time)>date.sequence[i] & NLP::meta(x,meta_time)<date.sequence[i+1]
    })
    temp.df<-append(temp.df,FUN(temp.corpus))
    i<-i+1
  }
  
  Narrative::xtsGenerate(date.sequence[1:(length(date.sequence)-1)],unlist(temp.df))
}

#'Generate an xts object
#'
#'Returns an xts object given time and value data frames
#'@param time data frame of time values
#'@param value data frame of values corresponding to times
xtsGenerate <- function(time,value){
  df<-data.frame(time,value)
  df<-na.omit(df)
  names(df)<-c("date","value")
  return(xts(df$value,order.by=df$date))
}

#'Aggregate xts objects
#'
#'Returns an xts object aggregated and normalised over the given time window by the specified aggregation function
xtsAggregate <- function(xts.scores, time_aggregate, aggregate_function = sum, normalisation){
  
  if(time_aggregate == "none"){
    return(xts.scores) # apply no aggregation, and therefore no normalisation
  }else if(time_aggregate == "daily"){
    apply_aggregate <- xts::apply.daily
  }else if(time_aggregate == "weekly"){
    apply_aggregate <- xts::apply.weekly
  }else if(time_aggregate == "monthly"){
    apply_aggregate <- xts::apply.monthly
  }else if(time_aggregate == "quarterly"){
    apply_aggregate <- xts::apply.quarterly
  }else if(time_aggregate == "yearly"){
    apply_aggregate <- xts::apply.yearly
  }else{
    stop("No time aggregate value provided")
  }
  
  if(normalisation == F){
    norm.aggregate = 1
  }else if(normalisation == T){
    norm.aggregate<-apply_aggregate(xts.scores,length)
  }else if(class(normalisation) == "vector" & length(normalisation) == length(xts.scores)){
    xts.norm <- xtsGenerate(xts.scores$time,normalisation)
    norm.aggregate <- apply_aggregate(xts.norm, sum)
  }
  
  return(apply_aggregate(xts.scores, aggregate_function) / norm.aggregate)
}
