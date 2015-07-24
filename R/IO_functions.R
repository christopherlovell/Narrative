#' Read and write to and from corpus
#' 
#' @name Narrative
#' @docType package
NULL

#' Read separate text documents within a given directory, output corpus.
#' @param directory path to text file directory
readSeparateText <- function(directory, load_metadata = F, meta_directory = directory, metadata_filename = NULL){
  corp <- tm::VCorpus(tm::DirSource(directory, encoding = "UTF-8",pattern="^.*\\.(txt)$"))
  names(corp)<-basename(file_path_sans_ext(names(corp)))  # rename list items as filenames (without extensions)
  
  if(load_metadata){
    if(is.null(metadata_filename)){
      stop("Please provide a valid metadata filename")
    }
    file_dir <- paste(meta_directory,metadata_filename,sep="/")
    if(file.exists(file_dir)){
      metadata <- readRDS(file = file_dir)
      corp <- loadMetaData(corp,metadata)
    }else{
      stop("metadata file does not exists as the specified directory")
    }
  }
  
  return(corp)
}

#'Read text output from Lexis Nexis
#'
#'Reads text output from Lexis Nexis. Output must be in txt format with the following fields specified (in order):
#'\itemize{
#'\item{"LENGTH"}
#'\item{"HEADLINE"}
#'\item{"BODY"}
#'\item{"LANGUAGE"}
#'\item{"PUBLICATION TYPE"}
#'} 
#'
#'If you wish to include more fields, please contact the package owner.
#'
#' @param file string indicating directory of lexis nexis output file
.readLexisNexisText <- function(file){
  # read each line of the file
  page <- readLines(file)
  
  # REGEX the document start strings
  doc_starts<-grep("[0-99](\\W|^)(of)(\\W|$)[0-99]*(\\W|^)(DOCUMENTS)(\\W|$)",page,value=F)
  # get number of documents from number of doc starts read
  no_of_docs<-length(doc_starts)
  # add final line count to doc_counts so that subsequent loop doesn't end prematurely
  doc_starts<-append(doc_starts,length(page))
  
  # create a list with the same dimensions of the number of documents
  parsed<-as.list(doc_starts)
  
  # reset document counter
  n<-0
  i<-1
  # loop over documents
  while(i <= no_of_docs){
    # set line_no iterator to value from doc_start
    line_no<-i
    # increment document counter
    n<-n+1
    
    print(i)
    
    # while condition: test that line is within current document
    while(line_no<doc_starts[n+1]){
      
      if(grepl("(\\W|^)(LENGTH:)(\\W|$)",page[line_no])){
        parsed[[n]]$length<-substr(page[line_no],9,nchar(page[line_no]))
        line_no<-line_no+1
        next
      }
      
      if(grepl("(\\W|^)(HEADLINE:)(\\W|$)",page[line_no])){
        parsed[[n]]$headline<-as.POSIXct(substr(page[line_no],11,nchar(page[line_no])),format="")
        line_no<-line_no+1
        next
      }
      
      if(grepl("(\\W|^)(LOAD-DATE:)(\\W|$)",page[line_no])){
        parsed[[n]]$loadDate<-as.POSIXct(substr(page[line_no],12,nchar(page[line_no])),format="%B %d, %Y")
        line_no<-line_no+1
        next
      }
      
      as.POSIXct(substr(page[98],12,nchar(page[98])),format="%B %d, %Y")
      
      ## add paragraph structure to data frame??
      if(grepl("(\\W|^)(BODY:)(\\W|$)",page[line_no])){
        line_no<-line_no+1
        bodyText<-character()
        while(!grepl("(\\W|^)(LANGUAGE:)(\\W|$)",page[line_no])){
          bodyText<-append(bodyText,page[line_no])
          line_no<-line_no+1
        }
        
        parsed[[n]]$body<-bodyText
        
        if(grepl("(\\W|^)(LANGUAGE:)(\\W|$)",page[line_no])){
          parsed[[n]]$language<-substr(page[line_no],11,nchar(page[line_no]))
          line_no<-line_no+1
          next
        }
        
        line_no<-line_no+1
        next
      }
      
      
      if(grepl("(\\W|^)(PUBLICATION-TYPE:)(\\W|$)",page[line_no])){
        parsed[[n]]$publicationType<-substr(page[line_no],19,nchar(page[line_no]))
        line_no<-line_no+1
        next
      }
      
      #     if(grepl("(\\W|^)(SUBJECT:)(\\W|$)",page[line_no])){
      #       parsed[[n]]$subject<-substr(page[line_no],10,nchar(page[line_no]))
      #       next
      #     }
      
      line_no<-line_no+1
    }
    
    i<-i+1
    
  }
  
  # initialise corpus with dummy first article
  lex_corpus <- Corpus(VectorSource(paste(parsed[[1]]$body,sep="",collapse=" ")))
  # convert to PlainTextDocument corpus (allows DTM to be produced)
  lex_corpus <- tm_map(lex_corpus, PlainTextDocument)  
  
  # loop over list, convert to PlainTextDocument, and add to Corpus
  i<-1
  while(i <= no_of_docs){
    # concatenate the body and place in a PlainTextDocument
    # - REQUIRES WORK - HEADINGS AND SENTENCE DELIMINATORS LOST with paste
    text_doc <- PlainTextDocument(paste(parsed[[i]]$body,sep="",collapse=" "))
    
    meta(text_doc,tag="language") <- parsed[[i]]$language
    meta(text_doc,tag="publicationType") <- parsed[[i]]$publicationType
    meta(text_doc,tag="length") <- parsed[[i]]$length
    meta(text_doc,tag="heading") <- parsed[[i]]$headline
    meta(text_doc,tag="loadDate") <- parsed[[i]]$loadDate
    meta(text_doc,tag="id") <- i
    
    lex_corpus[[i]] <- text_doc                         
    
    i <- i+1
  }
  
  return(lex_corpus)
}

#'Extract meta data
#'
#'Extract meta data in to a data frame from a corpus object
#'@param corpus corpus object with associated meta data
extractMetaData <- function(corpus) {
  metadata<-lapply(corpus, function(x) {  # extract meta data to list of lists, arranged by document
    meta_data <- NLP::meta(x)
    class(meta_data) <- "list"
    meta_data
  })
  
  listnames <- unique(reshape2::melt(sapply(metadata, names))$value)  # get all unique meta data names
  md <- lapply(listnames,function(x) sapply(metadata, '[', x))  # convert list of lists to named list by meta data field
  names(md) <- listnames
  md
}

#'Load meta data on to a corpus
#'
#'@param corpus tm corpus object
#'@param metadata list of lists, where each higher level list item is a named meta data field, 
#'and each lower level list is of equal length to the corpus, and contains values for the given field. `extractMetaData()` produces 
#'compatible output.
loadMetaData <- function(corpus,metadata){
  i <- 1
  while(i <= length(metadata)) {
    corpus <- Narrative::addToMetaData(corpus, vector = metadata[[i]], tag = names(metadata[i]))
    i <- i + 1
  }
  corpus
}

#'Save Corpus
#'
#'Save with meta data
#'@param corpus a tm corpus object
#'@param dir ouput directory
#'@param tags list of meta data tags to save. Defaults to all meta data
#'@param save_metadata switch to indicate that metadata should be saved
#'@param meta_directory ouput metadata directory. defaults to `dir`
saveCorpus <- function(corpus, directory, filenames = NULL, save_metadata = F, meta_directory = directory){
  if(save_metadata){
    metadata <- extractMetaData(corpus)
    metadata_filename <- paste(deparse(substitute(corpus)),"metadata.RDS", sep = "_")
    base::saveRDS(metadata,file = paste(meta_directory, metadata_filename, sep = "//"))
    cat(paste("metadata saved to \"",meta_directory,"\" as \"",metadata_filename,"\"",sep=" "))
  }
  tm::writeCorpus(corpus, directory, filenames = filenames)
}

