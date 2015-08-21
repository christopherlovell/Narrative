#' Read and write to and from corpus
#' 
#' @name Narrative
#' @docType package
NULL

#' Read separate text documents within a given directory, output corpus.
#' @param directory path to text file directory
#' @param load_metadata bool, whether to load metadata along with the corpus or not
#' @param meta_directory if load_metadata is True, specify the location directory. Defaults to `directory`.
#' @param metadata_filename if load_metadata is True, specify the filename.
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
      stop("metadata file not found at the specified directory")
    }
  }
  
  return(corp)
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
loadMetaData <- function(corpus, metadata){
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
#'@param directory ouput directory
#'@param filenames filenames with which to save each document. If `NULL`, defaults to document id metadata in corpus.
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

