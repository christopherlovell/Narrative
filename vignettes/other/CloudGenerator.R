if (!require("tm",character.only = TRUE))
{
  install.packages("tm",dep=TRUE)
}
if (!require("wordcloud",character.only = TRUE))
{
  install.packages("wordcloud",dep=TRUE)
}
if (!require("data.table",character.only = TRUE))
{
  install.packages("data.table",dep=TRUE)
}

# try removing lib.loc argument if library load errors occur
library("wordcloud", lib.loc="~/R/win-library/3.1")
library("tm", lib.loc="~/R/win-library/3.1")
library("data.table", lib.loc="~/R/win-library/3.1")

# your local source directory
# directories on windows need to have double forward slashes in order to be read correctly
setwd("C://Documents//Word Clouds//")

rawDocs <- Corpus(DirSource(directory = "input")); # sub-directory to put your .txt files in

rawDocs <- tm_map(rawDocs, stripWhitespace);
rawDocs <- tm_map(rawDocs, removeWords, "Bank"); # remove 'Bank' capitalised only
rawDocs <- tm_map(rawDocs, tolower);
rawDocs <- tm_map(rawDocs, removeNumbers);
rawDocs <- tm_map(rawDocs, removePunctuation);

# Remove any non ASCII characters (eg MSWord-generated quote symbols)
(f <- function(x) iconv(x, "latin1", "ASCII", sub="")); 
rawDocs <- tm_map(rawDocs,f);

rawDocs <- tm_map(rawDocs, removeWords, stopwords("english"));



# Remove custom stop words (stop words must be tuned to the corpus of documents you are analysing)
customStops <- scan("dictionaries//customStopWords.txt", what="", sep="\n");
rawDocs <- tm_map(rawDocs, removeWords, customStops);

# stem the document
# if your stop words are stemmed this should be carried out before removing stop words
rawDocs <- tm_map(rawDocs, stemDocument);

# Extract word frequencies...
tdm <- TermDocumentMatrix(rawDocs);
m <- as.matrix(tdm);
v <- sort(rowSums(m),decreasing=TRUE);
d <- data.table(word = names(v),freq=v,key="word")

# write.csv(d, file = "Output\\wordlist.csv", row.names=FALSE);

# Re-label stemmer output based on input dictionary (dictionary must be tuned to the Corpus of documents you are analysing)
dict = data.table(read.csv("dictionaries//stemReplacementDictionary.csv"),key='word');
cloudStats = dict[d];

# cloud image will be written to this file within the Output sub-directory
# there are various options here for different output formats and sizes
png("output\\wordcloud_withStemming_manyStopwords.png", width=12,height=9,units='in',res=300); 

# loads of options on the call to wordcloud, to customise the output image
# type ?wordcloud for further information 
wordcloud(cloudStats$displayText, cloudStats$freq, scale=c(4,0.1), max.words=200, min.freq=10, random.order=FALSE, 
          rot.per=0, fixed.asp=TRUE, use.r.layout=FALSE, colors=brewer.pal(9, "Reds"), random.color=FALSE)
# wordcloud(rawDocs, scale=c(4,0.5), max.words=200, random.order=TRUE, 
#           rot.per=0.2, fixed.asp=TRUE, use.r.layout=FALSE, colors=brewer.pal(12, "Paired"))

dev.off();


# ... and the total number of words

# c = colSums(m);
