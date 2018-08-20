# Parse the NLP strings into a matrix format
parseSentence<-function(Sentence,Parameters=c("words","dep_paths","dep_parents")) {
  Sentence<-setNames(gsub("\"\"","SPACESUB",Sentence),names(Sentence))
  Sentence<-setNames(gsub("\",\"","COMMASUB",Sentence),names(Sentence))
  WordsMatrix<-sapply(Sentence[Parameters],function(x) strsplit(substring(x,2,nchar(x)-1),","))
  WordsMatrix<-do.call(rbind,WordsMatrix)
  WordsMatrix[which(WordsMatrix=="COMMASUB")]<-","
  WordsMatrix[which(WordsMatrix=="SPACESUB")]<-""
  colnames(WordsMatrix)<-1:ncol(WordsMatrix)
  return(WordsMatrix)
}

# Sees if numbers are consecutive
findConsecutive<-function(NumericSequence) {
  Breaks<-c(0,which(diff(NumericSequence)!=1),length(NumericSequence))
  ConsecutiveList<-lapply(seq(length(Breaks)-1),function(x) NumericSequence[(Breaks[x]+1):Breaks[x+1]])
  return(ConsecutiveList)
}

# A function to find proper noun clusters
findCluster<-function(Sentence,Parameters=c("words","poses")) {
  ParsedSentence<-parseSentence(Sentence,Parameters)
  FindConsecutive<-findConsecutive(which(ParsedSentence["poses",]=="NNP"))
  Proper<-sapply(FindConsecutive,function(x) paste(unname(ParsedSentence["words",x]),collapse=" "))
  Proper<-unname(cbind(Sentence["docid"],Sentence["sentid"],Proper))
  return(Proper)
}


docs<-as.data.frame(read_delim('C:/Users/erikai94/Documents/UW_Madison/GDD/test_docs/sentences_nlp352', delim='\t', col_names = FALSE))
# Remove { and } from docs
docs<-gsub("\\{|\\}", "", docs)
colnames(docs)<-c('docid','sentid','wordidx','words','poses','ners','lemmas','dep_paths','dep_parents')




clean_words <- function(x) {
  
  . <- NULL
  
  ret <- gsub("[(${)(}^)]", "", x) %>%                # Curly brakets
    gsub("(,([\\;\\:\\%]),)","\\2 ", .) %>%
    gsub("(([^\"]),([^\"]))", "\\2 \\3", ., perl = TRUE) %>%
    gsub(",\",\",", ", ", .) %>%                 # Commas
    gsub(" \\.", "\\.", .) %>%
    gsub("-LRB-\\s", "\\(", .) %>%
    gsub("(,-RRB-)|(\\s-RRB-)", "\\)", .) %>%
    gsub("(,-RSB-)|(\\s-RSB-)", "\\]", .) %>%
    gsub("-LSB-\\s", "\\[", .)
  
  attr(ret, 'input') <- x
  
  return(ret)
  
}

options(warn=2)

for (i in 1:nrow(docs)){
parseSentence(docs[i,])
  print(i)
}

# Remove bracket symbols ({ and }) from SubsetDeepDive sentences
SubsetDeepDive[,"words"]<-gsub("\\{|\\}", "", SubsetDeepDive[,"words"])
