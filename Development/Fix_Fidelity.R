##########################################################################################################################################
############################################################# LOAD LIBRARIES #############################################################
##########################################################################################################################################
library(readr)

##########################################################################################################################################
############################################################### FUNCTIONS ###############################################################
##########################################################################################################################################

parseSentence<-function(Sentence,Parameters=c("words","dep_paths","dep_parents")) {
        Sentence<-setNames(cleanPunctuation(Sentence),names(Sentence))
        if ("words"%in%names(Sentence)) {Sentence["words"]<-trueCommas(Sentence["words"])}
        WordsMatrix<-sapply(Sentence[Parameters],function(x) strsplit(x,","))
        if (sum(diff(sapply(WordsMatrix,length)))!=0) {return(NA)}
        WordsMatrix<-do.call(rbind,WordsMatrix)
        WordsMatrix[which(WordsMatrix=="COMMASUB")]<-","
        WordsMatrix[which(WordsMatrix=="SPACESUB")]<-""
        colnames(WordsMatrix)<-1:ncol(WordsMatrix)
        return(WordsMatrix)
        }

# Account for actual phrases or numbersets with commas - e.g., "19,560,238" -> 19560238
trueCommas<-function(Words) {
        InsideQuotes<-regmatches(Words, gregexpr('"[^"]*"',Words))[[1]]
        if (length(InsideQuotes)<1) {return(Words)}
        Replacements<-gsub(",","",InsideQuotes)
        for (i in 1:length(InsideQuotes)) {
                Words<-noquote(gsub(InsideQuotes[i],Replacements[i],Words))
                }
        return(Words)
        }

# Remove or replace problematic punctuation
# Even though this is redundnat with trueCommas it applies to more fields
cleanPunctuation<-function(Sentence) {
        Sentence<-gsub("\"\"","SPACESUB",Sentence)
        Sentence<-gsub("\",\"","COMMASUB",Sentence) 
        Sentence<-gsub("\",\"","COMMASUB",Sentence) 
        Sentence<-gsub("\\{|\\}","",Sentence)
        Sentence<-gsub("-LRB-","(",Sentence)
        Sentence<-gsub("-RRB-",")",Sentence)
        Sentence<-gsub("-LCB-","{",Sentence)
        Sentence<-gsub("-RCB-","}",Sentence)
        return(Sentence)
        }

# A function to find proper noun clusters
findCluster<-function(Sentence,Parameters=c("words","poses")) {
        ParsedSentence<-parseSentence(Sentence,Parameters)
        FindConsecutive<-findConsecutive(which(ParsedSentence["poses",]=="NNP"))
        Proper<-sapply(FindConsecutive,function(x) paste(unname(ParsedSentence["words",x]),collapse=" "))
        Proper<-unname(cbind(Sentence["docid"],Sentence["sentid"],Proper))
        return(Proper)
        }

# Sees if numbers are consecutive
findConsecutive<-function(NumericSequence) {
        Breaks<-c(0,which(diff(NumericSequence)!=1),length(NumericSequence))
        ConsecutiveList<-lapply(seq(length(Breaks)-1),function(x) NumericSequence[(Breaks[x]+1):Breaks[x+1]])
        return(ConsecutiveList)
        }

##########################################################################################################################################
############################################################### LOAD DATA ################################################################
##########################################################################################################################################

# Set working directory
setwd('C:/Users/erikai94/Documents/UW_Madison/GDD/test_docs')
# Load in test documents
docs<-as.data.frame(read_delim('sentences_nlp352', delim='\t', col_names = FALSE))

##########################################################################################################################################
############################################################### CLEAN DATA ###############################################################
##########################################################################################################################################

# Assign column names to the NLP documents
colnames(docs)<-c('docid','sentid','wordidx','words','poses','ners','lemmas','dep_paths','dep_parents')

# Subset the test document set (first 2 documents)
docs<-docs[1:max(which(docs[,"docid"]==unique(docs[,"docid"])[2])),]

# Clean the punctuation in all of the documents
test<-t(apply(docs[1:4113,],1 , cleanPunctuation))
                                
parseSentence(test[1,])                                

                 
                 
                 

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

for (i in 296:nrow(test)){
parseSentence(test[i,])
  print(i)
}
                                
ParsedSentence<-vector("list", length=nrow(test))                           
for (i in 1:nrow(test)){
        tryCatch(ParsedSentence[[i]]<-parseSentence(test[i,]), warning=function(i) {print(i)})
        }
        
        
        
        
        parseSentence<-function(Sentence,Parameters=c("words","dep_paths","dep_parents")) {
        Sentence<-setNames(cleanPunctuation(Sentence),names(Sentence))
        if ("words"%in%names(Sentence)) {Sentence["words"]<-trueCommas(Sentence["words"])}
        WordsMatrix<-sapply(Sentence[Parameters],function(x) strsplit(x,","))
  if (sum(diff(sapply(WordsMatrix,length)))!=0) {return(NA)}
        WordsMatrix<-do.call(rbind,WordsMatrix)
        WordsMatrix[which(WordsMatrix=="COMMASUB")]<-","
        WordsMatrix[which(WordsMatrix=="SPACESUB")]<-""
        colnames(WordsMatrix)<-1:ncol(WordsMatrix)
        return(WordsMatrix)
        }
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
                
trycatch(parseSentence(test[1,])                                
