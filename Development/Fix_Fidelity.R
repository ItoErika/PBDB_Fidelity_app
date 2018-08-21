##########################################################################################################################################
############################################################# LOAD LIBRARIES #############################################################
##########################################################################################################################################
library(readr)

##########################################################################################################################################
############################################################### FUNCTIONS ###############################################################
##########################################################################################################################################

# Remove or replace problematic punctuation
# Even though this is redundnat with trueCommas it applies to more fields
cleanPunctuation<-function(Sentence) {
        Sentence<-gsub("\"\"","SPACESUB",Sentence)
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
        if(all(is.na(ParsedSentence))) {return(setNames(c(Sentence["docid"], Sentence["sentid"],"parsing error"), c("docid", "sentid", "Proper")))}
        FindConsecutive<-findConsecutive(which(ParsedSentence["poses",]=="NNP"))
        Proper<-sapply(FindConsecutive,function(x) paste(unname(ParsedSentence["words",x]),collapse=" "))
        Proper<-unname(cbind(Sentence["docid"],Sentence["sentid"],Proper))
        colnames(Proper)<-c("docid","sentid","Proper") 
        return(Proper)
        }

# Sees if numbers are consecutive
findConsecutive<-function(NumericSequence) {
        Breaks<-c(0,which(diff(NumericSequence)!=1),length(NumericSequence))
        ConsecutiveList<-lapply(seq(length(Breaks)-1),function(x) NumericSequence[(Breaks[x]+1):Breaks[x+1]])
        return(ConsecutiveList)
        }

# Parse the NLP strings into a matrix format
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

# R confuses 2,000,381 in a PostgreSQL array as 2 000 381, this function will convert those cases to 2000381.  
trueCommas<-function(Words) {
        InsideQuotes<-regmatches(Words, gregexpr('"[^"]*"',Words))[[1]]
        if (length(InsideQuotes)<1) {return(Words)}
        Replacements<-gsub(",","",InsideQuotes)
        for (i in 1:length(InsideQuotes)) {
               Words<-noquote(gsub(InsideQuotes[i],Replacements[i],Words))
               }
        return(Words)
        }

# A function to parse a sentence and extract any grammatically linked termspaired terms
# In principle this should work with other path types, not just amod, but I have not tested it.
adjacencyPath<-function(Sentence,Path="amod") {
        ParsedSentence<-parseSentence(Sentence,c("words","dep_paths","dep_parents"))
        if (all(is.na(ParsedSentence))) {return(setNames(c(Sentence["docid"], Sentence["sentid"],rep("parsing error",2)),c("docid","sentid","child","parent")))}
        PathMods<-as.matrix(ParsedSentence[,which(ParsedSentence["dep_paths",]==Path)])
        if (length(PathMods)<1) {return(setNames(rep(NA,4),c("docid","sentid","child","parent")))}
        FinalList<-vector("list")
        for (j in seq_len(ncol(PathMods))) {
              FinalList[[length(FinalList)+1]]<-c(Sentence[1:2],PathMods["words",j],ParsedSentence["words",as.numeric(PathMods["dep_parents",j])])
              }
        FinalTable<-do.call(rbind,FinalList)
        colnames(FinalTable)<-c("docid","sentid","child","parent")
        return(FinalTable)
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

# Subset the test document set (first 300 documents)
docs<-docs[1:max(which(docs[,"docid"]==unique(docs[,"docid"])[300])),]

# Clean the punctuation in all of the documents
docs<-t(apply(docs,1 , cleanPunctuation))

# Download sedimentary lithologies from Macrostrat
SedLiths<-read.csv("https://macrostrat.org/api/v1/defs/lithologies?lith_class=sedimentary&format=csv")
 
# Run findCluster on docs                            
Clusters<-apply(docs,1,findCluster)
# Remove rows where there were no clusters found
Clusters<-na.omit(do.call(rbind,Clusters))
# Convert Clusters into a data frame and change the format of sentid to be numeric
Clusters<-as.data.frame(Clusters, stringsAsFactors=FALSE)
Clusters[,"sentid"]<-as.numeric(as.character(Clusters[,"sentid"]))
                            
# Find linked words in docs
LinkedWords<-apply(docs, 1, adjacencyPath)
# Remove rows where there were no linked words
LinkedWords<-na.omit(do.call(rbind,LinkedWords))
# Convert LinkedWords into a data frame and change the format of sentid to be numeric
LinkedWords<-as.data.frame(LinkedWords, stringsAsFactors=FALSE)
LinkedWords[,"sentid"]<-as.numeric(as.character(LinkedWords[,"sentid"]))

# Search for the word "formation" in the proper noun clusters
FmClusters<-grep(Clusters[,"Proper"], pattern=" Formation", perl=TRUE, ignore.case=TRUE)


# Search for the word "fossil" in the proper noun clusters
FossilChildren<-grep(LinkedWords[,"child"], pattern="fossil", perl=TRUE, ignore.case=TRUE)

unique(LinkedWords[FossilChildren,"parent"])[1:100]                            



|


                                  
# Download occurrences data from the Paleobiology Database
print(paste("Download PBDB occurrence data.",Sys.time()))
PBDBURL<-"https://paleobiodb.org/data1.2/occs/list.csv?&cc=NOA"
PBDBURL<-RCurl::getURL(PBDBURL)
OccurrencesData<-read.csv(text=PBDBURL)

# Download geologic unit data from the Macrostrat database. 
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
print(paste("Download Macrostrat unit and age data.",Sys.time()))
# Download all sedimentary unit data from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv" 
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download data for geologic formations from the Macrostrat database API
StartNamesURL<-"https://macrostrat.org/api/defs/strat_names?&format=csv"
StartNamesURL<-RCurl::getURL(StartNamesURL)
StratNamesFrame<-read.csv(text=FormationsURL, header=TRUE)

# Download geologic time scale data from the Macrostrat API
IntervalsURL<-"https://macrostrat.org/api/defs/intervals?all&format=csv"
IntervalsURL<-RCurl::getURL(IntervalsURL)
IntervalsFrame<-read.csv(text= IntervalsURL, header=TRUE)                                

StratNamesFrame<-subset(StratNamesFrame, StratNamesFrame[,"rank"]%in%c("Mbr", "Fm", "Gp"))                                
 
# First, remove ambiguoulsy named formations from UnitsFrame and FormationsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"|UnitsFrame[,"strat_name_long"]=="Sandy Limestone"),]
FormationsFrame<-FormationsFrame[-which(FormationsFrame[,"strat_name_long"]=="Muddy Sandstone"|FormationsFrame[,"strat_name_long"]=="Mutual Formation"|FormationsFrame[,"strat_name_long"]=="Sandy Limestone"),]
# Second, subset UnitsFrame to only include members, formations, and groups                               
SubsetUnits<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%StratNamesFrame[,"strat_name_long"])
# Third, remove Precambrian units from FormationUnits
# Extract the maximum age for units of interest
Max_age<-IntervalsFrame[which(IntervalsFrame[,"name"]=="Precambrian"),"t_age"]
# Make sure the top age of the formations are less than the max age (less than the Cambrian-Proterozoic boundary age)
SubsetUnits<-SubsetUnits[which(SubsetUnits[,"t_int_age"]<Max_age),]

# (1) geologic units without fossils, (2) geologic units with fossils, (3) the first two dictionaries combined
# Convert the strat_name_long column of formation units to character
SubsetUnits[,"strat_name_long"]<-as.character(SubsetUnits[,"strat_name_long"])
# Take sum of pbdb_collections values associated with each strat name 
Collections<-tapply(SubsetUnits[,"pbdb_collections"], SubsetUnits[,"strat_name_long"], sum)
# Extract strat names with a sum of zero pbdb_collections (units with no fossil occurrences according to PBDB)
CandidateUnits<-names(which(Collections==0))
# Extract the strat names with a at least one pbdb collection record
FossilUnits<-names(which(Collections>0))
# Bind all units together
AllUnits<-c(CandidateUnits, FossilUnits)   


                            

                            
                            
                            
                            
# Search for formal geologic unit names in the proper noun clusters                           
UnitHits<-sapply(FormationUnits, function(x,y) agrep(x,y, ignore.case=TRUE, max.distance=0.2), Clusters[,"Proper"])
                               
                                
                                
                                
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
                                
       

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
                
trycatch(parseSentence(test[1,])                                
