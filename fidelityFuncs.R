# Custom functions are camelCase. Arrays, parameters, and arguments are PascalCase
# Dependency functions are not embedded in master functions, and are marked with the flag dependency in the documentation
# []-notation is used wherever possible, and $-notation is avoided.

######################################### Load Required Libraries ###########################################
# Save and print the app start time
Start<-print(Sys.time())

# If running from UW-Madison
# Load or install the doParallel package
if (suppressWarnings(require("doParallel"))==FALSE) {
        install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
        library("doParallel");
        }

# Load or install the RPostgreSQL package
if (suppressWarnings(require("RPostgreSQL"))==FALSE) {
        install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
        library("RPostgreSQL");
        }

# Start a cluster for multicore, 3 by default or higher if passed as command line argument
CommandArgument<-commandArgs(TRUE)
if (length(CommandArgument)==0) {
        Cluster<-makeCluster(3)
        } else {
        Cluster<-makeCluster(as.numeric(CommandArgument[1]))
        }

#############################################################################################################
####################################### LOAD DATA FUNCTIONS, GEODEEPDIVE ####################################
#############################################################################################################
# No functions at this time

######################################### LOAD DATA SCRIPT, GEODEEPDIVE #####################################
# print current status to terminal 
print(paste("Load postgres tables",Sys.time()))

# Download the config file
Credentials<-as.matrix(read.table("Credentials.yml",row.names=1))
# Connect to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])
# Query the sentences fro postgresql
DeepDiveData<-dbGetQuery(Connection,"SELECT docid, sentid, words, poses, dep_paths, dep_parents FROM nlp_sentences_352")

#############################################################################################################
###################################### NNP CLUSTER FUNCTIONS, GEODEEPDIVE ###################################
#############################################################################################################

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
        if (sum(diff(sapply(WordsMatrix,length)))!=0) {return(NA)}f
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

######################################## NNP CLUSTER SCRIPT, GEODEEPDIVE ####################################
# print current status to terminal
print(paste("Generate the nnp_clusters",Sys.time()))

# Pass the functions to the cluster
clusterExport(cl=Cluster,varlist=c("parseSentence","findConsecutive","findCluster"))

# Generate the proper noun strings
ProperList<-parApply(Cluster,DeepDiveData,1,findCluster)
# Stop the cluster
stopCluster(Cluster)

# Collapse the list into a character matrix
ProperMatrix<-na.omit(do.call(rbind,ProperList))
colnames(ProperMatrix)<-c("docid","sentid","Proper")

# Remove the "NA"'s
ProperMatrix<-subset(ProperMatrix,ProperMatrix[,"Proper"]!="NA")

# Set directory for output
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory,"/output",sep=""))
    
# Clear any old output files
unlink("*")

# Write csv output files
write.csv(ProperMatrix, "ProperMatrix.csv")

# COMPLETE
print(paste("Complete",Sys.time()))