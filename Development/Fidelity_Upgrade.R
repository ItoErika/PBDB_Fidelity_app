Start<-print(Sys.time())

if (require("doParallel",warn.conflicts=FALSE)==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (require("RPostgreSQL",warn.conflicts=FALSE)==FALSE) {
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

# STEP ONE: Load DeepDiveData 
print(paste("Load postgres tables",Sys.time()))

# Download the config file
Credentials<-as.matrix(read.table("Credentials.yml",row.names=1))

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])
# Make SQL query
DeepDiveData<-dbGetQuery(Connection,"SELECT* FROM nlp_sentences_352") 

# If Testing: 
#Driver <- dbDriver("PostgreSQL") # Establish database driver
#Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")
#DeepDiveData<-dbGetQuery(Connection,"SELECT* FROM pbdb_fidelity.pbdb_fidelity_data")

# RECORD INITIAL STATS
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA: 
StepOneDescription<-"Initial Data"
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA:
StepOneDocs<-length((unique(DeepDiveData[,"docid"])))
StepOneRows<-nrow(DeepDiveData)

# STEP TWO: Clean DeepDiveData 
print(paste("Clean DeepDiveData",Sys.time()))

# Remove bracket symbols ({ and }) from DeepDiveData sentences
DeepDiveData[,"words"]<-gsub("\\{|\\}","",DeepDiveData[,"words"])
# Remove bracket symbols ({ and }) from DeepDiveData poses column
DeepDiveData[,"poses"]<-gsub("\\{|\\}","",DeepDiveData[,"poses"])
# Remove commas from DeepDiveData to prepare to run grep function
CleanedWords<-gsub(","," ",DeepDiveData[,"words"])
# Remove commas from DeepDiveData poses column
DeepDiveData[,"poses"]<-gsub(","," ",DeepDiveData[,"poses"])

# STEP THREE: Search for the word "formation" in all cleaned DeepDiveData sentences (CleanedWords)
print(paste("Search for the word 'formation' in DeepDiveData sentences",Sys.time()))
# Apply grep to cleaned words
FormationHits<-parSapply(Cluster,"formation",function(x,y) grep(x,y,ignore.case=TRUE, perl = TRUE),CleanedWords)

# STEP FOUR: Extact DeepDiveData rows corresponding with formation hits
print(paste("Extract formation hit rows from DeepDiveData",Sys.time()))
SubsetDeepDive<-sapply(FormationHits,function(x) DeepDiveData[x,])
# If testing: SubsetDeepDive<-sapply(FormationHits[1:1000],function(x) DeepDiveData[x,])
# Reformat SubsetDeepDive
SubsetDeepDive<-t(SubsetDeepDive)
    
# RECORD STATS
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE: 
StepFourDescription<-"Subset DeepDiveData to rows which contain the word 'formation'"
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE:
StepFourDocs<-length((unique(SubsetDeepDive[,"docid"])))
StepFourRows<-nrow(SubsetDeepDive)

# STEP FIVE: Replace slashes from SubsetDeepDive words and poses columns with the word "SLASH"
print(paste("Clean SubsetDeepDive",Sys.time()))
SubsetDeepDive[,"words"]<-gsub("\"","SLASH",SubsetDeepDive[,"words"])
SubsetDeepDive[,"poses"]<-gsub("\"","SLASH",SubsetDeepDive[,"poses"])

# STEP SIX: Extract NNPs from SubsetDeepDive
print(paste("Extract NNPs from SubsetDeepDive",Sys.time()))
# Create a list of vectors showing each formation hit sentence's unlisted poses column 
DeepDivePoses<-sapply(SubsetDeepDive[,"poses"],function(x) unlist(strsplit(as.character(x)," ")))
# Assign names to each list element corresponding to the document and sentence id of each sentence
doc.sent<-paste(SubsetDeepDive[,"docid"],SubsetDeepDive[,"sentid"],sep=".")
names(DeepDivePoses)<-doc.sent
    
#check: 
#test1<-sapply(DeepDivePoses, length)
#test2<-sapply(DeepDiveWords,length)
#identical(test1,test2)
#[1] TRUE

# Extract all the NNPs from DeepDivePoses
DeepDiveNNPs<-sapply(DeepDivePoses,function(x) which(x=="NNP"))
    
# STEP SEVEN: Find consecutive NNPs in DeepDiveNNPs
print(paste("Find consecutive NNPs in DeepDiveNNPs",Sys.time()))
    
# Consecutive word position locater function:
findConsecutive<-function(DeepDivePoses) {
    Breaks<-c(0,which(diff(DeepDivePoses)!=1),length(DeepDivePoses))
    ConsecutiveList<-lapply(seq(length(Breaks)-1),function(x) DeepDivePoses[(Breaks[x]+1):Breaks[x+1]])
    return(ConsecutiveList)
    }

# Apply function to DeepDiveNNPs list
ConsecutiveNNPs<-sapply(DeepDiveNNPs, function(x) findConsecutive(x))   
# Collapse each cluster into a single character string such that each sentence from formation hits shows its associated clusters    
SentenceNNPs<-sapply(ConsecutiveNNPs,function(y) sapply(y,function(x) paste(x,collapse=",")))
    
# STEP EIGHT: Find words associated with Conescutive NNPs
print(paste("Find words Associated with Conescutive NNPs",Sys.time()))

# Create a matrix with a row for each NNP cluster
# Make a column for sentence IDs
ClusterCount<-sapply(SentenceNNPs,length)
# Repeat the document & sentence ID info (denoted in the names of SentenceNNPs) by the number of NNP clusters in each sentence
DocSentID<-rep(names(SentenceNNPs),times=ClusterCount)
# Make a column for cluster elements 
ClusterPosition<-unlist(SentenceNNPs)
# Remove NA's from ClusterPosition
ClusterPosition<-ClusterPosition[which(ClusterPosition!="NA")]
# Make a column for the words associated with each NNP
# Get numeric elements for each NNP
NNPElements<-lapply(ClusterPosition,function(x) as.numeric(unlist(strsplit(x,","))))
# split document and sentence id data denoted in NNPElements names
SplitDocSent<-strsplit(names(NNPElements),'\\.')
# make a docid column for associated NNPElements
docid<-sapply(SplitDocSent,function(x) x[1])
# make a sentid column for associated NNPElements
sentid<-as.numeric(sapply(SplitDocSent,function(x) x[2]))
    
which(which(SubsetDeepDive[,"docid"]==docid[1])%in%SubsetDeepDive[,"sentid"][[as.numeric(sentid[[1]])]]==TRUE)
 
# Create a list of vectors showing each formation hit sentence's unlisted words column 
DeepDiveWords<-sapply(SubsetDeepDive[,"words"],function(x) unlist(strsplit(as.character(x),",")))
# Assign names to each list element corresponding to the document and sentence id of each sentence
doc.sent<-paste(SubsetDeepDive[,"docid"],SubsetDeepDive[,"sentid"],sep=".")
names(DeepDiveWords)<-doc.sent    
    
    
    
# Extract NNP words using NNP elements obtained above    
NNPWords<-vector("character",length=length(NNPElements))
for(Document in 1:length(NNPElements)){
    ExtractElements<-NNPElements[[Document]]
    DocumentName<-names(NNPElements)[Document]
    SplitWords<-unlist(strsplit(DDMatches[DocumentName,"words"],","))
    NNPWords[Document]<-paste(SplitWords[ExtractElements],collapse=" ")
    }    
    
    
    
