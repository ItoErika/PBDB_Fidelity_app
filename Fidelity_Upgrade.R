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
StepOneClusters<-0

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

# STEP THREE: Search for the word " formation" in all cleaned DeepDiveData sentences (CleanedWords)
print(paste("Search for the word ' formation' in DeepDiveData sentences",Sys.time()))
# Apply grep to cleaned words
FormationHits<-parSapply(Cluster," formation",function(x,y) grep(x,y,ignore.case=TRUE, perl = TRUE),CleanedWords)

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
StepFourClusters<-0

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
    
# Create a data frame with a row for each NNP cluster
# Make a column for cluster elements 
ClusterPosition<-unlist(SentenceNNPs)
# Make a column for sentence IDs
ClusterCount<-sapply(SentenceNNPs,length)
# Repeat the document & sentence ID info (denoted in the names of SentenceNNPs) by the number of NNP clusters in each sentence
DocSentID<-rep(names(SentenceNNPs),times=ClusterCount)
SplitDocSent<-strsplit(DocSentID,'\\.') 
# Create docid column for each cluster
docid<-sapply(SplitDocSent,function(x) x[1])
# make a sentid column for each cluster
sentid<-as.numeric(sapply(SplitDocSent,function(x) x[2]))    
# Bind cluster position data with document/sentence id data
ClusterData<-as.data.frame(cbind(ClusterPosition,docid,sentid))
# Remove NA's from ClusterData
ClusterData<-ClusterData[which(ClusterData[,"ClusterPosition"]!="NA"),]
# Reformat ClusterData
ClusterData[,"ClusterPosition"]<-as.character(ClusterData[,"ClusterPosition"])
ClusterData[,"docid"]<-as.character(ClusterData[,"docid"])
ClusterData[,"sentid"]<-as.numeric(as.character(ClusterData[,"sentid"]))
    
# Make a column for the words associated with each NNP
# Create a vector of the number of rows in ClusterData.
NumClusterVector<-1:nrow(ClusterData)   
# Extract the proper SubsetDeepDive rows based on the data in ClusterData    
SubsetDeepDiveRow<-sapply(NumClusterVector,function(x) which(SubsetDeepDive[,"docid"]==ClusterData[x,"docid"]&SubsetDeepDive[,"sentid"]==ClusterData[x,"sentid"]))
# Bind row data to ClusterData and convert it into a dataframe
ClusterData<-cbind(ClusterData,SubsetDeepDiveRow)
ClusterData[,"SubsetDeepDiveRow"]<-as.numeric(as.character(ClusterData[,"SubsetDeepDiveRow"]))
 
# Extract the sentences the associated SubsetDeepDive rows  
ClusterSentences<-sapply(ClusterData[,"SubsetDeepDiveRow"], function (x) SubsetDeepDive[x,"words"])
# Split and unlist the words in each cluster sentence
ClusterSentencesSplit<-sapply(ClusterSentences,function(x) unlist(strsplit(as.character(x),",")))
# Extract the NNP Clusters from theh associate sentences 
# Get numeric elements for each NNP Cluster word
NNPElements<-lapply(ClusterData[,"ClusterPosition"],function(x) as.numeric(unlist(strsplit(x,","))))
# Create a vector for the number of Clusters in ClusterData
NumClusterVector<-1:nrow(ClusterData) 
# Extract the words from ClusterSentencesSplit       
ClusterWords<-sapply(NumClusterVector, function(y) sapply(NNPElements[y], function(x) ClusterSentencesSplit[[y]][x]))
# Collapse the clusters into single character strings
NNPWords<-sapply(ClusterWords, function(x) paste(array(x), collapse=" "))
# Bind the clusters to the ClusterData frame
ClusterData[,"NNPWords"]<-NNPWords
    
# RECORD STATS
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE: 
StepEightDescription<-"Extract NPP clusters from SubsetDeepDive"
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE:
StepEightDocs<-length(unique(ClusterData[,"docid"]))
StepEightRows<-length(unique(ClusterData[,"SubsetDeepDiveRow"]))
StepEightClusters<-nrow(ClusterData)
    
# STEP NINE: Extract the rows with clusters with the word 'formation' from ClusterData   
print(paste("Extract 'formation' clusters from ClusterData",Sys.time()))
FormationClusters<-grep(" formation",ClusterData[,"NNPWords"],ignore.case=TRUE,perl=TRUE)
# Extract those rows from ClusterData
FormationData<-ClusterData[FormationClusters,]
FormationData[,"docid"]<-as.character(FormationData[,"docid"])
    
# RECORD STATS
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE: 
StepNineDescription<-"Extract NNP clusters containing the word 'formation'"
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE:
StepNineDocs<-length(unique(FormationData[,"docid"]))
StepNineRows<-length(unique(FormationData[,"SubsetDeepDiveRow"]))
StepNineClusters<-nrow(FormationData)

# STEP TEN: Subset SubsetDeepDive to only include rows with formation clusters   
print(paste("Subset SubsetDeepDive to only include rows with formation clusters",Sys.time()))
FormationDeepDive<-sapply(FormationData[,"SubsetDeepDiveRow"], function(x) SubsetDeepDive[x,])
# Reformat FormationtDeepDive
FormationDeepDive<-t(FormationDeepDive)      
    
# STEP ELEVEN: Remove Formations that are more than 5 words in length.
print(paste("Remove Formations > 5 words in length",Sys.time()))
# Determine the number of words in each NNPWords row
WordLength<-sapply(sapply(FormationData[,"ClusterPosition"], function(x) strsplit(x, ",")), function(x) length(x))
# Determine which rows have more than 5 NNPWords
BadFormations<-which(WordLength>5)
# Remove those rows from FormationData
FormationData<-FormationData[-BadFormations,]

# RECORD STATS
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE: 
StepElevenDescription<-"Remove Formations > 5 words in length"
# NUMBER OF DOCUMENTS AND ROWS IN SUBSETDEEPDIVE:
StepElevenDocs<-length(unique(FormationData[,"docid"]))
StepElevenRows<-length(unique(FormationData[,"SubsetDeepDiveRow"]))
StepElevenClusters<-nrow(FormationData)

# Extract columns of interest for the output
FormationData<-FormationData[,c("ClusterPosition","docid","sentid","NNPWords")]
   
print(paste("Writing Outputs",Sys.time()))

# Return stats table 
StepDescription<-c(StepOneDescription, StepFourDescription, StepEightDescription, StepNineDescription, StepElevenDescription)
NumberDocuments<-c(StepOneDocs, StepFourDocs, StepEightDocs, StepNineDocs, StepElevenDocs)
NumberRows<-c(StepOneRows, StepFourRows, StepEightRows, StepNineRows, StepElevenRows)
NumberClusters<-c(StepOneClusters, StepFourClusters, StepEightClusters, StepNineClusters, StepElevenClusters) 
# Bind Stats Columns
Stats<-cbind(StepDescription,NumberDocuments,NumberRows,NumberClusters)    

# Set directory for output
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory,"/output",sep=""))
    
# Clear any old output files
unlink("*")

# Write output files
saveRDS(FormationData, "FormationData.rds")
write.csv(FormationData, "FormationData.csv")
write.csv(Stats, "Stats.csv")
      
print(paste("Complete",Sys.time()))   
    
# See https://github.com/ItoErika/PBDB_Fidelity_app/blob/master/Development/StateCountryData_Relationships.R
# Based on PBDB API, the countries with less than or eqal to 50 occurrences are as follows: 
# Bangladesh, Burkina Faso, Burundi, Benin, Bhutan, Botswana, Central African Republic, Congo, Finland, Gabon, Guinea,                                     
# Guinea-Bissau, Guyana, Honduras, Kiribati, Comoros, Kazakhstan, Saint Lucia, Liberia, Montenegro, Macedonia, 
# the former Yugoslav Republic of Maldives, Nicaragua, Palestine State of, Rwanda, Solomon Islands, Sierra Leone                               
# Suriname, Sao Tome and Principe, El Salvador, Namibia  

# Based on the list of countries above, create a vector of countries of interest for searching for fossils in literature
# DarkCountries<-c("Bangladesh","Finland","Honduras","Kazakhstan","Nicaragua","Rwanda","Namibia","El Salvador")
# Make a list of the country codes associated with each country
# DarkCodes<-c("BD", "FI", "HN", "KZ", "NI", "RW", "NA", "SV")
# Bind country data to codes
# DarkCountryData<-cbind(DarkCountries,DarkCodes)

# Download world cities list
# WorldCities<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/LocationData/worldcities.csv")
# Create a list of cities for each country of interest

# NOTE: Namibia is listed as "NA" because of R reading error, so account for that in extraction method
# BDCities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="BD"),"name"]
# FICities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="FI"),"name"]
# HNCities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="HN"),"name"]
# KZCities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="KZ"),"name"]
# NICities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="NI"),"name"]
# RWCities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="RW"),"name"]
# NACities<-WorldCities[which(is.na(WorldCities[,"ISO.3166.1.country.code"])),"name"]
# SVCities<-WorldCities[which(WorldCities[,"ISO.3166.1.country.code"]=="SV"),"name"]
