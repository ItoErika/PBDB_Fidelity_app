Start<-print(Sys.time())

if (require("doParallel",warn.conflicts=FALSE)==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (require("RPostgreSQL",warn.conflicts=FALSE)==FALSE) {
    install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
    library("RPostgreSQL");
    }

# Start a cluster for multicore, 4 by default or higher if passed as command line argument
CommandArgument<-commandArgs(TRUE)
if (is.na(CommandArgument)) {
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
DeepDiveData<-dbGetQuery(Connection,"SELECT docid, sentid, words FROM nlp_sentences_352") 

# If Testing: 
#Driver <- dbDriver("PostgreSQL") # Establish database driver
#Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")
#DeepDiveData<-dbGetQuery(Connection,"SELECT docid, sentid, words FROM pbdb_fidelity.pbdb_fidelity_data")

# RECORD INITIAL STATS
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA: 
StepOneDescription<-"Initial Data"
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA:
StepOneDocs<-length((unique(DeepDiveData[,"docid"])))
StepOneRows<-nrow(DeepDiveData)
StepOneUnits<-"NA"
StepOneTuples<-"NA"

# STEP TWO: Clean DeepDiveData 
print(paste("Clean DeepDiveData",Sys.time()))

# Remove bracket symbols ({ and }) from DeepDiveData sentences
DeepDiveData[,"words"]<-gsub("\\{|\\}","",DeepDiveData[,"words"])
# Remove commas from DeepDiveData to prepare to run grep function
CleanedWords<-gsub(","," ",DeepDiveData[,"words"])

# STEP THREE: Search for the word "formation" in all cleaned DeepDiveData sentences (CleanedWords)
print(paste("Search for the word 'formation' in DeepDiveData sentences",Sys.time()))
# Apply grep to cleaned words
FormationHits<-parSapply(Cluster,"formation",function(x,y) grep(x,y,ignore.case=TRUE, perl = TRUE),CleanedWords)
