# REFINE OUTPUT1

# Load the output from Stage 1 which involved extracting DeepDiveData sentences which contain both a word (or words) indicating the occurrence of fossils, and a candidate unit name. 
# Load the stage 1 output table from postgres
# Load required library
library("RPostgreSQL")

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

Stage1Output<-dbGetQuery(Connection,"SELECT * FROM pbdb_fidelity.output1_nlp352")

# Remove hits for microfossils and trace fossils within the output sentences
Microfossils<-grep("microfossil", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
TraceFossils<-grep("trace fossil", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
# Remove instances of the words "no fossils" to account for reading errors
NoFossils<-grep(" no fossils", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
LackFossils<-grep("lacks fossils", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
LackOfFossils<-grep("lack of fossils", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
AbsentFossils<-grep("absence of fossils", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)

# Combine all unwanted rows (sentences) with microfossils or trace fossils into one vector 
UnwantedRows<-unique(c(Microfossils,TraceFossils,NoFossils,LackFossils,LackOfFossils,AbsentFossils,VoidFossils,Correlative,Equivalent))

# Remove unwanted sentences from Stage1Output
CleanedOutput<-Stage1Output[-UnwantedRows,]

# Take a random sample of 100 Stage1Output Rows to check accuracy
CleanedSampleOutput1<-CleanedOutput[sample(c(1:nrow(CleanedOutput)),100,replace=FALSE,prob=NULL),]

# Save SampleOutput1 to a folder
write.csv(CleanedSampleOutput1,file="~/Documents/DeepDive/PBDB_Fidelity/R/CleanedSampleOutput1.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "CleanedSampleOutput1_Completed.csv"

