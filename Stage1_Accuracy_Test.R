# Stage 1: Extracting DeepDiveData sentences which contain both a word (or words) indicating the occurrence of fossils, and a candidate unit name. 
# Load the stage 1 output table from postgres
# Load required library
library("RPostgreSQL")

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

Stage1Output<-dbGetQuery(Connection,"SELECT * FROM pbdb_fidelity.output1_nlp352")

# Take a random sample of 100 Stage1Output Rows to check accuracy
SampleOutput1<-Stage1Output[sample(c(1:nrow(Stage1Output)),100,replace=FALSE,prob=NULL),]

# Save SampleOutput1 to a folder
write.csv(SampleOutput1,file="~/Documents/DeepDive/PBDB_Fidelity/R/SampleOutput1.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "SampleOutput1_Completed.csv"
# 
