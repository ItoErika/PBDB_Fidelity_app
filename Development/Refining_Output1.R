# Load the output from Stage 1 which involved extracting DeepDiveData sentences which contain both a word (or words) indicating the occurrence of fossils, and a candidate unit name. 
# Load the stage 1 output table from postgres
# Load required library
library("RPostgreSQL")

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

Stage1Output<-dbGetQuery(Connection,"SELECT * FROM pbdb_fidelity.output1_nlp352")

# Remove hits for microfossils and trace fossils within the output sentences
Microfossils<-grep("microfossil",Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)
TraceFossils<-grep("trace fossil",Stage1Output[,"sentence"], ignore.case=TRUE, perl=TRUE)

# Combine all rows (sentences) with microfossils or trace fossils into one vector 
MicroTraceRows<-c(Microfossils,TraceFossils)

# Remove unwanted sentences from Stage1Output
CleanedOutput<-Stage1Output[-MicroTraceRows,]
