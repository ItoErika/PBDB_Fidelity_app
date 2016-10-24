# Load Libraries
library("RCurl")
library("RPostgreSQL")

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

# Load intersected location tuples table 
LocationTuples<-dbGetQuery(Connection,"SELECT* FROM column_locations.intersections")

# Load data for macrostrat units that contain candidate units (marine, sedimentary, unfossiliferous in PBDB)

# Download dictionary of unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Group by long strat name and take sum of pbdb_collections values
Collections<-tapply(UnitsFrame[,"pbdb_collections"],UnitsFrame[,"strat_name_long"],sum)
# Extract strat names with a sum of zero pbdb_collections, indicating the unit name has no fossil occurrences according to PBDB
CandidateUnits<-names(which(Collections==0))

# Subset UnitsFrame so it only includes Candidate Units
# Remove all rows from UnitsFrame with blank "strat_name_long" columns
UnitsFrame<-UnitsFrame[which(nchar(as.character(UnitsFrame[,"strat_name_long"]))>0),]
CandidatesFrame<-UnitsFrame[which(as.character(UnitsFrame[,"strat_name_long"])%in%CandidateUnits),]

# Join the territory names which intersect with the unit locations to CandidatesFrame
CandidatesFrame<-merge(CandidatesFrame,LocationTuples, by="col_id",all.x="TRUE")
CandidatesFrame<-unique(CandidatesFrame)

# Extracts columns of interest
CandidatesFrame<-CandidatesFrame[,c("strat_name_long","col_id","name")]

# Sort CandidatesFrame data by col_id and return state/territory name for each 
ColumnStates<-by(CandidatesFrame,CandidatesFrame[,"col_id"], function (x) unique(x[,"name"]))
  
OutputFrame<-subset(CandidatesFrame,CandidatesFrame[,"strat_name_long"]%in%unique(OutputData[,"UnitName"])
UnitStates<-by(OutputFrame, OutputFrame[,"strat_name_long"],function(x) unique(x[,"name"]))
             
for (NAME in names(UnitStates)) {
    Documents<-subset(OutputData,OutputData[,"UnitName"]==NAME)[,"DocID"]
    for (DOCID in DOCUMENTS) {
      DocumentSubset<-subset(DeepDiveData,DeepDiveData[,"docid"]==DOCID)
        for (STATE in UnitStates[[NAME]]) {
          grep(STATE,DocumentSubset[,"words"],perl=TRUE)
          }
      }
  }

Test<-merge(OutputData,CandidatesFrame, by="strat_name_long", all.x=TRUE)  
  
goodFunction<-function(SubsetDeepDive,Document=Test[,"DocID"], location=Test[,"name"]){
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Document)
    CleanedWords<-gsub(","," ",DeepDive[,"words"])
    LocationHits<-grep(location, CleanedWords[,"words"],perl=TRUE)
    return(LocationHits)
    }
  
  
Test2<-mapply(goodFunction,SubsetDeepDive, Test[,"DocID"], Test[,"name"])

    
