# Load Libraries
library("data.table")
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
CandidatesFrame<-CandidatesFrame[,c("strat_name_long","col_id","location")]

# Sort CandidatesFrame data by col_id and return state/territory name for each 
ColumnStates<-by(CandidatesFrame,CandidatesFrame[,"col_id"], function (x) unique(x[,"location"]))
  
OutputFrame<-subset(CandidatesFrame,CandidatesFrame[,"strat_name_long"]%in%unique(OutputData[,"UnitName"]))
UnitStates<-by(OutputFrame, OutputFrame[,"strat_name_long"],function(x) unique(x[,"location"]))
  
  
# rename the output data column of unit names so OutputData can be merged with location data for the units
colnames(OutputData)[1]<-"strat_name_long"
# Create a table of unit data that has the unit name, docid of the match, and the location(s) the unit is known to be in.
UnitOutputData<-merge(OutputData,CandidatesFrame, by="strat_name_long", all.x=TRUE)
# Remove the columns that are not needed
UnitOutputData<-UnitOutputData[,c("strat_name_long","DocID","SentID","location")]
  
locationSearch<-function(SubsetDeepDive,Document=UnitOutputData[,"DocID"], location=UnitOutputData[,"location"]){
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Document)
    CleanedWords<-gsub(","," ",DeepDive[,"words"])
    LocationHits<-sapply(location, function (x,y) grep (x,y, ignore.case=TRUE,perl=TRUE), CleanedWords)
    LocationHitsLength<-sapply(LocationHits,length)
    Location<-rep(names(LocationHits),times=LocationHitsLength)
    LocationDocs<-DeepDive[unlist(LocationHits),"docid"]
    return(cbind(LocationDocs,Location))
    }
                         
LocationHits<-locationSearch(SubsetDeepDive,Document=UnitOutputData[,"DocID"], location=UnitOutputData[,"location"])
LocationHits<-unique(LocationHits)
                         
# Convert UnitOutputData into a matrix of just docid and location names
UnitOutputData<-as.matrix(UnitOutputData[,c("DocID","location")])
# Find the rows from UnitOutputData that are also in LocationHits to varify that the correct location appears in the document with the unit assocoiated with the location.
UnitOutputData[which(UnitOutputData%in%LocationHits),]

    
