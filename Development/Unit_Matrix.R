# Load libraries
library("RCurl")
library("RPostgreSQL")

# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Subset UnitsFrame to only include units from UnitHitData
MatrixUnits<-unique(UnitHitData[,"UnitName"])
SubsetUnitsFrame<-UnitsFrame[which(UnitsFrame[,"strat_name_long"]%in%MatrixUnits),]

# Create a vector of lithology categories from SubsetUnitsFrame
Lithologies<-(c("amphibolite","ash","andesite","argillite","arkose","basalt","breccia","chalk","chert","clay","coal","conglomerate",
"dacite","diamictite","dolomite","gabbro","gneiss","gravel","graywacke","greywacke","evaporite","lignite","limestone","marble",
"marl","mudstone","oolitic limestone","phosophorite","phyllite","quartzite","rhyolite","sand ","sandstone","schist","shale",
"siliciclastic","silt","siltstone","silty clay","silty sand","skeletal silt ","slate","tuff","volcanic"))

# Create a matrix showing whether or not each lithology category corresponds with each row of SubsetUnitsFrame[,"lith"]
LithMatrix<-sapply(Lithologies,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),SubsetUnitsFrame[,"lith"])
# Convert the logical data into numerical data
LithMatrix[,1:ncol(LithMatrix)]<-as.numeric(LithMatrix[,1:ncol(LithMatrix)])

# Bind the LithMatrix to the "strat_name_long" column of SubsetUnitsFrame
UnitDataTable<-as.data.frame(cbind(as.character(SubsetUnitsFrame[,"strat_name_long"]),LithMatrix))

# Create a vector of all the state/location names
# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")
# Load intersected location tuples table 
LocationTuples<-dbGetQuery(Connection,"SELECT* FROM column_locations.intersections") 

# Group LocationTuple data by "col_id" column
GroupedLocationsList<-tapply(LocationTuples[,"location"],LocationTuples[,"col_id"],as.character)
# Collapse the elements for each "col_id" in the list so that a vector is returned
GroupedLocations<-sapply(GroupedLocations, function(x) paste(x, collapse=' '))
# make a vector of each respective col_id for the collapsed lcoations
col_id<-as.numeric(names(GroupedLocationsVector))
# bind the ColID vector with the GroupedLocations vector
ColIDLocationData<-as.data.frame(cbind(col_id,GroupedLocations))
# Convert the GroupedLocations column to character data
ColIDLocationData[,"GroupedLocations"]<-as.character(ColIDLocationData[,"GroupedLocations"])
  
# merge the GroupedLocationsData to SubsetUnitsFrame by col_id
SubsetUnitsFrame<-merge(SubsetUnitsFrame,ColIDLocationData,by="col_id", all.x=TRUE)
  
# Create a vector of locations
Locations<-unique(LocationTuples[,"location"])
# Remove blanks
Locations<-na.omit(Locations)
# Create a matrix showing whether or not each location corresponds with each row or SubsetUnitsFrame[,"GroupedLocations"]
LocationMatrix<-sapply(Locations,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),as.character(SubsetUnitsFrame[,"GroupedLocations"]))
# Convert the logical data into numerical data
LocationMatrix[,1:ncol(LocationMatrix)]<-as.numeric(LocationMatrix[,1:ncol(LocationMatrix)])
  
# Bind the LocationMatrix to LithMatrix
UnitDataTable<-as.data.frame(cbind(LithMatrix,LocationMatrix))
