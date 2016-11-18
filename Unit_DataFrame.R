# NOTE: Refine the output prior to running this stage.
# Load libraries
library("RCurl")
library("RPostgreSQL")

# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

#Load all of the candidate units (marine, sedimentary, unfossiliferous) that were matched in the documents
UnitHitData<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_18_2016/UnitHitData.rds")
# Extract all unit name matches
MatrixUnits<-unique(UnitHitData[,"UnitName"])
# Subset UnitsFrame to only include units from UnitHitData
SubsetUnitsFrame<-UnitsFrame[which(UnitsFrame[,"strat_name_long"]%in%MatrixUnits),]
	
############################################# CREATE LOCATION COLUMNS ####################################################	
	
# Create a vector of all the state/location names
# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")
# Load intersected location tuples table 
LocationTuples<-dbGetQuery(Connection,"SELECT* FROM column_locations.intersections") 

# Group LocationTuple data by "col_id" column
GroupedLocationsList<-tapply(LocationTuples[,"location"],LocationTuples[,"col_id"],as.character)
# Collapse the elements for each "col_id" in the list so that a vector is returned
GroupedLocations<-setNames(as.data.frame(sapply(GroupedLocationsList, function(x) paste(x, collapse=' '))),"States")
# Convert the GroupedLocations column to character data
GroupedLocations[,"States"]<-as.character(GroupedLocations[,"States"])
  
# merge the GroupedLocationsData to SubsetUnitsFrame by col_id
SubsetUnitsFrame<-merge(SubsetUnitsFrame,GroupedLocations,by.x="col_id",by.y="row.names", all.x=TRUE)
  
# Create a vector of locations
Locations<-unique(LocationTuples[,"location"])
# Remove blanks
Locations<-na.omit(Locations)
# Create a matrix showing whether or not each location corresponds with each row or SubsetUnitsFrame[,"GroupedLocations"]
LocationMatrix<-sapply(Locations,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),as.character(SubsetUnitsFrame[,"States"]))
# Convert the logical data into numerical data
LocationMatrix[,1:ncol(LocationMatrix)]<-as.numeric(LocationMatrix[,1:ncol(LocationMatrix)])
  
# Name final matrix
UnitDataTable<-data.matrix(LocationMatrix)
rownames(UnitDataTable)<-as.character(SubsetUnitsFrame[,"unit_id"])
	
############################################# CREATE LITHOLOGY COLUMNS ###################################################

# Create a vector of lithology categories from SubsetUnitsFrame
Lithologies<-(c("amphibolite","ash","andesite","argillite","arkose","basalt","breccia","chalk","chert","clay","coal","conglomerate",
"dacite","diamictite","dolomite","gabbro","gneiss","gravel","graywacke","greywacke","evaporite","lignite","limestone","marble",
"marl","mudstone","oolitic limestone","phosophorite","phyllite","quartzite","rhyolite","sand ","sandstone","schist","shale",
"siliciclastic","silt ","siltstone","silty clay","silty sand ","skeletal silt","slate","tuff","volcanic"))

# Create a matrix showing whether or not each lithology category corresponds with each row of SubsetUnitsFrame[,"lith"]
LithMatrix<-sapply(Lithologies,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),SubsetUnitsFrame[,"lith"])
# Convert the logical data into numerical data
LithMatrix[,1:ncol(LithMatrix)]<-as.numeric(LithMatrix[,1:ncol(LithMatrix)])

# Bind the LithMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,LithMatrix))

############################################## CREATE TIME COLUMNS ###################################################
	
# Load communityMatrix.R modgule of paleobiology database r package
source("https://raw.githubusercontent.com/aazaff/paleobiologyDatabase.R/master/communityMatrix.R")
	
Periods<-downloadTime("international%20periods")
	
# create a matrix showing whether or not each period from Periods corresponds with each unit of SubsetUnitsFrame  
multipleAges<-function(SubsetUnitsFrame,Periods) {
	FinalMatrix<-matrix(0,nrow=nrow(SubsetUnitsFrame),ncol=nrow(Periods))
	SubsetUnitsFrame[,"b_int_name"]<-as.character(SubsetUnitsFrame[,"b_int_name"])
	SubsetUnitsFrame[,"t_int_name"]<-as.character(SubsetUnitsFrame[,"t_int_name"])
	colnames(FinalMatrix)<-Periods[,"name"]
	for (i in 1:nrow(Periods)) {
		  EarlyPos<-which(SubsetUnitsFrame[,"t_int_age"]>Periods[i,"t_age"] & SubsetUnitsFrame[,"t_int_age"]<=Periods[i,"b_age"])
		  SubsetUnitsFrame[EarlyPos,"b_int_name"]<-as.character(Periods[i,"name"])
 		  LatePos<-which(SubsetUnitsFrame[,"b_int_age"]>=Periods[i,"t_age"] & SubsetUnitsFrame[,"b_int_age"]<Periods[i,"b_age"])
 		  SubsetUnitsFrame[LatePos,"t_int_name"]<-as.character(Periods[i,"name"])
 		  }
	EarlyInterval<-match(SubsetUnitsFrame[,"b_int_name"],colnames(FinalMatrix))
 	LateInterval<-match(SubsetUnitsFrame[,"t_int_name"],colnames(FinalMatrix))
 	Positions<-rbind(cbind(1:nrow(FinalMatrix),EarlyInterval),cbind(1:nrow(FinalMatrix),LateInterval))
 	FinalMatrix[Positions]<-1
	return(FinalMatrix)
	}
 
AgeMatrix<-multipleAges(SubsetUnitsFrame,Periods)
	
# Account for the units which span more than two periods in age
Distance<-apply(AgeMatrix,1,function(x) diff(which(x==1))>1)
Separated<-which(Distance==TRUE)

# Assign the value 1 for the periods which fall between the earliest and latest period
for(i in Separated){
	AgeMatrix[i,which(AgeMatrix[i,]==1)[1]:which(AgeMatrix[i,]==1)[2]]<-1
	}	
	
# Bind the AgeMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,AgeMatrix))

##################################### ADD ENVIRONMENTS COLUMNS ###########################################

# download a list of environmnets from Macrostrat database
EnvironsURL<-"https://macrostrat.org/api/defs/environments?all&format=csv"
GotURL<-getURL(EnvironsURL)
EnvironsFrame<-read.csv(text=GotURL,header=TRUE)
Environments<-unique(EnvironsFrame[,"name"])
	
# Create a matrix showing whether or not each environment category corresponds with each row of SubsetUnitsFrame[,"environ"]
EnvironMatrix<-sapply(Environments,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),SubsetUnitsFrame[,"environ"])
# assign column names
colnames(EnvironMatrix)<-Environments

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))
	
# UnitDataTable[which(UnitDataTable[,"non-marine"]=="1"),"marine"]<-0
	
###################### Find which rows in the UnitDataTable were found in the cleaned app output ########################

# Load CleanedOutput
CleadedOutput<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_2_2016/OutputRefined/CleanedOutput.rds")
# Extract the unit names from CleanedOutput (units which have fossils according to app)
CleanedOutputUnits<-unique(CleanedOutput[,"strat_name_long"])
# Find the unit_id for each unit in cleaned output using SubsetUnitsFrame
OutputUnitID<-SubsetUnitsFrame[which(SubsetUnitsFrame[,"strat_name_long"]%in%CleanedOutputUnits),"unit_id"]
# Find which unit_ids associated with units in CleanedOutput are in UnitDataTable
FossilUnitIDs<-rownames(UnitDataTable[which(rownames(UnitDataTable)%in%OutputUnitID),])
	
