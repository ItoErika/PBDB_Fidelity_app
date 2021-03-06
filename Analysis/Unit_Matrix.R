# NOTE: Refine the output prior to running this stage.
# Load libraries
library("RCurl")
library("RPostgreSQL")
library("velociraptr")

# Download all sedimentary unit names from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

#Load all of the candidate units (unfossiliferous, sedimentary) that were matched in the documents
MatchData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/MatchData.csv", row.names=1)
# Load the fidelity output with noisy words and phrases cleaned out
NoNoiseOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoNoiseOutput.csv")
# Load output with no trace hits
NoTraceOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoTraceOutput.csv")
# Load output with no micro hits
NoMicroOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoMicroOutput.csv")
# Load output with no trace hits and no micro hits
NoMicroNoTraceOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoMicroNoTraceOutput.csv")

# Remove "White Dolomite" "Sandsuck Shale", "Cochran Formation", "Cambridge Argillite", and "Deep Spring Formation" from MatchData
MatchData<-MatchData[-which(MatchData[,"Formation"]=="White Dolomite"|MatchData[,"Formation"]=="Sandsuck Shale"|MatchData[,"Formation"]=="Cochran Formation"|MatchData[,"Formation"]=="Cambridge Argillite"|MatchData[,"Formation"]=="Deep Spring Formation"),]

# Create a temporary column of strat name | col_id tuples in NoNoiseOutput, NoTraceOutput, NoMicroOutput, and in MatchData
NoNoiseOutput[,"Name_ColID"]<-paste(NoNoiseOutput[,"Formation"],NoNoiseOutput[,"col_id"], sep="|")
NoMicroOutput[,"Name_ColID"]<-paste(NoMicroOutput[,"Formation"],NoMicroOutput[,"col_id"], sep="|")
NoTraceOutput[,"Name_ColID"]<-paste(NoTraceOutput[,"Formation"],NoTraceOutput[,"col_id"], sep="|")
MatchData[,"Name_ColID"]<-paste(MatchData[,"Formation"], MatchData[,"col_id"], sep="|")

# Add a column to MatchData showing if the formation was captured in the final NoNoiseOutput
MatchData[which(MatchData[,"Name_ColID"]%in%NoNoiseOutput[,"Name_ColID"]),"GDD_occ"]<-"TRUE"
MatchData[which(is.na(MatchData[,"GDD_occ"])),"GDD_occ"]<-"FALSE"

# Add a column to MatchData showing if the formation appeared in sentences with the word "micro", "spore", "foram", 
# "radiolaria", "graptolite", "conodont", "diatom", "coccolith", "pollen" or "acritarch"
# Tag formations in NoNoiseOutput which are NOT found in the NoMicroOutput as TRUE
MicroFormations<-unique(as.character(NoNoiseOutput[which(!(NoNoiseOutput[,"Name_ColID"]%in%NoMicroOutput[,"Name_ColID"])),"Name_ColID"]))
MatchData[which(as.character(MatchData[,"Name_ColID"])%in%MicroFormations),"Micro"]<-"TRUE"

# Add a column to MatchData showing if the formation appeared in sentences with the words "trace fossil", "ichno", "burrow", or "trackway"
# Tag formations in NoNoiseOutput which are NOT found in the NoTraceOutput as TRUE
TraceFormations<-unique(as.character(NoNoiseOutput[which(!(NoNoiseOutput[,"Name_ColID"]%in%NoTraceOutput[,"Name_ColID"])),"Name_ColID"]))
MatchData[which(as.character(MatchData[,"Name_ColID"])%in%TraceFormations),"Trace"]<-"TRUE"

# Extract all unit name | col_id matches
MatrixUnits<-unique(MatchData[,"Name_ColID"])
# Add a column of strat name | col_id tuples to UnitsFrame
UnitsFrame[,"Name_ColID"]<-paste(UnitsFrame[,"strat_name_long"], UnitsFrame[,"col_id"], sep="|")
# Subset UnitsFrame to only include units from MatchData
SubsetUnitsFrame<-UnitsFrame[which(UnitsFrame[,"Name_ColID"]%in%MatrixUnits),]

# Attach unit_id data to MatchData
# Merge unit_id data to MatchData, and remove the temporary column
MatchData<-merge(MatchData, unique(SubsetUnitsFrame[,c("unit_id","Name_ColID")]), by="Name_ColID")
MatrixData<-MatchData[,c("docid", "col_id", "Formation", "sentid", "SubsetDDRow", "PBDB_occ", "col_locations", "doc_locations", "GDD_occ", "Micro", "Trace", "unit_id")]

# Save MatrixData
# write.csv(MatrixData, "~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/MatrixData.csv", row.names=FALSE)

########################################### CLEAN SUBSETUNITSFRAME COLUMNS ##############################################

# Make sure there is a space before and after every word in the lithology column of SubsetUnitsFrame
SubsetUnitsFrame[,"lith"]<-gsub("\\|", " | ", SubsetUnitsFrame[,"lith"])
# Add as space to the end of every lithology column row
SubsetUnitsFrame[,"lith"]<-paste(SubsetUnitsFrame[,"lith"]," ", sep="")
# Make sure there is a space before and after every word in the environment column of SubsetUnitsFrame
SubsetUnitsFrame[,"environ"]<-gsub("\\|", " | ", SubsetUnitsFrame[,"environ"])
# Add as space before of every environment column row
SubsetUnitsFrame[,"environ"]<-paste(" ", SubsetUnitsFrame[,"environ"], sep="")

############################################# CREATE LOCATION COLUMNS ####################################################	
	
# Create a vector of all the state/location names
# Load intersected location tuples table 
LocationTuples<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/PBDB_Fidelity_app/input/LocationTuples.csv")

# Group LocationTuple data by "col_id" column
GroupedLocations<-tapply(LocationTuples[,"name"],LocationTuples[,"col_id"],as.character)
# Collapse the elements for each "col_id" in the list so that a vector is returned
GroupedLocations<-setNames(as.data.frame(sapply(GroupedLocations, function(x) paste(x, collapse=' '))),"Locations")
# Convert the GroupedLocations column to character data
GroupedLocations[,"Locations"]<-as.character(GroupedLocations[,"Locations"])
  
# merge the GroupedLocationsData to SubsetUnitsFrame by col_id
SubsetUnitsFrame<-merge(SubsetUnitsFrame, GroupedLocations, by.x="col_id", by.y="row.names", all.x=TRUE)
  
# Create a vector of locations
Locations<-unique(LocationTuples[,"name"])
# Remove blanks
Locations<-Locations[which(Locations!="")]
# Create a matrix showing whether or not each location corresponds with each row in SubsetUnitsFrame
LocationMatrix<-sapply(Locations, function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE), as.character(SubsetUnitsFrame[,"Locations"]))
# Convert the logical data into numerical data
LocationMatrix[,1:ncol(LocationMatrix)]<-as.numeric(LocationMatrix[,1:ncol(LocationMatrix)])
colnames(LocationMatrix)<-paste("location_", Locations, sep="")
	
# Name final matrix
FormationMatrix<-data.matrix(LocationMatrix)
rownames(FormationMatrix)<-as.character(SubsetUnitsFrame[,"unit_id"])
	
############################################# CREATE LITHOLOGY COLUMNS ###################################################
#################################################### LITHOLOGY NAME ######################################################

# download a list of lithologies from Macrostrat database
LithologyURL<-"https://macrostrat.org/api/defs/lithologies?all&format=csv"
GotURL<-getURL(LithologyURL)
LithologyFrame<-read.csv(text=GotURL,header=TRUE)
Lithologies<-unique(LithologyFrame[,"name"])
	
# Add a space at the beginning and end of each lithology name to prepare for grep
LithologiesWS<-paste(" ", Lithologies, sep="")
LithologiesWS<-paste(LithologiesWS, " ", sep="")

# Create a matrix showing whether or not each lithology category corresponds with each row of SubsetUnitsFrame[,"lith"]
LithNameMatrix<-sapply(LithologiesWS,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
# Assign column names
colnames(LithNameMatrix)<-paste("lithname_", Lithologies, sep="")
# Convert the logical data into numerical data
LithNameMatrix[,1:ncol(LithNameMatrix)]<-as.numeric(LithNameMatrix[,1:ncol(LithNameMatrix)])

# Bind the LithNameMatrix to FormationMatrix
FormationMatrix<-data.matrix(cbind(FormationMatrix,LithNameMatrix))
	
############################################### LITHOLOGY TYPE ###############################################

# Group lithology names into their associated types based on the Macrostrat API (NOTE: this function creates a list)
LithTypes<-split(LithologyFrame[,"name"],LithologyFrame[,"type"])
# Add spaces before and after each lithology name
LithTypes<-sapply(LithTypes, function(x) paste(x, " ", sep=""))
LithTypes<-sapply(LithTypes, function(x) paste(" ", x, sep=""))	

# Make vectors of lithology names associated with each type
Carbonate<-as.character(LithTypes[["carbonate"]])
Cataclastic<-c(" cataclastic ", as.character(LithTypes[["cataclastic"]]))
Chemical<-c(" chemical ", as.character(LithTypes[["chemical"]]))
Evaporite<-as.character(LithTypes[["evaporite"]])
Igneous<-as.character(LithTypes[["igneous"]])
Metaigneous<-c(" metaigneous ", as.character(LithTypes[["metaigneous"]]))
Metamorphic<-as.character(LithTypes[["metamorphic"]])
Metasedimentary<-as.character(LithTypes[["metasedimentary"]])
Metavolcanic<-as.character(LithTypes[["metavolcanic"]])
Organic<-c(" organic ", as.character(LithTypes[["organic"]]))
Plutonic<-as.character(LithTypes[["plutonic"]])
Regolith<-as.character(LithTypes[["regolith"]])
Sedimentary<-as.character(LithTypes[["sedimentary"]])
Siliciclastic<-as.character(LithTypes[["siliciclastic"]])
Volcanic<-as.character(LithTypes[["volcanic"]])

# Run greps for all of the words in each type
CarbonateMatrix<-sapply(Carbonate,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
CataclasticMatrix<-sapply(Cataclastic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
ChemicalMatrix<-sapply(Chemical,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
EvaporiteMatrix<-sapply(Evaporite,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
IgneousMatrix<-sapply(Igneous,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
MetaigneousMatrix<-sapply(Metaigneous,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
MetamorphicMatrix<-sapply(Metamorphic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
MetasedimentaryMatrix<-sapply(Metasedimentary,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
MetavolcanicMatrix<-sapply(Metavolcanic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
OrganicMatrix<-sapply(Organic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
PlutonicMatrix<-sapply(Plutonic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
RegolithMatrix<-sapply(Regolith,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
SedimentaryMatrix<-sapply(Sedimentary,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
SiliciclasticMatrix<-sapply(Siliciclastic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
VolcanicMatrix<-sapply(Volcanic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])

# Find if any of the names within each type had a hit in the grepl search
carbonate<-apply(CarbonateMatrix, 1, function(x) any(x)==TRUE)
cataclastic<-apply(CataclasticMatrix, 1, function(x) any(x)==TRUE)
chemical<-apply(ChemicalMatrix, 1, function(x) any(x)==TRUE)
evaporite<-apply(EvaporiteMatrix, 1, function(x) any(x)==TRUE)
igneous<-apply(IgneousMatrix, 1, function(x) any(x)==TRUE)
metaigneous<-apply(MetaigneousMatrix, 1, function(x) any(x)==TRUE)
metamorphic<-apply(MetamorphicMatrix, 1, function(x) any(x)==TRUE)
metasedimentary<-apply(MetasedimentaryMatrix, 1, function(x) any(x)==TRUE)
metavolcanic<-apply(MetavolcanicMatrix, 1, function(x) any(x)==TRUE)
organic<-apply(OrganicMatrix, 1, function(x) any(x)==TRUE)
plutonic<-apply(PlutonicMatrix, 1, function(x) any(x)==TRUE)
regolith<-apply(RegolithMatrix, 1, function(x) any(x)==TRUE)
sedimentary<-apply(SedimentaryMatrix, 1, function(x) any(x)==TRUE)
siliciclastic<-apply(SiliciclasticMatrix, 1, function(x) any(x)==TRUE)
volcanic<-apply(VolcanicMatrix, 1, function(x) any(x)==TRUE)

# Create a single matrix for lithology types
LithTypeMatrix<-data.matrix(cbind(carbonate, cataclastic, chemical, evaporite, igneous, metaigneous, metamorphic,
metasedimentary, metavolcanic, organic, plutonic, regolith, sedimentary, siliciclastic, volcanic))

# Assign column names
colnames(LithTypeMatrix)<-paste("lithtype_", colnames(LithTypeMatrix), sep="")
	
FormationMatrix<-data.matrix(cbind(FormationMatrix, LithTypeMatrix))
	
############################################### CREATE TIME COLUMNS ###################################################
##################################################### PERIODS #########################################################

Periods<-downloadTime("international%20periods")
# Remove Precambrian ages
Periods<-Periods[which(Periods[,"b_age"]<=541.000),]
	
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
 
PeriodMatrix<-multipleAges(SubsetUnitsFrame,Periods)
	
# Account for the units which span more than two periods in age
Spanned<-apply(PeriodMatrix, 1, function(x) diff(which(x==1))>1)
Separated<-which(Spanned==TRUE)

# Assign the value 1 for the periods which fall between the earliest and latest period
for(i in Separated){
	PeriodMatrix[i,which(PeriodMatrix[i,]==1)[1]:which(PeriodMatrix[i,]==1)[2]]<-1
	}
	
# Assign column names
colnames(PeriodMatrix)<-paste("period_", colnames(PeriodMatrix), sep="")
	
# Bind the PeriodMatrix to FormationMatrix
FormationMatrix<-data.matrix(cbind(FormationMatrix,PeriodMatrix))
	
################################################### EPOCHS ######################################################

Epochs<-downloadTime("international%20epochs")
	
# create a matrix showing whether or not each epoch from epochs corresponds with each unit of SubsetUnitsFrame  
multipleAges<-function(SubsetUnitsFrame,Epochs) {
	FinalMatrix<-matrix(0,nrow=nrow(SubsetUnitsFrame),ncol=nrow(Epochs))
	SubsetUnitsFrame[,"b_int_name"]<-as.character(SubsetUnitsFrame[,"b_int_name"])
	SubsetUnitsFrame[,"t_int_name"]<-as.character(SubsetUnitsFrame[,"t_int_name"])
	colnames(FinalMatrix)<-Epochs[,"name"]
	for (i in 1:nrow(Epochs)) {
		  EarlyPos<-which(SubsetUnitsFrame[,"t_int_age"]>Epochs[i,"t_age"] & SubsetUnitsFrame[,"t_int_age"]<=Epochs[i,"b_age"])
		  SubsetUnitsFrame[EarlyPos,"b_int_name"]<-as.character(Epochs[i,"name"])
 		  LatePos<-which(SubsetUnitsFrame[,"b_int_age"]>=Epochs[i,"t_age"] & SubsetUnitsFrame[,"b_int_age"]<Epochs[i,"b_age"])
 		  SubsetUnitsFrame[LatePos,"t_int_name"]<-as.character(Epochs[i,"name"])
 		  }
	EarlyInterval<-match(SubsetUnitsFrame[,"b_int_name"],colnames(FinalMatrix))
 	LateInterval<-match(SubsetUnitsFrame[,"t_int_name"],colnames(FinalMatrix))
 	Positions<-rbind(cbind(1:nrow(FinalMatrix),EarlyInterval),cbind(1:nrow(FinalMatrix),LateInterval))
 	FinalMatrix[Positions]<-1
	return(FinalMatrix)
	}
	
EpochMatrix<-multipleAges(SubsetUnitsFrame,Epochs)
	
# Account for the units which span more than two epochs in age
Spanned<-apply(EpochMatrix,1,function(x) diff(which(x==1))>1)
Separated<-which(Spanned==TRUE)
	
# Assign the value 1 for the epochs which fall between the earliest and latest epoch
for(i in Separated){
	EpochMatrix[i,which(EpochMatrix[i,]==1)[1]:which(EpochMatrix[i,]==1)[2]]<-1
	}	
	
# Assign column names
colnames(EpochMatrix)<-paste("epoch_", colnames(EpochMatrix), sep="")
	
# Bind the EpochMatrix to FormationMatrix
FormationMatrix<-data.matrix(cbind(FormationMatrix, EpochMatrix))

########################################### ADD ENVIRONMENTS COLUMNS ################################################
############################################## ENVIRONMENT NAME #####################################################
EnvironsURL<-"https://macrostrat.org/api/defs/environments?all&format=csv"
GotURL<-getURL(EnvironsURL)
EnvironsFrame<-read.csv(text=GotURL,header=TRUE)	    
# Extract distinct environment names from EnvironsFrame
EnvironNames<-unique(EnvironsFrame[,"name"])	       
	       
# Add a space at the beginning and end of each environment name to prepare for grep
EnvironNamesWS<-paste(" ", EnvironNames, sep="")
EnvironNamesWS<-paste(EnvironNamesWS, " ", sep="")

# Create a matrix showing whether or not each environment name corresponds with each row of SubsetUnitsFrame[,"environ"]
EnvironNameMatrix<-sapply(EnvironNamesWS, function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
# Assign column names
colnames(EnvironNameMatrix)<-paste("environname_", EnvironNames, sep="")
# Convert the logical data into numerical data
EnvironNameMatrix[,1:ncol(EnvironNameMatrix)]<-as.numeric(EnvironNameMatrix[,1:ncol(EnvironNameMatrix)])

# Bind the EnvironNameMatrix to FormationMatrix
FormationMatrix<-data.matrix(cbind(FormationMatrix,EnvironNameMatrix))	       
	       
############################################## ENVIRONMENT TYPE #####################################################

# Group environment names into their associated types based on the Macrostrat API (NOTE: this function creates a list)
EnvironTypes<-split(EnvironsFrame[,"name"],EnvironsFrame[,"type"])
# Add spaces before and after each environ name
EnvironTypes<-sapply(EnvironTypes, function(x) paste(x, " ", sep=""))
EnvironTypes<-sapply(EnvironTypes, function(x) paste(" ", x, sep=""))	
	
# Make a vector of environment names associated with each type
Carbonate<-c(" carbonate ", as.character(EnvironTypes[["carbonate"]]))
Eolian<-c(" eolian ", as.character(EnvironTypes[["eolian"]]))
Fluvial<-c(" fluvial ", as.character(EnvironTypes[["fluvial"]]))
Glacial<-c(" glacial ", as.character(EnvironTypes[["glacial"]]))
Lacustrine<-c(" lacustrine ", as.character(EnvironTypes[["lacustrine"]]))
Landscape<-c(" landscape ", as.character(EnvironTypes[["landscape"]]))
Siliciclastic<-c(" siliciclastic ", as.character(EnvironTypes[["siliciclastic"]]))

# Run greps for all of the names in each type
CarbonateMatrix<-sapply(Carbonate, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
EolianMatrix<-sapply(Eolian, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
FluvialMatrix<-sapply(Fluvial, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
GlacialMatrix<-sapply(Glacial, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
LacustrineMatrix<-sapply(Lacustrine, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
LandscapeMatrix<-sapply(Landscape, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
SiliciclasticMatrix<-sapply(Siliciclastic, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
		
# Find if any of the names within each type had a hit in the grepl search
carbonate<-apply(CarbonateMatrix, 1, function(x) any(x)==TRUE)
eolian<-apply(EolianMatrix, 1, function(x) any(x)==TRUE)
fluvial<-apply(FluvialMatrix, 1, function(x) any(x)==TRUE)
glacial<-apply(GlacialMatrix, 1, function(x) any(x)==TRUE)
lacustrine<-apply(LacustrineMatrix, 1, function(x) any(x)==TRUE)
landscape<-apply(LandscapeMatrix, 1, function(x) any(x)==TRUE)
siliciclastic<-apply(SiliciclasticMatrix, 1, function(x) any(x)==TRUE)	
	
# Create a single matrix for environment types
EnvironTypeMatrix<-data.matrix(cbind(carbonate, eolian, fluvial, glacial, lacustrine, landscape, siliciclastic))
	
# Assign column names
colnames(EnvironTypeMatrix)<-paste("environtype_",colnames(EnvironTypeMatrix), sep="")

FormationMatrix<-data.matrix(cbind(FormationMatrix, EnvironTypeMatrix))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             	                                                                                       
####################################### ENVIRONMENT CLASS ###############################################

# Group environment types into their associated classes based on the Macrostrat API (NOTE: this function creates a list)
EnvironClasses<-split(EnvironsFrame[,"type"],EnvironsFrame[,"class"])
EnvironClasses[["non-marine"]]<-unique(EnvironClasses[["non-marine"]])
EnvironClasses[["marine"]]<-unique(EnvironClasses[["marine"]])

# Make a vector of environment names and types associated with each class
Marine<-c(" marine ", Carbonate, Siliciclastic)
NonMarine<-c(" non-marine ", Eolian, Fluvial, Glacial, Lacustrine, Landscape)
	
# Run greps for all of the names and types in each class
MarineMatrix<-sapply(Marine, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])
NonMarineMatrix<-sapply(NonMarine, function(x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE), SubsetUnitsFrame[,"environ"])

# Find if any of the names and types within each class had a hit in the grepl search
marine<-apply(MarineMatrix, 1, function(x) any(x)==TRUE)
nonmarine<-apply(NonMarineMatrix, 1, function(x) any(x)==TRUE)
	
# Create a single matrix for environment classes
EnvironClassMatrix<-data.matrix(cbind(marine, nonmarine))
	
# Assign column names
colnames(EnvironClassMatrix)<-paste("environclass_",colnames(EnvironClassMatrix), sep="")
	
FormationMatrix<-data.matrix(cbind(FormationMatrix, EnvironClassMatrix))
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
##################################### rownames as strat_name_long option ################################################

# Sort UnitDataTable by unit id
SortUnitMatrix<-UnitDataTable[order(as.numeric(as.character(row.names(UnitDataTable)))),]
# Sort SubsetUnitsFrame by unti id
SortSubsetUnitsFrame<-SubsetUnitsFrame[order(as.numeric(as.character(SubsetUnitsFrame[,"unit_id"]))),]
# extract strat_name_long column from SortSubsetUnitsFrame in the same order as associated unit ids in SortUnitMatrix
strat_name_long<-as.character(SortSubsetUnitsFrame[,"strat_name_long"])	
	
UnitMatrix<-by(SortUnitMatrix,strat_name_long,function(x) apply(x,2,max))
UnitMatrix<-do.call(rbind,UnitMatrix)
	
############################################## UNIT MATRIX ANALYSIS ####################################################
library("RCurl")

# Download official geologic time scale colors from macrostrat
ColorsURL<-"https://macrostrat.org/api/defs/intervals?true_clors&format=csv"
GotURL<-getURL(ColorsURL)
TimeScaleColors<-read.csv(text=GotURL,header=TRUE)

# load unit matrix with strat_name_long row names and no lumping of columns
UnitMatrix<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_18_2016/UnitMatrix.rds")
# load most recent CleanedOutput
CleanedOutput<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_18_2016/CleanedOutput.rds")

# create a version of unit matrix which only has strat names that are in the cleaned output
OutputUnitMatrix<-UnitMatrix[which(rownames(UnitMatrix)%in%CleanedOutput[,"strat_name_long"]),]


################################################### EPOCHS BAR PLOT ##################################################

# download epoch names
source("https://raw.githubusercontent.com/aazaff/paleobiologyDatabase.R/master/communityMatrix.R")
Epochs<-downloadTime("international%20epochs")

# subset OutputUnitMatrix to only epoch columns (NOTE: rownames of Epochs are epoch names)
EpochOutputMatrix<-OutputUnitMatrix[,rownames(Epochs)]

# subset TimeScaleColors to only include epochs
EpochColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Epochs))
# extract only name and color columns from EpochColors
EpochColors<-EpochColors[,c("name","color")]
# create a color palette of colors for each epoch
colors<-as.character(EpochColors[,"color"])
names(colors)<-EpochColors[,"name"]

# Make a bar plot showing the RAW NUMBER of units in the EpochOutputMatrix that fall into each epoch category
# take the sum of all of the columns of EpochOutputMatrix
EpochSums<-apply(EpochOutputMatrix,2,sum)
# make raw data bar plot
quartz(height=10,width=12)
layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))
par(oma=c(4,1,0.5,0),mar=c(3,3,2,0.5),mgp=c(1.5,0.5,0))
barplot(EpochSums, names.arg=colnames(EpochOutputMatrix),ylab="# of Fidelity Output Units",col=colors,las=2)
abline(h=mean(EpochSums),lwd=5,col="dark grey",lty=3)
	
# make a bar plot showing the PERCENTAGE of units in EpochOutputMatrix which fall into each epoch category
# subset UnitMatrix to only include epoch columns
EpochMatrix<-UnitMatrix[,rownames(Epochs)]
# find the total number of units in UnitMatrix that fall into each epoch category
EpochTotalSums<-apply(EpochMatrix,2,sum)
# divide EpochSums by EpochTotalSums
EpochPercentages<-EpochSums/EpochTotalSums
# make percentages barplot
barplot(EpochPercentages,names.arg=colnames(EpochOutputMatrix),ylab="% of Fidelity Output Units", col=colors,las=2)
abline(h=mean(EpochPercentages),lwd=5,col="dark grey",lty=3)
	
################################################### PERIODS BAR PLOT ##################################################

Periods<-downloadTime("international%20periods")
# get rid of preCambrian rows
# make a vector of preCambrian Periods
PreCambrian<-c("Ediacaran","Cryogenian","Tonian","Stenian","Ectasian","Calymmian","Statherian","Orosirian","Rhyacian",
"Siderian")
# remove PreCambrian rows
Periods<-Periods[-which(rownames(Periods)%in%PreCambrian),]

# subset OutputUnitMatrix to only epoch columns (NOTE: rownames of Periods are period names)
PeriodOutputMatrix<-OutputUnitMatrix[,rownames(Periods)]
	
# subset TimeScaleColors to only include periods
PeriodColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Periods))
# extract only name and color columns from EpochColors
PeriodColors<-PeriodColors[,c("name","color")]	
# create a color palette of colors for each period
colors<-as.character(PeriodColors[,"color"])
names(colors)<-PeriodColors[,"name"]	
	
# Make a bar plot showing the RAW NUMBER of units in the PeriodOutputMatrix that fall into each period category
# take the sum of all of the columns of PeriodOutputMatrix
PeriodSums<-apply(PeriodOutputMatrix,2,sum)
# make raw data bar plot
barplot(PeriodSums, names.arg=colnames(PeriodOutputMatrix),xlab="Period",ylab="# of Fidelity Output Units",col=colors)	
	
# make a bar plot showing the PERCENTAGE of units in PeriodOutputMatrix which fall into each period category
# subset UnitMatrix to only include period columns
PeriodMatrix<-UnitMatrix[,rownames(Periods)]
# find the total number of units in UnitMatrix that fall into each epoch category
PeriodTotalSums<-apply(PeriodMatrix,2,sum)
# divide PeriodSums by PeriodTotalSums
PeriodPercentages<-PeriodSums/PeriodTotalSums
# make percentages barplot
barplot(PeriodPercentages,names.arg=colnames(PeriodOutputMatrix),xlab="Period",ylab="% of Fidelity Output Units", col=colors)
