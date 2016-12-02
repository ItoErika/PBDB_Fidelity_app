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
############################################### UNCATEGORIZED OPTION #####################################################

# download a list of lithologies from Macrostrat database
LithologyURL<-"https://macrostrat.org/api/defs/lithologies?all&format=csv"
GotURL<-getURL(LithologyURL)
LithologyFrame<-read.csv(text=GotURL,header=TRUE)
Lithologies<-unique(LithologyFrame[,"name"])

# Create a matrix showing whether or not each lithology category corresponds with each row of SubsetUnitsFrame[,"lith"]
LithMatrix<-sapply(Lithologies,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
# Assign column names
colnames(LithMatrix)<-Lithologies
# Convert the logical data into numerical data
LithMatrix[,1:ncol(LithMatrix)]<-as.numeric(LithMatrix[,1:ncol(LithMatrix)])

# Bind the LithMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,LithMatrix))
	
######################################### CATEGORIZED LITHOLOGY OPTION ###############################################

# group lithology names into their associated types based on the Macrostrat API (NOTE: this function creates a list)
LithologyTypes<-split(LithologyFrame[,"name"],LithologyFrame[,"type"])	

# make a vector of lithology names associated with each type
carbonate<-as.character(unlist(LithologyTypes["carbonate"]))
cataclastic<-as.character(unlist(LithologyTypes["cataclastic"]))
chemical<-as.character(unlist(LithologyTypes["chemical"]))
evaporite<-as.character(unlist(LithologyTypes["evaporite"]))
igneous<-as.character(unlist(LithologyTypes["igneous"]))
metaigneous<-as.character(unlist(LithologyTypes["metaigneous"]))
metamorphic<-as.character(unlist(LithologyTypes["metamorphic"]))
metasedimentary<-as.character(unlist(LithologyTypes["metasedimentary"]))
organic<-as.character(unlist(LithologyTypes["organic"]))
plutonic<-as.character(unlist(LithologyTypes["plutonic"]))
regolith<-as.character(unlist(LithologyTypes["regolith"]))
sedimentary<-as.character(unlist(LithologyTypes["sedimentary"]))
siliciclastic<-as.character(unlist(LithologyTypes["siliciclastic"]))
volcanic<-as.character(unlist(LithologyTypes["volcanic"]))

# run greps for all of the words in each type
LithMatrix1<-sapply(carbonate,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix2<-sapply(cataclastic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix3<-sapply(chemical,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix4<-sapply(evaporite,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix5<-sapply(igneous,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix6<-sapply(metaigneous,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix7<-sapply(metamorphic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix8<-sapply(metasedimentary,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix9<-sapply(organic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix10<-sapply(plutonic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix11<-sapply(regolith,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix12<-sapply(sedimentary,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix13<-sapply(siliciclastic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])
LithMatrix14<-sapply(volcanic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"lith"])

# find if any of the names within each type had a hit in the grepl search
carbonate<-apply(LithMatrix1, 1, function(x) any(x)==TRUE)
cataclastic<-apply(LithMatrix2, 1, function(x) any(x)==TRUE)
chemical<-apply(LithMatrix3, 1, function(x) any(x)==TRUE)
evaporite<-apply(LithMatrix4, 1, function(x) any(x)==TRUE)
igneous<-apply(LithMatrix5, 1, function(x) any(x)==TRUE)
metaigneous<-apply(LithMatrix6, 1, function(x) any(x)==TRUE)
metamorphic<-apply(LithMatrix7, 1, function(x) any(x)==TRUE)
metasedimentary<-apply(LithMatrix8, 1, function(x) any(x)==TRUE)
organic<-apply(LithMatrix9, 1, function(x) any(x)==TRUE)
plutonic<-apply(LithMatrix10, 1, function(x) any(x)==TRUE)
regolith<-apply(LithMatrix11, 1, function(x) any(x)==TRUE)
sedimentary<-apply(LithMatrix12, 1, function(x) any(x)==TRUE)
siliciclastic<-apply(LithMatrix13, 1, function(x) any(x)==TRUE)
volcanic<-apply(LithMatrix14, 1, function(x) any(x)==TRUE)

LithMatrix<-data.matrix(cbind(carbonate,cataclastic,chemical,evaporite,igneous,metaigneous,metamorphic,metasedimentary,
organic,plutonic,regolith,sedimentary,siliciclastic,volcanic))
colnames(LithMatrix)<-c("carbonate","cataclastic","chemical","evaporite","igneous","metaigneous","metamorphic"
,"metasedimentary","organic","plutonic","regolith","sedimentary","siliciclastic","volcanic")
	

UnitDataTable<-data.matrix(cbind(UnitDataTable,LithMatrix))
	
############################################## CREATE TIME COLUMNS ###################################################
################################################# PERIODS OPTION #####################################################

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
 
PeriodMatrix<-multipleAges(SubsetUnitsFrame,Periods)
	
# Account for the units which span more than two periods in age
Distance<-apply(PeriodMatrix,1,function(x) diff(which(x==1))>1)
Separated<-which(Distance==TRUE)

# Assign the value 1 for the periods which fall between the earliest and latest period
for(i in Separated){
	PeriodMatrix[i,which(PeriodMatrix[i,]==1)[1]:which(PeriodMatrix[i,]==1)[2]]<-1
	}	
	
# Bind the PeriodMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,PeriodMatrix))
	
############################################## EPOCHS OPTION ###################################################

# Load communityMatrix.R modgule of paleobiology database r package
source("https://raw.githubusercontent.com/aazaff/paleobiologyDatabase.R/master/communityMatrix.R")
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
Distance<-apply(EpochMatrix,1,function(x) diff(which(x==1))>1)
Separated<-which(Distance==TRUE)
	
# Assign the value 1 for the epochs which fall between the earliest and latest epoch
for(i in Separated){
	EpochMatrix[i,which(EpochMatrix[i,]==1)[1]:which(EpochMatrix[i,]==1)[2]]<-1
	}	
	
# Bind the EpochMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,EpochMatrix))

########################################## ADD ENVIRONMENTS COLUMNS ##################################################
########################################## NO CATEGORIES OPTION ##################################################

# download a list of environmnets from Macrostrat database
EnvironsURL<-"https://macrostrat.org/api/defs/environments?all&format=csv"
GotURL<-getURL(EnvironsURL)
EnvironsFrame<-read.csv(text=GotURL,header=TRUE)
Environments<-unique(EnvironsFrame[,"name"])
	
# Create a matrix showing whether or not each environment category corresponds with each row of SubsetUnitsFrame[,"environ"]
EnvironMatrix<-sapply(Environments,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
# assign column names
colnames(EnvironMatrix)<-Environments

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             	                                                                                       
####################################### CATEGORIZED ENVIRONMENTS OPTION ###############################################
	
lacustrine<-c("lacustrine - small","lacustrine prodelta","lacustrine delta front","lacustrine deltaic indet.","crater lake",
"lacustrine delta plain","fluvial-lacustrine indet.","lacustrine interdistributary bay","lacustrine - small",
"lacustrine - large","lacustrine indet.","playa")
fluvial<-c("fluvial indet.","fluvial-lacustrine indet.","fluvial meandering","fluvial braided","fluvial indet.",
"fluvial-deltaic indet.","alluvial fan","crevasse splay","floodplain","channel lag","levee","channel","interdistributary bay",
"pond") # Need to think about where alluvial fans go	   
shallowsubtidal<-c("shallow subtidal","open shallow subtidal")
deepsubtidal<-c("deep subtidal indet.","deep subtidal shelf","deep subtidal ramp")
aeolian<-c("eolian indet.","loess","dune","interdune")
glacial<-c("ground moraine","esker","drumlin","outwash plain","glacial indet.","end moraine")
lagoon<-c("lagoonal/restricted shallow subtidal sand shoal","lagoonal","tidal flat")
transitionzone<-c("transition zone/lower shoreface") # consider moving tansition zone to deepsubtidal
deepwater<-c("abyss","submarine fan","deep-water indet.","basinal","slope","basin reef")
marine<-c("marine","inferred marine","marginal marine")
deltaic<-c("delta front","deltaic indet.","delta plain","prodelta","lacustrine delta front","fluvial-deltaic indet.",
"lacustrine prodelta","lacustrine deltaic indet.","lacustrine delta plain")
nonmarine<-c("non-marine","marginal marine")
cave<-c("fissure fill","cave","karst indet.","sinkhole","tar","mire/swamp","spring")
deltaiccoastal<-c("interdistributary bay","deltaic indet.","prodelta","delta plain","delta front","shoreface")
nondeltaiccoastal<-c("foreshore","transition zone/lower shoreface","offshore","offshore indet.","offshore shelf",
"shoreface","offshore ramp") # Only siliciclastic settings?
paraliccoastal<-c("paralic indet.","lagoonal/restricted shallow subtidal sand shoal","lagoonal","tidal flat","estuary/bay")	     	     
coastal<-c("coastal indet.","barrier bar","peritidal",paraliccoastal,nondeltaiccoastal)
shallowreef<-c("intrashelf/intraplatform reef","perireef")
shelfmarginreef<-c("platform/shelf-margin reef")
carbonateslopereef<-c("slope/ramp reef")
deepreef<-c("basin reef")
reef<-c("buildup or bioherm","reef","peritidal",deepreef,carbonateslopereef,shelfmarginreef,shallowreef)
terrestrial<-c("weathering surface","colluvial slope")
	     
EnvironMatrix1<-sapply(lacustrine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix2<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix3<-sapply(shallowsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix4<-sapply(deepsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix5<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix6<-sapply(lagoon,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix7<-sapply(shallowsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix8<-sapply(transitionzone,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix9<-sapply(marine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix10<-sapply(cave,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix11<-sapply(deltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix12<-sapply(nondeltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix13<-sapply(paraliccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix14<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix15<-sapply(shallowreef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix16<-sapply(shelfmarginreef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix17<-sapply(carbonateslopereef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix18<-sapply(reef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix19<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix19)<-terrestrial
	
lacustrine<-apply(EnvironMatrix1, 1, function(x) any(x)==TRUE)
fluvial<-apply(EnvironMatrix2, 1, function(x) any(x)==TRUE)
shallowsubtidal<-apply(EnvironMatrix3, 1, function(x) any(x)==TRUE)
deepsubtidal<-apply(EnvironMatrix4, 1, function(x) any(x)==TRUE)
aeolian<-apply(EnvironMatrix5, 1, function(x) any(x)==TRUE)
lagoon<-apply(EnvironMatrix6, 1, function(x) any(x)==TRUE)
shallowsubtidal<-apply(EnvironMatrix7, 1, function(x) any(x)==TRUE)
transitionzone<-apply(EnvironMatrix8, 1, function(x) any(x)==TRUE)
marine<-apply(EnvironMatrix9, 1, function(x) any(x)==TRUE)
cave<-apply(EnvironMatrix10, 1, function(x) any(x)==TRUE)
deltaiccoastal<-apply(EnvironMatrix11, 1, function(x) any(x)==TRUE)
nondeltaiccoastal<-apply(EnvironMatrix12, 1, function(x) any(x)==TRUE)
paraliccoastal<-apply(EnvironMatrix13, 1, function(x) any(x)==TRUE)
coastal<-apply(EnvironMatrix14, 1, function(x) any(x)==TRUE)
shallowreef<-apply(EnvironMatrix15, 1, function(x) any(x)==TRUE)
shelfmarginreef<-apply(EnvironMatrix16, 1, function(x) any(x)==TRUE)
carbonateslopereef<-apply(EnvironMatrix17, 1, function(x) any(x)==TRUE)
reef<-apply(EnvironMatrix18, 1, function(x) any(x)==TRUE)
terrestrial<-apply(EnvironMatrix19, 1, function(x) any(x)==TRUE)

EnvironMatrix<-data.matrix(cbind(lacustrine,fluvial,shallowsubtidal,deepsubtidal,aeolian,lagoon,shallowsubtidal,
transitionzone,marine,cave,deltaiccoastal,paraliccoastal,coastal,shallowreef,shelfmarginreef,carbonateslopereef,
reef,terrestrial))
colnames(EnvironMatrix)<-c("lacustrine","fluvial","shallowsubtidal","deepsubtidal","aeolian","lagoon","shallowsubtidal",
"transitionzone","marine","cave","deltaiccoastal","paraliccoastal","coastal","shallowreef","shelfmarginreef",
"carbonateslopereef","reef","terrestrial")

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))

####################################### MORE CATEGORIZED ENVIRONMENTS OPTION ########################################

deepwater<-c("marine","marginal marine","shallow subtidal","open shallow subtidal","abyss","submarine fan",
"deep-water indet.","inferred marine","deep subtidal indet.","offshore shelf","deep subtidal ramp","deep subtidal shelf","basinal","barrier bar")
paraliccoastal<-c("lagoonal/restricted shallow subtidal sand shoal","lagoonal","estuary/bay","paralic indet.","tidal flat")
nondeltaiccoastal<-c("foreshore","transition zone/lower shoreface","offshore ramp","offshore","offshore indet.","shoreface",
"offshore shelf")
deltaiccoastal<-c("interdistributary bay","deltaic indet.","prodelta","delta plain","delta front")
coastal<-c("coastal indet.",paraliccoastal,nondeltaiccoastal)
glacial<-c("esker","ground moraine","drumlin","end moraine","glacial indet.","outwash plain")
fluvial<-c("fluvial indet.","fluvial braided","fluvial meandering","channel","channel lag","floodplain","levee",
"fluvial-lacustrine indet.","crevasse splay","fluvial-deltaic indet.","alluvial fan")
lacustrine<-c("lacustrine - small","lacustrine - large","lacustrine delta front","fluvial-lacustrine indet.","pond",
"crater lake","lacustrine interdistributary bay","lacustrine delta plain","lacustrine indet.","lacustrine deltaic indet.",
"lacustrine prodelta")
carbonate<-c("reef","slope/ramp reef","buildup or bioherm","perireef","intrashelf/intraplatform reef","basin reef",
"platform/shelf-margin reef","peritidal","slope")
aeolian<-c("eolian indet.","dune","interdune","loess")
terrestrial<-c("weathering surface","colluvial slope","tar","playa","cave","fissure fill","karst indet.","sinkhole","spring","mire/swamp") # misc. terrestrial
nonmarine<-"non-marine"

EnvironMatrix1<-sapply(deepwater,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix2<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix3<-sapply(paraliccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix4<-sapply(deltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix5<-sapply(nondeltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix6<-sapply(glacial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix7<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix8<-sapply(lacustrine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix9<-sapply(carbonate,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix10<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix11<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix12<-sapply(nonmarine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
	
deepwater<-apply(EnvironMatrix1, 1, function(x) any(x)==TRUE)
coastal<-apply(EnvironMatrix2, 1, function(x) any(x)==TRUE)
paraliccoastal<-apply(EnvironMatrix3, 1, function(x) any(x)==TRUE)
deltaiccoastal<-apply(EnvironMatrix4, 1, function(x) any(x)==TRUE)
nondeltaiccoastal<-apply(EnvironMatrix5, 1, function(x) any(x)==TRUE)
glacial<-apply(EnvironMatrix6, 1, function(x) any(x)==TRUE)
fluvial<-apply(EnvironMatrix7, 1, function(x) any(x)==TRUE)
lacustrine<-apply(EnvironMatrix8, 1, function(x) any(x)==TRUE)	
carbonate<-apply(EnvironMatrix9, 1, function(x) any(x)==TRUE)	
aeolian<-apply(EnvironMatrix10, 1, function(x) any(x)==TRUE)
terrestrial<-apply(EnvironMatrix11, 1, function(x) any(x)==TRUE)
nonmarine<-apply(EnvironMatrix12, 1, function(x) any(x)==TRUE)
	
EnvironMatrix<-data.matrix(cbind(deepwater,coastal,paraliccoastal,nondeltaiccoastal,glacial,fluvial,lacustrine,carbonate,
aeolian,terrestrial,nonmarine))
colnames(EnvironMatrix)<-c("deepwater","coastal","paraliccoastal","nondeltaiccoastal","glacial","fluvial","lacustrine",
"carbonate","aeolian","terrestrial","nonmarine")

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))

################################ EVEN MORE CATEGORIZED ENVIRONMENTS OPTION ########################################	     
marine<-c("marine","marginal marine","shallow subtidal","open shallow subtidal","abyss","submarine fan",
"deep-water indet.","inferred marine","deep subtidal indet.","offshore shelf","deep subtidal ramp","deep subtidal shelf",
"basinal","barrier bar","reef","slope/ramp reef","buildup or bioherm","perireef","intrashelf/intraplatform reef","basin reef",
"platform/shelf-margin reef","peritidal","slope")
coastal<-c("coastal indet.","lagoonal/restricted shallow subtidal sand shoal","lagoonal","estuary/bay","paralic indet."
,"tidal flat","foreshore","transition zone/lower shoreface","offshore ramp","offshore","offshore indet.","shoreface",
"offshore shelf","interdistributary bay","deltaic indet.","prodelta","delta plain","delta front")
glacial<-c("esker","ground moraine","drumlin","end moraine","glacial indet.","outwash plain")
fluvial<-c("fluvial indet.","fluvial braided","fluvial meandering","channel","channel lag","floodplain","levee",
"fluvial-lacustrine indet.","crevasse splay","fluvial-deltaic indet.","alluvial fan")	     
lacustrine<-c("lacustrine - small","lacustrine - large","lacustrine delta front","fluvial-lacustrine indet.","pond",
"crater lake","lacustrine interdistributary bay","lacustrine delta plain","lacustrine indet.","lacustrine deltaic indet.",
"lacustrine prodelta")
aeolian<-c("eolian indet.","dune","interdune","loess")
terrestrial<-c("weathering surface","colluvial slope","tar","playa","cave","fissure fill","karst indet.","sinkhole","spring","mire/swamp") # misc. terrestrial
nonmarine<-"non-marine"
	
EnvironMatrix1<-sapply(marine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix2<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix3<-sapply(glacial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix4<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix5<-sapply(lacustrine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix6<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix7<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix8<-sapply(nonmarine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
	
marine<-apply(EnvironMatrix1, 1, function(x) any(x)==TRUE)
coastal<-apply(EnvironMatrix2, 1, function(x) any(x)==TRUE)
glacial<-apply(EnvironMatrix3, 1, function(x) any(x)==TRUE)
fluvial<-apply(EnvironMatrix4, 1, function(x) any(x)==TRUE)
lacustrine<-apply(EnvironMatrix5, 1, function(x) any(x)==TRUE)
aeolian<-apply(EnvironMatrix6, 1, function(x) any(x)==TRUE)
terrestrial<-apply(EnvironMatrix7, 1, function(x) any(x)==TRUE)
nonemarine<-apply(EnvironMatrix8, 1, function(x) any(x)==TRUE)
	
EnvironMatrix<-data.matrix(cbind(marine,coastal,glacial,fluvial,lacustrine,aeolian,terrestrial,nonemarine))
colnames(EnvironMatrix)<-c("marine","coastal","glacial","fluvial","lacustrine","aeolian","terrestrial","nonemarine")
	
UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))
	
################################ EVEN MOOOOOORE CATEGORIZED ENVIRONMENTS OPTION ########################################	     

marine<-c("marine","marginal marine","shallow subtidal","open shallow subtidal","abyss","submarine fan",
"deep-water indet.","inferred marine","deep subtidal indet.","offshore shelf","deep subtidal ramp","deep subtidal shelf",
"basinal","barrier bar","reef","slope/ramp reef","buildup or bioherm","perireef","intrashelf/intraplatform reef","basin reef",
"platform/shelf-margin reef","peritidal","slope","offshore ramp","offshore","offshore indet.","shoreface",
"transition zone/lower shoreface","paralic indet.","foreshore","coastal indet.","tidal flat","offshore shelf")
aquatic<-c("lagoonal/restricted shallow subtidal sand shoal","lagoonal","estuary/bay","interdistributary bay",
"deltaic indet.","prodelta","delta plain","delta front")   
terrestrial<-c("esker","ground moraine","drumlin","end moraine","glacial indet.","outwash plain","fluvial indet.",
"fluvial braided","fluvial meandering","channel","channel lag","floodplain","levee","fluvial-lacustrine indet.",
"crevasse splay","fluvial-deltaic indet.","alluvial fan","lacustrine - small","lacustrine - large","lacustrine delta front",
"fluvial-lacustrine indet.","pond","crater lake","lacustrine interdistributary bay","lacustrine delta plain",
"lacustrine indet.","lacustrine deltaic indet.","lacustrine prodelta","weathering surface","colluvial slope","tar","playa",
"cave","fissure fill","karst indet.","sinkhole","spring","mire/swamp","eolian indet.","dune","interdune","loess")

EnvironMatrix1<-sapply(marine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix2<-sapply(aquatic,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
EnvironMatrix3<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])

marine<-apply(EnvironMatrix1, 1, function(x) any(x)==TRUE)
aquatic<-apply(EnvironMatrix2, 1, function(x) any(x)==TRUE)
terrestrial<-apply(EnvironMatrix3, 1, function(x) any(x)==TRUE)	
	
EnvironMatrix<-data.matrix(cbind(marine,aquatic,terrestrial))
colnames(EnvironMatrix)<-c("marine","aquatic","terrestrial")
	
UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix))
	
####################################### ADDRESS MARINE / NON-MARINE ISSUE ############################################

# Make a vector of environments from SubsetUnitsFrame
EnvironString<-SubsetUnitsFrame[,"environ"]
# Add a space in front of every EnvironString element for grepl search
EnvironVector<-sapply(" ", paste, EnvironString)	
# Search for " marine" in EnvironString
Marine<-sapply(" marine", function (x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE),EnvironVector)
# Search for "non-marine" in EnvironString
NonMarine<-sapply("non-marine", function (x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE),EnvironVector)
# Search for "inferred marine" in EnvironString
InferredMarine<-sapply("inferred marine", function (x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE),EnvironVector)
# Bind vectors
MarineMatrix<-cbind(Marine, NonMarine, InferredMarine)
# Assign column names
colnames(MarineMatrix)<-c("marine","non-marine","inferred marine")
# Extract rows form EnvironString which are non-marine but NOT marine
NonMarineRows<-which(MarineMatrix[,"marine"]==FALSE&MarineMatrix[,"non-marine"]==TRUE)
# Locate those corresponding rows from UnitDataTable, and assign 0 to the marine column
UnitDataTable[NonMarineRows,"marine"]<-0
# Extract rows form EnvironString which are inferred marine but NOT marine
InferredMarineRows<-which(MarineMatrix[,"marine"]==FALSE&MarineMatrix[,"inferred marine"]==TRUE)
# Locate those corresponding rows from UnitDataTable, and assign 0 to the marine column
UnitDataTable[InferredMarineRows,"marine"]<-0
	
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
