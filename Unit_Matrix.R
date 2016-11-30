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
############################################## PERIODS OPTION ###################################################

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
	
AgeMatrix<-multipleAges(SubsetUnitsFrame,Epochs)
	
# Account for the units which span more than two epochs in age
Distance<-apply(AgeMatrix,1,function(x) diff(which(x==1))>1)
Separated<-which(Distance==TRUE)
	
# Assign the value 1 for the epochs which fall between the earliest and latest epoch
for(i in Separated){
	AgeMatrix[i,which(AgeMatrix[i,]==1)[1]:which(AgeMatrix[i,]==1)[2]]<-1
	}	
	
# Bind the AgeMatrix to UnitDataTable
UnitDataTable<-data.matrix(cbind(UnitDataTable,AgeMatrix))

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
nonmarine<-c("non-marine","marginal marine"
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
colnames(EnvironMatrix1)<-lacustrine
EnvironMatrix2<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix2)<-fluvial
EnvironMatrix3<-sapply(shallowsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix3)<-shallowsubtidal
EnvironMatrix4<-sapply(deepsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix4)<-deepsubtidal
EnvironMatrix5<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix5)<-aeolian
EnvironMatrix6<-sapply(lagoon,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix6)<-lagoon
EnvironMatrix7<-sapply(shallowsubtidal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix7)<-shallowsubtidal
EnvironMatrix8<-sapply(transitionzone,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix8)<-transitionzone
EnvironMatrix9<-sapply(marine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix9)<-marine
EnvironMatrix10<-sapply(cave,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix10)<-cave
EnvironMatrix11<-sapply(deltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix11)<-deltaiccoastal
EnvironMatrix12<-sapply(nondeltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix12)<-nondeltaiccoastal
EnvironMatrix13<-sapply(paraliccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix13)<-paraliccoastal
EnvironMatrix14<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix14)<-coastal
EnvironMatrix15<-sapply(shallowreef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix15)<-shallowreef
EnvironMatrix16<-sapply(shelfmarginreef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix16)<-shelfmarginreef
EnvironMatrix17<-sapply(carbonateslopereef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix17)<-carbonateslopereef
EnvironMatrix18<-sapply(reef,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix18)<-reef
EnvironMatrix19<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix19)<-terrestrial

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix1,EnvironMatrix2,EnvironMatrix3,EnvironMatrix4,EnvironMatrix5
,EnvironMatri6,EnvironMatrix7,EnvironMatrix8,EnvironMatrix9,EnvironMatrix10,EnvironMatrix11,EnvironMatrix12,EnvironMatrix13
,EnvironMatrix14,EnvironMatrix15,EnvironMatrix16,EnvironMatrix17,EnvironMatrix19))

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
nonmΩarine<-"non-marine"

EnvironMatrix1<-sapply(deepwater,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix1)<-deepwater
EnvironMatrix2<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix2)<-coastal
EnvironMatrix3<-sapply(paraliccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix3)<-paraliccoastal
EnvironMatrix4<-sapply(deltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix4)<-deltaiccoastal
EnvironMatrix5<-sapply(nondeltaiccoastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix5)<-nondeltaiccoastal
EnvironMatrix6<-sapply(glacial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix6)<-glacial
EnvironMatrix7<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix7)<-fluvial
EnvironMatrix8<-sapply(lacustrine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix8)<-lacustrine
EnvironMatrix9<-sapply(carbonate,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix9)<-carbonate
EnvironMatrix10<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix10)<-aeolian
EnvironMatrix11<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix11)<-terrestrial
EnvironMatrix12<-sapply(nonmarine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix12)<-nonmarine

UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix1,EnvironMatrix2,EnvironMatrix3,EnvironMatrix4,EnvironMatrix5
,EnvironMatrix6,EnvironMatrix7,EnvironMatrix8,EnvironMatrix9,EnvironMatrix10,EnvironMatrix11,EnvironMatrix12))

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
colnames(EnvironMatrix1)<-marine
EnvironMatrix2<-sapply(coastal,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix2)<-coastal
EnvironMatrix3<-sapply(glacial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix3)<-glacial
EnvironMatrix4<-sapply(fluvial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix4)<-fluvial
EnvironMatrix5<-sapply(lacustrine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix5)<-lacustrine
EnvironMatrix6<-sapply(aeolian,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix6)<-aeolian
EnvironMatrix7<-sapply(terrestrial,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix7)<-terrestrial
EnvironMatrix8<-sapply(nonmarine,function(x,y) grepl(x,y,ignore.case=TRUE, perl = TRUE),SubsetUnitsFrame[,"environ"])
colnames(EnvironMatrix8)<-nonmarine
	
UnitDataTable<-data.matrix(cbind(UnitDataTable,EnvironMatrix1,EnvironMatrix2,EnvironMatrix3,EnvironMatrix4,EnvironMatrix5
,EnvironMatrix6,EnvironMatrix7,EnvironMatrix8))
	
####################################### ADDRESS MARINE / NON-MARINE ISSUE ############################################

# Make a vector of environments from SubsetUnitsFrame
EnvironString<-SubsetUnitsFrame[,"environ"]
# Add a space in front of every EnvironString element for grepl search
EnvironVector<-sapply(" ", paste, EnvironString)	
# Search for " marine" in EnvironString
Marine<-sapply(" marine", function (x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE),EnvironVector)
# Search for "non-marine" in EnvironString
NonMarine<-sapply("non-marine", function (x,y) grepl(x,y, ignore.case=TRUE, perl=TRUE),EnvironVector)
# Bind vectors
MarineMatrix<-cbind(Marine, NonMarine)
# Assign column names
colnames(MarineMatrix)<-c("marine","non-marine")
# Extract rows form EnvironString which are non-marine but NOT marine
NonMarineRows<-which(MarineMatrix[,"marine"]==FALSE&MarineMatrix[,"non-marine"]==TRUE)
# Locate those corresponding rows from UnitDataTable, and assign 0 to the marine column
UnitDataTable[NonMarineRows,"marine"]<-0
	
###################### Find which rows in the UnitDataTable were found in the cleaned app output ########################

# Load CleanedOutput
CleadedOutput<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_2_2016/OutputRefined/CleanedOutput.rds")
# Extract the unit names from CleanedOutput (units which have fossils according to app)
CleanedOutputUnits<-unique(CleanedOutput[,"strat_name_long"])
# Find the unit_id for each unit in cleaned output using SubsetUnitsFrame
OutputUnitID<-SubsetUnitsFrame[which(SubsetUnitsFrame[,"strat_name_long"]%in%CleanedOutputUnits),"unit_id"]
# Find which unit_ids associated with units in CleanedOutput are in UnitDataTable
FossilUnitIDs<-rownames(UnitDataTable[which(rownames(UnitDataTable)%in%OutputUnitID),])
	
