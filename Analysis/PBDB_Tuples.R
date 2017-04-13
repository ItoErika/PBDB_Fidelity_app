# Load libraries
library("RCurl")

# Download taxanomic names (genus and below) from the Paleobiology Database
TaxaURL<-"https://paleobiodb.org/data1.2/taxa/list.csv?rank=max_genus&all_records"
TaxaURL<-getURL(TaxaURL)
Taxa<-read.csv(text=TaxaURL, header=TRUE)

# Split Taxa into species and genera 
Species<-Taxa[which(Taxa[,"taxon_rank"]=="species"),"taxon_name"]
Genera<-Taxa[which(Taxa[,"taxon_rank"]=="genus"),"taxon_name"]

# Load taxonomic name, docid tuples
PBDB_Tuples<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_docid_terms.csv", header=FALSE)
# Assign column names
colnames(PBDB_Tuples)<-c("docid", "taxon_name")

# Subset PBDB_Tuples to only include taxonomic names that are at the genus or species level
PBDB_Tuples<-subset(PBDB_Tuples, PBDB_Tuples[,"taxon_name"]%in%Genera|PBDB_Tuples[,"taxon_name"]%in%Species)
    
# Download final cleaned fidelity output data
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")
    
# Subset CleanedOutput to only include documents from PBDB_Tuples
PBDB_Tuple_Output<-subset(CleanedOutput, CleanedOutput[,"docid"]%in%PBDB_Tuples[,"docid"])    
    
dim(PBDB_Tuple_Output)
# 1348
    
length(unique(PBDB_Tuple_Output[,"docid"]))
# 724
    
length(unique(PBDB_Tuple_Output[,"Formation"]))
# 353
    
############################################## ATTACH LOCATION DATA ##############################################
# Download all sedimentary unit names from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)
    
# Subset UnitsFrame to only include units from PBDB_Tuple_Output
UnitsFrame<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%PBDB_Tuple_Output[,"Formation"])
    
# Make a table of unique strat names and col_ids
Fm_Cols<-unique(UnitsFrame[,c("strat_name_long","col_id")])
    
# Load intersected location tuples table 
LocationTuples<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/PBDB_Fidelity_app-master/input/LocationTuples.csv")
# Substitute spaces in location names for underscores
LocationTuples[,"location"]<-gsub(" ", "_", LocationTuples[,"location"])
# Group LocationTuple data by "col_id" column
GroupedLocations<-tapply(LocationTuples[,"location"],LocationTuples[,"col_id"],as.character)
# Collapse the elements for each "col_id" in the list so that a vector is returned
Location<-sapply(GroupedLocations, function(x) paste(x, collapse=' '))
# Make a col_id column
col_id<-names(Location) 
# Bind Location and col_id
Col_Locations<-as.data.frame(cbind(as.numeric(as.character(col_id)), Location))
# Assign column names
colnames(Col_Locations)<-c("col_id","Location")
    
# Merge Fm_Cols with Col_Locations by col_id
Fm_Locations<-merge(Fm_Cols, Col_Locations, by="col_id", all.x=TRUE)
    
# Extract unique locations for each formation
# Make a unique formations column
Formation<-as.character(unique(Fm_Locations[,"strat_name_long"]))
# Extract locations
# Make a list of each location cluster associated with each unit
Fm_Location_List<-sapply(Formation, function(x) paste0(Fm_Locations[which(Fm_Locations[,"strat_name_long"]==x),"Location"]))
# Collapse clusters into states
Fm_Location_List<-sapply(Fm_Location_List, function(x) strsplit(paste(x, collapse=' '), ' '))
# Make list of states associated with each unit
Fm_Location_List<-sapply(Fm_Location_List, function(x) unique(x))
# Make a matrix of unit names and collapsed location names
Location<-as.character(sapply(Fm_Location_List, function(x) gsub(" ", ", ", paste(x, collapse=" "))))
Formation<-as.character(names(Fm_Location_List))
FormationLocations<-cbind(Formation,Location)
    
# Bind these locations to the output
PBDB_Tuple_Output[,"Formation"]<-as.character(PBDB_Tuple_Output[,"Formation"])
PBDB_Tuple_Output<-merge(PBDB_Tuple_Output, FormationLocations, by="Formation", all.x=TRUE)
    
    
