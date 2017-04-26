Start<-print(Sys.time()) # Record the app start time

# Record app run date for output
PaperStats<-cbind("Fidelity run date", as.character(Sys.time()))

# Custom functions are camelCase. Arrays, parameters, and arguments are PascalCase
# Dependency functions are not embedded in master functions, and are marked with the flag dependency in the documentation
# []-notation is used wherever possible, and $-notation is avoided.

########################################### Application Setup ###############################################
# Install libraries if necessary and load them into the environment
if (suppressWarnings(require("RCurl"))==FALSE) {
    install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/");
    library("RCurl");
    }
    
if (suppressWarnings(require("doParallel"))==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (suppressWarnings(require("RPostgreSQL"))==FALSE) {
    install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
    library("RPostgreSQL");
    }

# Start a cluster for multicore, 3 by default or higher if passed as command line argument
CommandArgument<-commandArgs(TRUE)
if (length(CommandArgument)==0) {
     Cluster<-makeCluster(3)
     } else {
     Cluster<-makeCluster(as.numeric(CommandArgument[1]))
     }

print(paste("Load postgres tables.", Sys.time())) # Establish postgres connection and load the data

# Download the config file
Credentials<-as.matrix(read.table("Credentials.yml", row.names=1))
# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])

# For 402 test: 
# Driver <- dbDriver("PostgreSQL") # Establish database driver
# Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

#############################################################################################################
##################################### DATA DOWNLOAD FUNCTIONS, FIDELITY #####################################
#############################################################################################################
# No functions at this time

########################################### Data Download Script ############################################
# Step 1: Load DeepDiveData 
# For 402 test:
# DeepDiveData<-dbGetQuery(Connection, "SELECT docid, sentid, words FROM pbdb_fidelity.pbdb_fidelity_data") # make an SQL query
DeepDiveData<-dbGetQuery(Connection, "SELECT docid, sentid, words FROM nlp_sentences_352") # make an SQL query

# Extract all unique docids from DeepDiveData for output
AllDocuments<-unique(DeepDiveData[,"docid"])

# Record stats
Description1<-"Initial Data"
# Initial number of documents and rows in DeepDiveData:
Docs1<-length((unique(DeepDiveData[,"docid"])))
Rows1<-nrow(DeepDiveData)
Candidates1<-"NA"
Fossils1<-"NA"
Tuples1<-"NA"

# Step 2: Load strat-name dictionary and docid tuples
print(paste("Load strat name-docid tuples",Sys.time()))
DocUnitTuples<-as.matrix(dbGetQuery(Connection, "SELECT * FROM doc_terms"))
# Assign column names to DocUnitTuples matrix
colnames(DocUnitTuples)[1]<-"docid"
colnames(DocUnitTuples)[2]<-"unit"

# Update the stats table
Description2<-"Load doc_term Tuples"
Docs2<-Docs1
Rows2<-Rows1
Candidates2<-"NA"
Fossils2<-"NA"
# Initial number of tuples: 
Tuples2<-nrow(DocUnitTuples)

# Step 3: Download occurrences data from the Paleobiology Database
print(paste("Download PBDB occurrence data",Sys.time()))
PBDBURL<-"https://paleobiodb.org/data1.2/occs/list.csv?&cc=NOA"
PBDBURL<-RCurl::getURL(PBDBURL)
OccurrencesData<-read.csv(text=PBDBURL)

# Record the number of North American occurrences in PBDB for the output
PaperStats<-rbind(PaperStats, cbind("Number of North American occurrences in PBDB", length(unique(OccurrencesData[,"occurrence_no"]))))

# Step 4: Download geologic unit data from the Macrostrat database. 
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
print(paste("Download Macrostrat unit and age data",Sys.time()))
# Download all sedimentary unit data from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download data for geologic formations from the Macrostrat database API
FormationsURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
FormationsURL<-RCurl::getURL(FormationsURL)
FormationsFrame<-read.csv(text=FormationsURL, header=TRUE)

# Download geologic time scale data from the Macrostrat API
IntervalsURL<-"https://macrostrat.org/api/defs/intervals?all&format=csv"
IntervalsURL<-RCurl::getURL(IntervalsURL)
IntervalsFrame<-read.csv(text= IntervalsURL, header=TRUE)

# Record the number of PBDB occurrences in Macrostrat for the output
PaperStats<-rbind(PaperStats, cbind("Number of PBDB occurrences in Macrostrat", sum(UnitsFrame[,"pbdb_occurrences"])))

#############################################################################################################
###################################### DATA CLEANING FUNCTIONS, FIDELITY ####################################
#############################################################################################################
# No functions at this time

############################################ Data Cleaning Script ###########################################

# First, remove ambiguoulsy named formations from UnitsFrame and FormationsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"|UnitsFrame[,"strat_name_long"]=="Sandy Limestone"),]
FormationsFrame<-FormationsFrame[-which(FormationsFrame[,"strat_name_long"]=="Muddy Sandstone"|FormationsFrame[,"strat_name_long"]=="Mutual Formation"|FormationsFrame[,"strat_name_long"]=="Sandy Limestone"),]
# Second, subset UnitsFrame to only include formations 
FormationUnits<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%FormationsFrame[,"strat_name_long"])
# Third, remove Precambrian units from FormationUnits
# Extract the maximum age for units of interest
Max_age<-IntervalsFrame[which(IntervalsFrame[,"name"]=="Precambrian"),"t_age"]
# Make sure the top age of the formations are less than the max age (less than the Cambrian-Proterozoic boundary age)
FormationUnits<-FormationUnits[which(FormationUnits[,"t_int_age"]<Max_age),]
# Record the number of sedimentary, Phanerozoic formations in Macrostrat for the output
PaperStats<-rbind(PaperStats, cbind("Total number of sedimentary, Phanerozoic formations in Macrostrat", length(unique(FormationUnits[,"strat_name_long"]))))

# Step 5: Create three dictionaries:
# (1) formations without fossils, (2) formations with fossils, (3) the first two dictionaries combined
# Convert the strat_name_long column of formation units to character
FormationUnits[,"strat_name_long"]<-as.character(FormationUnits[,"strat_name_long"])
# Take sum of pbdb_collections values associated with each strat name 
Collections<-tapply(FormationUnits[,"pbdb_collections"], FormationUnits[,"strat_name_long"], sum)
# Extract strat names with a sum of zero pbdb_collections (units with no fossil occurrences according to PBDB)
CandidateUnits<-names(which(Collections==0))
# Extract the strat names with a at least one pbdb collection record
FossilUnits<-names(which(Collections>0))
# Bind all formations together
Formations<-c(CandidateUnits, FossilUnits)

# Record formation stats for the output
PaperStats<-rbind(PaperStats, cbind("Number of Phanerozoic sedimentary formations in Macrostrat that have PBDB fossil occurrences", length(FossilUnits)))
PaperStats<-rbind(PaperStats, cbind("Number of candidate units (Phanerozoic sedimentary formations in Macrostrat that do not have PBDB fossil occurrences)", length(CandidateUnits)))

# Update the stats table
Description3<-"Make dictionaries of formation names"
Docs3<-Docs2
Rows3<-Rows2
# Number of units of interest
Candidates3<-length(CandidateUnits)
Fossils3<-length(FossilUnits)
Tuples3<-Tuples2

# Step 6: Subset tuples to those which have units from the Formations dictionary, and subset Formations dictionary to units found in tuples.
print(paste("Subset tuples to dictionary formations, and subset dictionary formations to tuple units",Sys.time()))
# Subset doc, term tuples
SubsetTuples<-subset(DocUnitTuples, DocUnitTuples[,"unit"]%in%Formations)
# Subset formations
Formations<-subset(Formations, Formations%in%SubsetTuples[,"unit"])
CandidateUnits<-subset(CandidateUnits, CandidateUnits%in%SubsetTuples[,"unit"])
FossilUnits<-subset(FossilUnits, FossilUnits%in%SubsetTuples[,"unit"])

# Update the stats table
Description4<-"Subset tuples to only dictionary formations, and subset dictionary formations to tuple units"
Docs4<-Docs3
Rows4<-Rows3
# Number of units of interest after subsetting to tuple units
Candidates4<-length(CandidateUnits)
Fossils4<-length(FossilUnits)
# Number of tuples after subsetting to dictionary formation tuples only
Tuples4<-nrow(SubsetTuples)

# Step 7: Subset DeepDive data to include only documents that are found in the updated tuples.
print(paste("Subset DeepDiveData to docids in updated tuples", Sys.time()))
SubsetDeepDive<-subset(DeepDiveData, DeepDiveData[,"docid"]%in%unique(SubsetTuples[,"docid"])) 

# Update the stats table
Description5<-"Subset DeepDiveData to include only docs in updated tuples"
# Number of documents of interest after subsetting DeepDiveData to docids from tuples
Docs5<-length(unique(SubsetDeepDive[,"docid"]))
# Number of rows after subsetting DeepDiveData
Rows5<-nrow(SubsetDeepDive)
Candidates5<-Candidates4
Fossils5<-Fossils4
Tuples5<-"NA"

# Remove bracket symbols ({ and }) from SubsetDeepDive sentences
SubsetDeepDive[,"words"]<-gsub("\\{|\\}", "", SubsetDeepDive[,"words"])
# Replace commas in SubsetDeepdive sentences with spaces to prepare to run grep function
CleanedWords<-gsub(",", " ", SubsetDeepDive[,"words"])
# Add a space at the beginning of each sentence to improve grep search results
CleanedWords<-paste(" ", CleanedWords, sep="")
# Replace "Fm" with "Formation" in CleanedWords
CleanedWords<-gsub(" Fm", " Formation", CleanedWords)

#############################################################################################################
###################################### FORMATION SEARCH FUNCTIONS, FIDELITY #################################
#############################################################################################################
# No functions at this time.

########################################### Formation Search Script #########################################
# Step 8: Search for formations known to be in the tuples in SubsetDeepDive data.
# Record Start Time
print(paste("Begin search for dictionary formations", Sys.time()))
# Add a space before and after each unit name to improve grep accuracy
FormationsWS<-sapply(Formations, function(x) paste(x, " ", sep=""))
FormationsWS<-sapply(FormationsWS, function(x) paste(" ", x, sep=""))
# Apply grep to cleaned words
UnitHits<-parSapply(Cluster, FormationsWS, function(x,y) grep(x,y, ignore.case=TRUE, perl=TRUE), CleanedWords)
# Record end time
print(paste("Finish search for dictionary formations.", Sys.time()))

# Create a vector of the number of unit hits for each unit name in DeepDiveData
NumUnitHits<-sapply(UnitHits,length)
# Create a vector of unit names, such that each name is repeated by its number of hits in DeepDiveData
Formation<-rep(names(UnitHits),times=NumUnitHits)
# Create a column representing the row number of the unit match
SubsetDDRow<-unlist(UnitHits)
# Extract the docid for each match
docid<-SubsetDeepDive[SubsetDDRow,"docid"]
# Extract the sentid for each match
sentid<-SubsetDeepDive[SubsetDDRow,"sentid"]
# Bind the match data as a data frame
MatchData<-as.data.frame(cbind(Formation, docid, sentid, SubsetDDRow))
# Make sure row and sentence data is numerical and docid and formation names is character data
MatchData[,"SubsetDDRow"]<-as.numeric(as.character(MatchData[,"SubsetDDRow"]))
MatchData[,"docid"]<-as.character(MatchData[,"docid"])
MatchData[,"sentid"]<-as.numeric(as.character(MatchData[,"sentid"]))   
MatchData[,"Formation"]<-as.character(MatchData[,"Formation"])
    
# Create a column which flags whether the unit is or is not fossiliferous according to PBDB
MatchData[,"PBDB_occ"]<-MatchData[,"Formation"]%in%FossilUnits
    
# Update the stats table
Description6<-"Search for dictionary formations in SubsetDeepDive"
# Number of documents in SubsetDeepDive with unit name hits of formations found in tuples
Docs6<-length(unique(MatchData[,"docid"]))
# Number of rows in SubsetDeepDive with unit name hits of formations found in tuples
Rows6<-length(unique(MatchData[,"SubsetDDRow"]))
# Number of dictionary formations found in SubsetDeepDive
Candidates6<-length(unique(MatchData[which(MatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils6<-length(unique(MatchData[which(MatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples6<-"NA"
    
#############################################################################################################
###################################### LOCATION SEARCH FUNCTIONS, FIDELITY ##################################
#############################################################################################################
# Search for locations in MatchData documents to create found docid, location tuples
locationSearch<-function(SubsetDeepDive, Documents=unique(MatchData[,"docid"]), location=unique(LocationTuples[,"col_location"])) {
    # Subset SubsetDeepDive to only documents referenced in MatchData
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Documents)
    # Clean sentences so grep can run
    CleanedWords<-gsub(",", " ", DeepDive[,"words"])
    # Search for locations in DeepDive
    LocationHits<-sapply(location, function (x,y) grep (x,y, ignore.case=TRUE, perl=TRUE), CleanedWords)
    # Make a column of location names for each associated hit
    LocationHitsLength<-sapply(LocationHits,length)
    names(LocationHits)<-unique(LocationTuples[,"col_location"])
    doc_location<-rep(names(LocationHits), times=LocationHitsLength)
    # make a column for each document the location name is found in
    docid<-DeepDive[unlist(LocationHits), "docid"]
    # create an output matrix which contains each location and the document in which it appears
    return(unique(cbind(docid, doc_location)))
    } 

########################################### Location Search Script ##########################################  
# Step 9: Search for locations within the MatchData documents
print(paste("Begin location search", Sys.time()))
# Load col_id, location tuple data
LocationTuples<-read.csv("input/LocationTuples.csv") 
# for 402 test: LocationTuples<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/PBDB_Fidelity_app-master/input/LocationTuples.csv")
# Reformat data
LocationTuples[,"col_name"]<-as.character(LocationTuples[,"col_name"])
LocationTuples[,"name"]<-as.character(LocationTuples[,"name"])
colnames(LocationTuples)<-c("col_id","col_name","col_location")
                         
# Make a vector of unique col_ids
col_ids<-unique(LocationTuples[,"col_id"])
# Make a list of every location associated with each col_id
LocationList<-sapply(col_ids, function(x) LocationTuples[which(LocationTuples[,"col_id"]==x),"col_location"])
# Collapse the list elements into character vectors 
col_locations<-sapply(LocationList, function(x) paste(x, collapse=", "))
# Create a matrix of col_ids and associated locations collapsed
ColLocations<-as.data.frame(cbind(col_ids, as.character(col_locations)))
# Assign column names
colnames(ColLocations)<-c("col_id", "col_locations")
# Reformat ColLocations
ColLocations[,"col_id"]<-as.integer(as.character(ColLocations[,"col_id"]))
ColLocations[,"col_locations"]<-as.character(ColLocations[,"col_locations"])
    
# Subset UnitsFrame to only include units in MatchData
MatchFrame<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%MatchData[,"Formation"])
# Bind col_id to MatchData by strat_name_long
MatchData<-merge(MatchData, unique(MatchFrame[,c("strat_name_long", "col_id")]), by.x="Formation", by.y="strat_name_long", all.x=TRUE)  

# Bind target locations created from the spatial intersect between col_id and provinces to MatchData
MatchData<-merge(MatchData, ColLocations, by="col_id")
    
# Varify that the unit matches are valid by making sure their correct locations appeared in the document
print(paste("Begin location check.", Sys.time()))
    
# Run the locationSearch function on MatchData documents
LocationHits<-locationSearch(SubsetDeepDive, Documents=unique(MatchData[,"docid"]), location=unique(LocationTuples[,"col_location"]))
                       
# Make a list of every location that was found in each document
LocationList<-sapply(unique(LocationHits[,"docid"]), function(x) LocationHits[which(LocationHits[,"docid"]==x),"doc_location"])
# Collapse the list elements into character vectors 
doc_locations<-sapply(LocationList, function(x) paste(x, collapse=", "))
# Create a matrix of docids and associated locations collapsed
DocLocations<-cbind(names(doc_locations), as.character(doc_locations))
# Assign column names
colnames(DocLocations)<-c("docid", "doc_locations")                         

# Bind the searched locations to MatchData by docid    
MatchData<-merge(MatchData, DocLocations, by="docid", all.x=TRUE)    
    
# Remove rows from MatchData with no location hits
LocationMatchData<-MatchData[-which(is.na(MatchData[,"doc_locations"])),]

# For each row in LocationMatchData, search for each state/province in col_locations in doc_locations
LocationMatch<-vector(length=nrow(LocationMatchData))
for(i in 1:length(LocationMatch)){
    # Determine if there is at least one location match from col_locations in doc_locations
    LocationMatch[[i]]<-any(sapply(sapply(unlist(strsplit(LocationMatchData[i,"col_locations"], ", ")), function (x,y) grep (x, y, ignore.case=TRUE, perl=TRUE), LocationMatchData[i,"doc_locations"]),length)==1)
    }  
                                        
# Remove rows from LocationMatchData for which none of the col_locations are in the doc_locations
LocationMatchData<-LocationMatchData[which(LocationMatch==TRUE),]                                       
    
# Remove unnecessary columns from LocationMatchData
LocationMatchData<-unique(LocationMatchData[,c("docid", "Formation", "sentid", "SubsetDDRow", "PBDB_occ", "doc_locations")])
    
print(paste("Finish location check.",Sys.time()))
                         
# Update the stats table
Description7<-"Validate unit locations"
# Number of documents of interest
Docs7<-length(unique(LocationMatchData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows7<-length(unique(LocationMatchData[,"SubsetDDRow"]))
# Number of unit matches
Candidates7<-length(unique(LocationMatchData[which(LocationMatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils7<-length(unique(LocationMatchData[which(LocationMatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples7<-"NA"     
    
#############################################################################################################
####################################### MATCH CLEANING FUNCTIONS, FIDELITY ##################################
############################################################################################################# 
# No functions at this time.    

#############################################################################################################     
# Step 10: Eliminate sentences from LocationMatchData which contain more than one formation unit name.
print(paste("Remove sentences with more than one dictionary formation name", Sys.time()))
# Make a table showing the number of unit names which occur in each DeepDiveData row that we know has at least one unit match
RowHitsTable<-table(LocationMatchData[,"SubsetDDRow"])
# Locate and extract rows which contain only one formation from the Formations dictionary
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    
    
# Subset LocationMatchData to only include sentences with one dictionary formation match
SingleMatchData<-subset(LocationMatchData, LocationMatchData[,"SubsetDDRow"]%in%SingleHits==TRUE)

# Create a column of the single match sentences and bind it to SingleMatchData
Sentence<-CleanedWords[SingleMatchData[,"SubsetDDRow"]]
SingleMatchData<-cbind(SingleMatchData, Sentence)

# Update the stats table
Description8<-"Eliminate sentences with more than one dictionary formation name" 
# Number of documents after narrowing down to rows with only one dictionary formation
Docs8<-length(unique(SingleMatchData[,"docid"]))
# Number of sentences in SubsetDeepDive after removing sentences which contain more than one formation name
Rows8<-length(unique(SingleMatchData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one formation
Candidates8<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils8<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples8<-"NA"

# Step 11: Remove sentences from SingleMatchData that contain macrostrat unit names which are NOT in Formations.
print(paste("Remove sentences with a formation name from Macrostrat that was not already searched for", Sys.time()))
# Run another search for ALL macrostrat database formation names (except dictionary formations) in SingleMatchData sentences
MacroUnits<-unique(as.character(FormationsFrame[,"strat_name_long"]))
# Remove any unnamed Macrostrat columns from MacroUnits
MacroUnits<-MacroUnits[which(MacroUnits!="")]
# Remove dictionary formation names from MacroUnits
MacroUnits<-MacroUnits[which(MacroUnits%in%Formations==FALSE)]

# Run a search for MacroUnits on SingleMatchData sentences
# Record start time
print(paste("Search sentences for other formation macrostrat names", Sys.time()))
# Add spaces before and after formations names                                         
MacroUnitsWS<-sapply(MacroUnits, function(x) paste(x, " ", sep=""))
MacroUnitsWS<-sapply(MacroUnitsWS, function(x) paste(" ", x, sep=""))                                          
# Apply grep SingleMatchData[,"Sentence"]
MacroUnitHits<-parSapply(Cluster, MacroUnitsWS, function(x,y) grep(x,y, ignore.case=TRUE, perl = TRUE), SingleMatchData[,"Sentence"])
# Record end time
print(paste("Finish search for sentences with other formation macrostrat names.",Sys.time()))
    
# Remove the rows in which macrostrat unit names appear
# NOTE: write if statement in case MacroUnitHits is empty
if (do.call(sum, MacroUnitHits)>0) {
    UnitData<-SingleMatchData[-unique(unlist(MacroUnitHits)),]
    } else {UnitData<-SingleMatchData}

# Update the stats table
Description9<-"Eliminate sentences with macrostrat formation names that were not already searched for"
# Number of documents of interest after removing non-formation-dictionary Macrostrat unit hits
Docs9<-length(unique(UnitData[,"docid"]))
# Number of sentences in SubsetDeepDive with single unit hits and no MacroUnit names
Rows9<-length(unique(UnitData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one formation dictionary unit and no other macrostrat name
Candidates9<-length(unique(UnitData[which(UnitData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils9<-length(unique(UnitData[which(UnitData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples9<-"NA"

# Step 12: Eliminate rows/sentences that are more than 350 characters in length.
print(paste("Remove sentences > 350 characters in length",Sys.time()))
# Find the character length for each character string in UnitData sentences
Chars<-sapply(as.character(UnitData[,"Sentence"]),nchar)
# Locate the rows which have UnitData sentences with less than or equal to 350 characters
ShortSents<-which(as.numeric(Chars)<=350)
UnitDataCut<-UnitData[ShortSents,]
    
# Update the stats table
Description10<-"Eliminate sentences > 350 characters in length"
# Number of documents of interest after cutting out long rows
Docs10<-length(unique(UnitDataCut["docid"]))
# Number of short sentences in SubsetDeepDive with single formation dictionary unit hits and no MacroUnits names
Rows10<-length(unique(UnitDataCut[,"SubsetDDRow"]))
# Number of unit matches after narrowing to only short sentences
Candidates10<-length(unique(UnitDataCut[which(UnitDataCut[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils10<-length(unique(UnitDataCut[which(UnitDataCut[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples10<-"NA"

#############################################################################################################
####################################### FOSSIL MATCH FUNCTIONS, FIDELITY ####################################
#############################################################################################################      
# No functions at this time.    

########################################## Fossil Match Script ##############################################  
    
# Step 13: Search for words indicating fossil occurrences in units.
# Search for the word "fossil" in UnitDataCut sentences 
# Record start time
print(paste("Begin search for unit and fossil matches.", Sys.time()))
# NOTE: searching for "fossil" will also return hits for "fossils" and "fossiliferous"
# NOTE: add space in front of "fossil" in grep search so "unfossiliferous" is not returned as a match
FossilHits<-grep(" fossil",UnitDataCut[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Record end time
print(paste("Finish search for unit and fossil matches.", Sys.time()))
    
# Subset UnitDataCut to only rows with fossil sentences
FossilData<-UnitDataCut[FossilHits,]    
    
# Update the stats table
Description11<-"Search for words indicating fossil occurrences"
# Number of documents of interest
Docs11<-length(unique(FossilData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows11<-length(unique(FossilData[,"SubsetDDRow"]))
# Number of unit matches
Candidates11<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils11<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples11<-"NA"

#############################################################################################################
###################################### OUTPUT WRITING FUNCTIONS, FIDELITY ###################################
#############################################################################################################      
# No functions at this time

########################################### Output Writing Script ##########################################    
# Clean and update final output tables
print(paste("Clean and update final output tables.", Sys.time()))

# Subset UnitsFrame to only include units in FossilData
DictionaryFrame<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%FossilData[,"Formation"])
    
# Re-merge col_id data to FossilData by strat_name_long
OutputData<-merge(FossilData, DictionaryFrame[,c("strat_name_long", "col_id")], by.x="Formation", by.y="strat_name_long")
    
# Merge target location data into FossilData by col_id
OutputData<-merge(OutputData, ColLocations, by="col_id")
                         
# Return stats table 
StepDescription<-c(Description1, Description2, Description3, Description4, Description5, Description6, Description7, Description8, Description9, Description10, Description11)
NumberDocuments<-c(Docs1, Docs2, Docs3, Docs4, Docs5, Docs6, Docs7, Docs8, Docs9, Docs10, Docs11)
NumberRows<-c(Rows1, Rows2, Rows3, Rows4, Rows5, Rows6, Rows7, Rows8, Rows9, Rows10, Rows11)
Candidate_Units<-c(Candidates1, Candidates2, Candidates3, Candidates4, Candidates5, Candidates6, Candidates7, Candidates8, Candidates9, Candidates10, Candidates11)
Fossil_Units<-c(Fossils1, Fossils2, Fossils3, Fossils4, Fossils5, Fossils6, Fossils7, Fossils8, Fossils9, Fossils10, Fossils11)
NumberTuples<-c(Tuples1, Tuples2, Tuples3, Tuples4, Tuples5, Tuples6, Tuples7, Tuples8, Tuples9, Tuples10, Tuples11) 

Stats<-cbind(StepDescription, NumberDocuments, NumberRows, Candidate_Units, Fossil_Units, NumberTuples)

# Stop the cluster
stopCluster(Cluster)
       
############################################## Output Data Script ###########################################    
print(paste("Writing Outputs", Sys.time()))
    
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory, "/output", sep=""))
                         
# Clear any old output files
unlink("*")

write.csv(AllDocuments,"AllDocuments.csv")
write.csv(MatchData, "MatchData.csv")
write.csv(Stats, "Stats.csv", row.names=FALSE)
write.csv(PaperStats, "PaperStats.csv", row.names=FALSE)
write.csv(OutputData,"Fidelity_OutputData.csv")
    
print(paste("Complete", Sys.time()))
