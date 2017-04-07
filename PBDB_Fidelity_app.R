Start<-print(Sys.time()) # Record the app start time

# Record app run date for output
Text<-"Fidelity run date"
Date<-Sys.time()
PaperStats<-cbind(Text, as.character(Date))

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

# If Testing: 
#Driver <- dbDriver("PostgreSQL") # Establish database driver
#Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

#############################################################################################################
##################################### DATA DOWNLOAD FUNCTIONS, FIDELITY #####################################
#############################################################################################################
# No functions at this time

########################################### Data Download Script ############################################
# Step 1: Load DeepDiveData 
# For test:
#DeepDiveData<-dbGetQuery(Connection, "SELECT docid, sentid, words FROM pbdb_fidelity.pbdb_fidelity_data") # make an SQL query
# For Ian:
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
DocUnitTuples<-dbGetQuery(Connection, "SELECT * FROM doc_terms")
DocUnitTuples<-as.matrix(DocUnitTuples)
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

# Step 3: Download geologic unit data from the Macrostrat database. 
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

# First, remove ambiguoulsy named formations from UnitsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"|UnitsFrame[,"strat_name_long"]=="Sandy Limestone"),]
# Second, subset UnitsFrame to only include formations 
FormationUnits<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%FormationsFrame[,"strat_name_long"])
# Third, remove Precambrian units from FormationUnits
# Extract the maximum age for units of interest
Max_age<-IntervalsFrame[which(IntervalsFrame[,"name"]=="Precambrian"),"t_age"]
# Make sure the top age of the formations are less than the max age (less than the Cambrian-Proterozoic boundary age)
FormationUnits<-FormationUnits[which(FormationUnits[,"t_int_age"]<Max_age),]
# Record the number of sedimentary, Phanerozoic formations in Macrostrat for the output
SedPhanMacro<-length(unique(FormationUnits[,"strat_name_long"]))
# Bind text to stat
PaperStats<-rbind(PaperStats, cbind("Total number of sedimentary, Phanerozoic formations in Macrostrat", SedPhanMacro))

# Create three dictionaries:
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

# Step 4: Subset tuples to those which have units that are in Formations, and subset Formations to units found in tuples.
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

# Step 5: Subset DeepDive data to include only documents that are found in the updated tuples.
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
# Step 6: Search for formations known to be in the tuples in SubsetDeepDive data.
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
####################################### MATCH CLEANING FUNCTIONS, FIDELITY ##################################
############################################################################################################# 
# No functions at this time.    

#############################################################################################################     
# Step 7: Eliminate sentences from MatchData which contain more than one formation unit name.
print(paste("Remove sentences with more than one dictionary formation name", Sys.time()))
# Make a table showing the number of unit names which occur in each DeepDiveData row that we know has at least one unit match
RowHitsTable<-table(MatchData[,"SubsetDDRow"])
# Locate and extract rows which contain only one long unit
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    
    
# Subset MatchData to only include sentences with one dictionary formation match
SingleMatchData<-subset(MatchData, MatchData[,"SubsetDDRow"]%in%SingleHits==TRUE)

# Create a column of the single match sentences and bind it to SingleMatchData
Sentence<-CleanedWords[SingleMatchData[,"SubsetDDRow"]]
SingleMatchData<-cbind(SingleMatchData, Sentence)

# Update the stats table
Description7<-"Eliminate sentences with more than one dictionary formation name" 
# Number of documents after narrowing down to rows with only one dictionary formation
Docs7<-length(unique(SingleMatchData[,"docid"]))
# Number of sentences in SubsetDeepDive after removing sentences which contain more than one formation name
Rows7<-length(unique(SingleMatchData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one formation
Candidates7<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils7<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples7<-"NA"

# Step 8: Remove sentences from SingleMatchData that contain macrostrat unit names which are NOT in Formations.
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
# Apply grep SingleMatchData[,"Sentence"]
MacroUnitHits<-parSapply(Cluster, MacroUnits, function(x,y) grep(x,y, ignore.case=TRUE, perl = TRUE), SingleMatchData[,"Sentence"])
# Record end time
print(paste("Finish search for sentences with other formation macrostrat names.",Sys.time()))
    
# Remove the rows in which macrostrat unit names appear
UnitData<-SingleMatchData[-unique(unlist(MacroUnitHits)),]
    
# Update the stats table
Description8<-"Eliminate sentences with macrostrat formation names that were not already searched for"
# Number of documents of interest after removing non-formation-dictionary Macrostrat unit hits
Docs8<-length(unique(UnitData[,"docid"]))
# Number of sentences in SubsetDeepDive with single unit hits and no MacroUnit names
Rows8<-length(unique(UnitData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one formation dictionary unit and no other macrostrat name
Candidates8<-length(unique(UnitData[which(UnitData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils8<-length(unique(UnitData[which(UnitData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples8<-"NA"

# Step 9: Eliminate rows/sentences that are more than 350 characters in length.
print(paste("Remove sentences > 350 characters in length",Sys.time()))
# Find the character length for each character string in UnitData sentences
Chars<-sapply(as.character(UnitData[,"Sentence"]),nchar)
# Locate the rows which have UnitData sentences with less than or equal to 350 characters
ShortSents<-which(as.numeric(Chars)<=350)
UnitDataCut<-UnitData[ShortSents,]
    
# Update the stats table
Description9<-"Eliminate sentences > 350 characters in length"
# Number of documents of interest after cutting out long rows
Docs9<-length(unique(UnitDataCut["docid"]))
# Number of short sentences in SubsetDeepDive with single formation dictionary unit hits and no MacroUnits names
Rows9<-length(unique(UnitDataCut[,"SubsetDDRow"]))
# Number of unit matches after narrowing to only short sentences
Candidates9<-length(unique(UnitDataCut[which(UnitDataCut[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils9<-length(unique(UnitDataCut[which(UnitDataCut[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples9<-"NA"

#############################################################################################################
####################################### FOSSIL MATCH FUNCTIONS, FIDELITY ####################################
#############################################################################################################      
# No functions at this time.    

########################################## Fossil Match Script ##############################################  
    
# Step 10: Search for words indicating fossil occurrences in units.
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
Description10<-"Search for words indicating fossil occurrences"
# Number of documents of interest
Docs10<-length(unique(FossilData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows10<-length(unique(FossilData[,"SubsetDDRow"]))
# Number of unit matches
Candidates10<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils10<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples10<-"NA"

#############################################################################################################
###################################### LOCATION SEARCH FUNCTIONS, FIDELITY ##################################
#############################################################################################################      
# Search for the locations from FossilData[,"location"] column in SubsetDeepDive documents are referenced by docid in FossilData
locationSearch<-function(SubsetDeepDive,Document=FidelityData[,"docid"], location=unique(FidelityData[,"location"])) {
    # Subset SubsetDeepDive to only documents referenced in OutputData
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Document)
    # Clean sentences so grep can run
    CleanedWords<-gsub(","," ",DeepDive[,"words"])
    # search for locations in SubsetDeepDive
    LocationHits<-sapply(location, function (x,y) grep (x,y, ignore.case=TRUE,perl=TRUE), CleanedWords)
    # make a column of location names for each associated hit
    LocationHitsLength<-sapply(LocationHits,length)
    names(LocationHits)<-unique(FidelityData[,"location"])
    Location<-rep(names(LocationHits),times=LocationHitsLength)
    # make a column for each document the location name is found in
    LocationDocs<-DeepDive[unlist(LocationHits),"docid"]
    # create an output matrix which contains each location and the document in which it appears
    return(cbind(LocationDocs,Location))
    } 

########################################### Location Search Script ##########################################    
# Step 11: Clean and subset the output. Try to varify that the unit matches are valid by searching for their locations.
print(paste("Begin location check.", Sys.time()))
# Remove all rows from UnitsFrame with blank "strat_name_long" columns
UnitsFrame<-UnitsFrame[which(nchar(as.character(UnitsFrame[,"strat_name_long"]))>0),]
# Subset UnitsFrame so it only includes formation dictionary units
DictionaryFrame<-UnitsFrame[which(as.character(UnitsFrame[,"strat_name_long"])%in%Formations),]
# Load col_id, location tuple data
LocationTuples<-read.csv("input/LocationTuples.csv")
# Join the territory names to DictionaryFrame
DictionaryFrame<-merge(DictionaryFrame, LocationTuples, by="col_id", all.x="TRUE")
DictionaryFrame<-unique(DictionaryFrame)   
# Extracts columns of interest
DictionaryFrame<-DictionaryFrame[,c("strat_name_long","col_id","location")] 

# Create a table of unit data that has the unit name, docid of the match, and the location(s) the unit is known to be in.
# NOTE: this merge will create a row for each location/col_id tuple associated with each match.
FidelityData<-merge(FossilData, DictionaryFrame, by.x="Formation", by.y="strat_name_long", all.x=TRUE)    

# Run the locationSearch function
LocationHits<-locationSearch(SubsetDeepDive,Document=FidelityData[,"docid"], location=unique(FidelityData[,"location"]))
LocationHits<-unique(LocationHits)    

# Create a version of FidelityData with just docid and location names
UnitDocLocation<-as.matrix(FidelityData[,c("docid","location")])  
# Make a column of docid and location data combined for LocationHits and UnitDocLocation
Doc.Location1<-paste(LocationHits[,"LocationDocs"], LocationHits[,"Location"], sep=".")
Doc.Location2<-paste(UnitDocLocation[,"docid"], UnitDocLocation[,"location"], sep=".")
# Bind these columns to each respective matrix
LocationHits<-cbind(LocationHits, Doc.Location1)
UnitDocLocation<-cbind(UnitDocLocation, Doc.Location2)
                                                 
# Find the rows from UnitOutputData that are also in LocationHits to varify that the correct location appears in the document with the unit assocoiated with the location.
# NOTE: this removes all rows associated with unit matches which do not have the correct location mentioned in the document
CheckedData<-FidelityData[which(UnitDocLocation[,"Doc.Location2"]%in%LocationHits[,"Doc.Location1"]),]
# remove duplicate rows of strat name, sentence, docid, and sentid data that were created from the location data merge
OutputData<-unique(CheckedData[,c("Formation", "Sentence", "docid", "sentid", "PBDB_occ", "col_id", "location")])
                         
print(paste("Finish location check.",Sys.time()))
                         
# Update the stats table
Description11<-"Validate unit locations"
# Number of documents of interest
Docs11<-length(unique(OutputData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows11<-length(unique(OutputData[,"Sentence"]))
# Number of unit matches
Candidates11<-length(unique(OutputData[which(OutputData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils11<-length(unique(OutputData[which(OutputData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples11<-"NA"                                                
                         
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
