Start<-print(Sys.time()) # Record the app start time

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

#############################################################################################################
##################################### DATA DOWNLOAD FUNCTIONS, FIDELITY #####################################
#############################################################################################################
# No functions at this time

########################################### Data Download Script ############################################
# Step 1: Load DeepDiveData 
# For test:
DeepDiveData<-dbGetQuery(Connection, "SELECT docid, sentid, words FROM pbdb_fidelity.pbdb_fidelity_data") # make an SQL query
# For Ian:
#DeepDiveData<-dbGetQuery(Connection, "SELECT docid, sentid, words FROM nlp_sentences_352") # make an SQL query

# Record stats
Description1<-"Initial Data"
# Initial number of documents and rows in DeepDiveData:
Docs1<-length((unique(DeepDiveData[,"docid"])))
Rows1<-nrow(DeepDiveData)
Barren1<-"NA"
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
Barren2<-"NA"
Fossils2<-"NA"
# Initial number of tuples: 
Tuples2<-nrow(DocUnitTuples)

# Step 3: Download a dictionary of unit names from the Macrostrat database. 
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
print(paste("Download Macrostrat unit data",Sys.time()))
# Download all sedimentary unit data from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download all units from Macrostrat database at the formation level
StratURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
StratURL<-RCurl::getURL(StratURL)
StratFrame<-read.csv(text=StratURL, header=TRUE)

#############################################################################################################
###################################### DATA CLEANING FUNCTIONS, FIDELITY ####################################
#############################################################################################################
# No functions at this time

############################################ Data Cleaning Script ###########################################
# Create three dictionaries:
# (1) formations without fossils, (2) formations with fossils, (3) the first two dictionaries combined
# First, remove ambiguoulsy named formations from UnitsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"),]
# Take sum of pbdb_collections values associated with each strat name 
Collections<-tapply(UnitsFrame[,"pbdb_collections"], UnitsFrame[,"strat_name_long"], sum)
# Extract strat names with a sum of zero pbdb_collections, indicating the unit name has no fossil occurrences according to PBDB
BarrenUnits<-names(which(Collections==0))
# Subset barren units to only include formations
BarrenUnits<-subset(BarrenUnits, BarrenUnits%in%StratFrame[,"strat_name_long"])
# Extract the strat names with a at least one pbdb collection record
FossilUnits<-names(which(Collections>0))
# Subset fossil units to only include formations
FossilUnits<-subset(FossilUnits, FossilUnits%in%StratFrame[,"strat_name_long"])
# Bind all candidate formations together
Formations<-c(BarrenUnits, FossilUnits)

# Update the stats table
Description3<-"Make dictionaries of formation names"
Docs3<-Docs2
Rows3<-Rows2
# Number of units of interest:
Barren3<-length(BarrenUnits)
Fossils3<-length(FossilUnits)
Tuples3<-Tuples2

# Step 4: Subset tuples to those which have units that are in candidate formations, and subset candidate formations to units found in tuples.
print(paste("Subset tuples to candidate formations, and subset formations to tuple units",Sys.time()))
# Subset tuples
SubsetTuples<-subset(DocUnitTuples, DocUnitTuples[,"unit"]%in%Formations)
# Subset candidate formations
Formations<-subset(Formations, Formations%in%SubsetTuples[,"unit"])
BarrenUnits<-subset(BarrenUnits, BarrenUnits%in%SubsetTuples[,"unit"])
FossilUnits<-subset(FossilUnits, FossilUnits%in%SubsetTuples[,"unit"])

# Update the stats table
Description4<-"Subset tuples to only candidate formations, and subset formations to tuple units"
Docs4<-Docs3
Rows4<-Rows3
# Number of units of interest found in initial document set
Barren4<-length(BarrenUnits)
Fossils4<-length(FossilUnits)
# Numer of tuples after subsetting to candidate formation tuples only
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
Barren5<-Barren4
Fossils5<-Fossils4
Tuples5<-"NA"

# Remove bracket symbols ({ and }) from SubsetDeepDive sentences
SubsetDeepDive[,"words"]<-gsub("\\{|\\}", "", SubsetDeepDive[,"words"])
# Replace commas in SubsetDeepdive sentences with spaces to prepare to run grep function
CleanedWords<-gsub(",", " ", SubsetDeepDive[,"words"])
# Add a space at the beginning of each sentence to improve grep search results
CleanedWords<-paste(" ", CleanedWords, sep="")

# REMOVE AFTER ACCURACY TESTS: Search for " Fm " in CleanedWords
FmHits<-grep(" Fm", ignore.case=FALSE, perl=TRUE, CleanedWords)

# Replace "Fm" with "Formation" in CleanedWords
CleanedWords<-gsub(" Fm", " Formation", CleanedWords)

#############################################################################################################
###################################### FORMATION SEARCH FUNCTIONS, FIDELITY #################################
#############################################################################################################
# No functions at this time.

########################################### Formation Search Script #########################################
# Step 6: Search for candidate units known to be in the tuples in SubsetDeepDive data.
# Record Start Time
print(paste("Begin search for candidate formations.", Sys.time()))
# Add a space before and after each unit name to improve grep accuracy
FormationsWS<-sapply(Formations, function(x) paste(x, " ", sep=""))
FormationsWS<-sapply(FormationsWS, function(x) paste(" ", x, sep=""))
# Apply grep to cleaned words
UnitHits<-parSapply(Cluster, FormationsWS, function(x,y) grep(x,y, ignore.case=TRUE, perl=TRUE), CleanedWords)
# Record end time
print(paste("Finish search for candidate formations.", Sys.time()))

# Create a vector of the number of unit hits for each unit name in DeepDiveData
NumUnitHits<-sapply(UnitHits,length)
# Create a vector of candidate unit names, such that each name is repeated by its number of hits in DeepDiveData
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
    
# REMOVE AFTER ACCURACY TESTS: Figure out which sentences had "Fm" in them
FmRows<-which(MatchData[,"SubsetDDRow"]%in%FmHits)
# Assign TRUE to all rows which contained "Fm"
MatchData[FmRows,"Fm"]<-"TRUE"    
    
# Update the stats table
Description6<-"Search for candidate units in SubsetDeepDive"
# Number of documents in SubsetDeepDive with unit name hits of candidate units found in tuples
Docs6<-length(unique(MatchData[,"docid"]))
# Number of rows in SubsetDeepDive with unit name hits of candidate units found in tuples
Rows6<-length(unique(MatchData[,"SubsetDDRow"]))
# Number of candidate units found in SubsetDeepDive
Barren6<-length(unique(MatchData[which(MatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils6<-length(unique(MatchData[which(MatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples6<-"NA"
    
#############################################################################################################
####################################### MATCH CLEANING FUNCTIONS, FIDELITY ##################################
############################################################################################################# 
# No functions at this time.    

#############################################################################################################     
# Step 7: Eliminate sentences from MatchData which contain more than one formation unit name.
print(paste("Remove sentences with more than one candidate formation name", Sys.time()))
# Make a table showing the number of unit names which occur in each DeepDiveData row that we know has at least one unit match
RowHitsTable<-table(MatchData[,"SubsetDDRow"])
# Locate and extract rows which contain only one long unit
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    
    
# Subset MatchData to only include sentences with one candidate unit match
SingleMatchData<-subset(MatchData, MatchData[,"SubsetDDRow"]%in%SingleHits==TRUE)

# Create a column of the single match sentences and bind it to SingleMatchData
Sentence<-CleanedWords[SingleMatchData[,"SubsetDDRow"]]
SingleMatchData<-cbind(SingleMatchData, Sentence)

# Update the stats table
Description7<-"Eliminate sentences with more than one candidate unit name" 
# Number of documents after narrowing down to rows with only one candidate unit
Docs7<-length(unique(SingleMatchData[,"docid"]))
# Number of sentences in SubsetDeepDive after removing sentences which contain more than one candidate unit name
Rows7<-length(unique(SingleMatchData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one candidate unit
Barren7<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils7<-length(unique(SingleMatchData[which(SingleMatchData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples7<-"NA"

# Step 8: Remove sentences from SingleMatchData that contain macrostrat unit names which are NOT in candidate formations.
print(paste("Remove sentences with non-candidate Macrostrat unit names", Sys.time()))
# Run another search for ALL macrostrat database formation names (except candidate units) in SingleMatchData sentences
MacroUnits<-unique(as.character(StratFrame[,"strat_name_long"]))
# Remove any unnamed Macrostrat columns from MacroUnits
MacroUnits<-MacroUnits[which(MacroUnits!="")]
# Remove candidate formation names from MacroUnits
MacroUnits<-MacroUnits[which(MacroUnits%in%Formations==FALSE)]

# Run a search for MacroUnits on SingleMatchData sentences
# Record start time
print(paste("Search sentences for non-candidate unit macrostrat names", Sys.time()))
# Apply grep SingleMatchData[,"Sentence"]
MacroUnitHits<-parSapply(Cluster, MacroUnits, function(x,y) grep(x,y, ignore.case=TRUE, perl = TRUE), SingleMatchData[,"Sentence"])
# Record end time
print(paste("Finish search for sentences with non candidate unit macrostrat names.",Sys.time()))
    
# Remove the rows in which macrostrat unit names appear
UnitData<-SingleMatchData[-unique(unlist(MacroUnitHits)),]
    
# Update the stats table
Description8<-"Eliminate sentences with macrostrat unit names that are not candidate units"
# Number of documents of interest after removing non-candidate Macrostrat unit hits
Docs8<-length(unique(UnitData[,"docid"]))
# Number of sentences in SubsetDeepDive with single candidate unit hits and no MacroUnit names
Rows8<-length(unique(UnitData[,"SubsetDDRow"]))
# Number of unit matches after narrowing down to rows with only one candidate unit and no other macrostrat name
Barren8<-length(unique(UnitData[which(UnitData[,"PBDB_occ"]==FALSE),"Formation"]))
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
# Number of short sentences in SubsetDeepDive with single candidate unit hits and no MacroUnits names
Rows9<-length(unique(UnitDataCut[,"SubsetDDRow"]))
# Number of unit matches after narrowing to only short sentences
Barren9<-length(unique(UnitDataCut[which(UnitDataCut[,"PBDB_occ"]==FALSE),"Formation"]))
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
Barren10<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils10<-length(unique(FossilData[which(FossilData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples10<-"NA"
    
# Step11: Search for and remove words that create noise in the data ("underlying","overlying","overlain", "overlie", "overlies", "underlain", "underlie", and "underlies")
# Record start time
print(paste("Begin search for unwanted matches.", Sys.time()))
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlain<-grep("overlain", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlie<-grep("overlie", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
  
# Combine all of the noisy rows (sentences) into one vector 
NoisySentences<-unique(c(Overlain, Overlie,Underlain, Underlie, Underlying, Overlying))

# Remove noisy sentences from FossilData
FidelityData<-FossilData[-NoisySentences,]

# Record end time
print(paste("Finish removing unwanted matches.", Sys.time()))
  
# Update the stats table
Description11<-"Remove sentences with noisy words"
# Number of documents of interest
Docs11<-length(unique(FidelityData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows11<-length(unique(FidelityData[,"SubsetDDRow"]))
# Number of unit matches
Barren11<-length(unique(FidelityData[which(FidelityData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils11<-length(unique(FidelityData[which(FidelityData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples11<-"NA"

#############################################################################################################
###################################### LOCATION SEARCH FUNCTIONS, FIDELITY ##################################
#############################################################################################################      
# Search for the locations from UnitOutputData[,"location"] column in SubsetDeepDive documents are referenced by docid in UnitOutputData
locationSearch<-function(SubsetDeepDive,Document=UnitOutputData[,"docid"], location=unique(UnitOutputData[,"location"])) {
    # Subset SubsetDeepDive to only documents referenced in OutputData
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Document)
    # Clean sentences so grep can run
    CleanedWords<-gsub(","," ",DeepDive[,"words"])
    # search for locations in SubsetDeepDive
    LocationHits<-sapply(location, function (x,y) grep (x,y, ignore.case=TRUE,perl=TRUE), CleanedWords)
    # make a column of location names for each associated hit
    LocationHitsLength<-sapply(LocationHits,length)
    names(LocationHits)<-unique(UnitOutputData[,"location"])
    Location<-rep(names(LocationHits),times=LocationHitsLength)
    # make a column for each document the location name is found in
    LocationDocs<-DeepDive[unlist(LocationHits),"docid"]
    # create an output matrix which contains each location and the document in which it appears
    return(cbind(LocationDocs,Location))
    } 

########################################### Location Search Script ##########################################    
# Create a data frame for the output  
# Remove unnecessary data from the final output data frame
LocationData<-FidelityData[,c("Formation", "Sentence", "docid","sentid","PBDB_occ", "Fm")]
    
# Step 12: Clean and subset the output. Try to varify that the unit matches are valid by searching for their locations.
print(paste("Begin location check.", Sys.time()))
# Remove all rows from UnitsFrame with blank "strat_name_long" columns
UnitsFrame<-UnitsFrame[which(nchar(as.character(UnitsFrame[,"strat_name_long"]))>0),]
# Subset UnitsFrame so it only includes Candidate Units
CandidatesFrame<-UnitsFrame[which(as.character(UnitsFrame[,"strat_name_long"])%in%Formations),]
# Load col_id, location tuple data
LocationTuples<-read.csv("input/LocationTuples.csv")
# Join the territory names to CandidatesFrame
CandidatesFrame<-merge(CandidatesFrame, LocationTuples, by="col_id", all.x="TRUE")
CandidatesFrame<-unique(CandidatesFrame)   
# Extracts columns of interest
CandidatesFrame<-CandidatesFrame[,c("strat_name_long","col_id","location")] 

# Create a table of unit data that has the unit name, docid of the match, and the location(s) the unit is known to be in.
# NOTE: this merge will create a row for each location/col_id tuple associated with each match.
UnitOutputData<-merge(LocationData, CandidatesFrame, by.x="Formation", by.y="strat_name_long", all.x=TRUE)    

# Run the locationSearch function
LocationHits<-locationSearch(SubsetDeepDive,Document=UnitOutputData[,"docid"], location=unique(UnitOutputData[,"location"]))
LocationHits<-unique(LocationHits)    

# Create a version of UnitOutputData with just docid and location names
UnitDocLocation<-as.matrix(UnitOutputData[,c("docid","location")])  
# Make a column of docid and location data combined for LocationHits and UnitDocLocation
Doc.Location1<-paste(LocationHits[,"LocationDocs"], LocationHits[,"Location"], sep=".")
Doc.Location2<-paste(UnitDocLocation[,"docid"], UnitDocLocation[,"location"], sep=".")
# Bind these columns to each respective matrix
LocationHits<-cbind(LocationHits, Doc.Location1)
UnitDocLocation<-cbind(UnitDocLocation, Doc.Location2)
                                                 
# Find the rows from UnitOutputData that are also in LocationHits to varify that the correct location appears in the document with the unit assocoiated with the location.
# NOTE: this removes all rows associated with unit matches which do not have the correct location mentioned in the document
CheckedOutputData<-UnitOutputData[which(UnitDocLocation[,"Doc.Location2"]%in%LocationHits[,"Doc.Location1"]),]
# remove duplicate rows of strat name, sentence, docid, and sentid data that were created from the location data merge
FinalOutputData<-unique(CheckedOutputData[,c("Formation", "Sentence", "docid", "sentid", "PBDB_occ", "Fm", "col_id")])
                         
print(paste("Finish location check.",Sys.time()))
                         
# Update the stats table
Description12<-"Validate unit locations"
# Number of documents of interest
Docs12<-length(unique(FinalOutputData[,"docid"]))
# Number of unique rows from SubsetDeepDive
Rows12<-length(unique(FinalOutputData[,"Sentence"]))
# Number of unit matches
Barren12<-length(unique(FinalOutputData[which(FinalOutputData[,"PBDB_occ"]==FALSE),"Formation"]))
Fossils12<-length(unique(FinalOutputData[which(FinalOutputData[,"PBDB_occ"]==TRUE),"Formation"]))
Tuples12<-"NA"                                                
                         
# Return stats table 
StepDescription<-c(Description1, Description2, Description3, Description4, Description5, Description6, Description7, Description8, Description9, Description10, Description11, Description12)
NumberDocuments<-c(Docs1, Docs2, Docs3, Docs4, Docs5, Docs6, Docs7, Docs8, Docs9, Docs10, Docs11, Docs12)
NumberRows<-c(Rows1, Rows2, Rows3, Rows4, Rows5, Rows6, Rows7, Rows8, Rows9, Rows10, Rows11, Rows12)
Barren_Units<-c(Barren1, Barren2, Barren3, Barren4, Barren5, Barren6, Barren7, Barren8, Barren9, Barren10, Barren11, Barren12)
Fossil_Units<-c(Fossils1, Fossils2, Fossils3, Fossils4, Fossils5, Fossils6, Fossils7, Fossils8, Fossils9, Fossils10, Fossils11, Fossils12)
NumberTuples<-c(Tuples1, Tuples2, Tuples3, Tuples4, Tuples5, Tuples6, Tuples7, Tuples8, Tuples9, Tuples10, Tuples11, Tuples12) 

Stats<-cbind(StepDescription, NumberDocuments, NumberRows, Barren_Units, Fossil_Units, NumberTuples)

# Stop the cluster
stopCluster(Cluster)
       
############################################## Output Data Script ###########################################    
print(paste("Writing Outputs", Sys.time()))
    
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory, "/output", sep=""))
                         
# Clear any old output files
unlink("*")

write.csv(MatchData, "MatchData.csv")
write.csv(Stats, "Stats.csv", row.names=FALSE)                         
write.csv(FinalOutputData,"Fidelity_OutputData.csv")
    
print(paste("Complete", Sys.time()))
