Start<-print(Sys.time())

# Install libraries if necessary and load them into the environment
if (require("RCurl",warn.conflicts=FALSE)==FALSE) {
    install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/");
    library("RCurl");
    }
    
if (require("doParallel",warn.conflicts=FALSE)==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (require("RPostgreSQL",warn.conflicts=FALSE)==FALSE) {
    install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
    library("RPostgreSQL");
    }

# Start a cluster for multicore, 4 by default or higher if passed as command line argument
CommandArgument<-commandArgs(TRUE)
if (is.na(CommandArgument)) {
    Cluster<-makeCluster(4)
    } else {
    Cluster<-makeCluster(as.numeric(CommandArgument[1]))
    }

# Download the config file
Credentials<-as.matrix(read.table("Credentials.yml",row.names=1))

print(paste("Begin loading postgres tables.",Sys.time()))

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])
# STEP ONE: Load DeepDiveData 
# Make SQL query
DeepDiveData<-dbGetQuery(Connection,"SELECT docid, sentid, words FROM nlp_sentences_352")

# RECORD INITIAL STATS
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA: 
StepOneDescription<-"Initial Data"
# INITIAL NUMBER OF DOCUMENTS AND ROWS IN DEEPDIVEDATA:
StepOneDocs<-length((unique(DeepDiveData[,"docid"])))
StepOneRows<-nrow(DeepDiveData)
StepOneUnits<-"NA"
StepOneTuples<-"NA"

# STEP TWO: Load strat-name dictionary and docid tuples from GeoDeepDive
DocUnitTuples<-dbGetQuery(Connection,"SELECT * FROM doc_terms")
DocUnitTuples<-as.matrix(DocUnitTuples)
# Assign column names to DocUnitTuples matrix
colnames(DocUnitTuples)[1]<-"docid"
colnames(DocUnitTuples)[2]<-"unit"

# RECORD STATS
StepTwoDescription<-"Load Tuples"
StepTwoDocs<-StepOneDocs
StepTwoRows<-StepOneRows
StepTwoUnits<-"NA"
# INITIAL NUMBER OF TUPLES: 
StepTwoTuples<-dim(DocUnitTuples)[1]

# STEP THREE: Download a dictionary of unit names from the Macrostrat database. Extract units that are sedimentary and marine according to Macrostrat, and unfossiliferous according to the Paleobiology Database.
# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

print(paste("Finish loading postgres tables.",Sys.time()))

# Group by long strat name and take sum of pbdb_collections values
Collections<-tapply(UnitsFrame[,"pbdb_collections"],UnitsFrame[,"strat_name_long"],sum)
# Extract strat names with a sum of zero pbdb_collections, indicating the unit name has no fossil occurrences according to PBDB
CandidateUnits<-names(which(Collections==0))

# RECORD STATS
# NUMBER OF UNITS OF INTEREST:
StepThreeDescription<-"Make unit dictionary of marine, sedimentary, and unfossiliferous(according to PBDB) units"
StepThreeDocs<-StepTwoDocs
StepThreeRows<-StepTwoRows
StepThreeUnits<-length(CandidateUnits)
StepThreeTuples<-StepTwoTuples

# STEP FOUR: Subset the tuples to only those which contain candidate unit names.
SubsetTuples<-subset(DocUnitTuples,DocUnitTuples[,"unit"]%in%CandidateUnits==TRUE) 

# RECORD STATS
StepFourDescription<-"Subset tuples to only candidate units"
StepFourDocs<-StepThreeDocs
StepFourRows<-StepThreeRows
# NUMBER OF UNITS OF INTEREST FOUND IN INITIAL DOCUMENTS:
StepFourUnits<-length(unique(SubsetTuples[,"unit"]))
# NUMBER OF TUPLES AFTER SUBSETTING TO CANDIDATE UNITS ONLY: 
StepFourTuples<-dim(SubsetTuples)[1]

# STEP FIVE: Subset DeepDive data to include only documents that are found in the tuples.
SubsetDeepDive<-subset(DeepDiveData,DeepDiveData[,"docid"]%in%unique(SubsetTuples[,"docid"])==TRUE) 

# RECORD STATS
StepFiveDescription<-"Subset DeepDiveData to include only docs in tuples"
# NUMBER OF DOCUMENTS OF INTEREST AFTER SUBSETTING DEEPDIVEDATA TO TUPLES OF CANDIDATE UNITS
StepFiveDocs<-length(unique(SubsetDeepDive[,"docid"]))
# Number OF ROWS AFTER SUBSETTING DEEPDIVEDATA TO TUPLES OF CANDIDATE UNITS
StepFiveRows<-nrow(SubsetDeepDive)
StepFiveUnits<-StepFourUnits
StepFiveTuples<-"NA"

# Clean up typographical issues in the words column of DeepDiveData
SubsetDeepDive[,"words"]<-gsub("\\{|\\}","",SubsetDeepDive[,"words"])
# Remove commas from DeepDiveData to prepare to run grep function
CleanedWords<-gsub(","," ",SubsetDeepDive[,"words"])

# STEP 6: Search for candidate units known to be in the tuples in SubsetDeepDive data.

# Record Start Time
print(paste("Begin search for candidate units.",Sys.time()))
# Apply grep to cleaned words
UnitHits<-parSapply(Cluster,CandidateUnits,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),CleanedWords)
# Record end time
print(paste("Finish search for candidate units.",Sys.time()))

# Create a vector of the number of unit hits for each respective unit name in DeepDiveData
UnitHitsLength<-sapply(UnitHits,length)
# Create a vector of candidate unit names, such that each name is repeated by its number of hits in DeepDiveData
UnitName<-rep(names(UnitHits),times=UnitHitsLength)
# Bind the unit name column to the corresponding row location for the match
UnitHitData<-cbind(UnitName,unlist(UnitHits))
# convert matrix to data frame
UnitHitData<-as.data.frame(UnitHitData)
# Name column denoting row locations within Cleaned Words
colnames(UnitHitData)[2]<-"MatchLocation"
# Make sure the column data is numerical
UnitHitData[,"MatchLocation"]<-as.numeric(as.character(UnitHitData[,"MatchLocation"]))   
    
# RECORD STATS
StepSixDescription<-"Search for candidate units from tuples in SubsetDeepDive"
# NUMBER OR DOCUMENTS IN SUBSETDEEPDIVE WITH UNIT NAME HITS OF CANDIDATE UNITS FOUND IN TUPLES
StepSixDocs<-length(unique(SubsetDeepDive[UnitHitData[,"MatchLocation"],"docid"]))
# NUMBER OF ROWS IN SUBSETDEEPDIVEWITH UNIT NAME HITS OF CANDIDATE UNITS FOUND IN TUPLES
StepSixRows<-length(unique(UnitHitData[,"MatchLocation"]))
# NUMBER OF CANDIDATE UNITS FOUND IN TUPLES MATCHED IN SUBSETDEEPDIVE
StepSixUnits<-length(unique(names(UnitHits[which(sapply(UnitHits,length)>0)])))   
StepSixTuples<-"NA"
    
# STEP 7: Eliminate row/sentences from SubsetDeepDive which contain more than one candidate unit name.
# Make a table showing the number of unit names which occur in each DeepDiveData row that we know has at least one unit match
RowHitsTable<-table(UnitHitData[,"MatchLocation"])
# Locate and extract rows which contain only one long unit
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    
    
# Subset UnitHitData to get dataframe of Cleaned Words rows and associated single hit long unit names
SingleHitData<-subset(UnitHitData,UnitHitData[,"MatchLocation"]%in%SingleHits==TRUE)

# Create a column of sentences from CleanedWords and bind it to SingleHitData
Sentence<-CleanedWords[SingleHitData[,"MatchLocation"]]
SingleHitData<-cbind(SingleHitData,Sentence)

# RECORD STATS
StepSevenDescription<-"Eliminate sentences with more than one candidate unit name" 
# NUMBER OF DOCUMENTS OF INTEREST AFTER NARROWING DOWN TO ROWS WITH ONLY ONE CANDIDATE UNIT
StepSevenDocs<-length(unique(SubsetDeepDive[SingleHitData[,"MatchLocation"],"docid"]))
# NUMBER OF ROWS (SENTENCES) IN SUBSETDEEPDIVE WITH UNIT NAME HITS OF CANDIDATE UNITS AFTER REMOVING SENTENCES WHICH CONTAIN MORE THAN ONE CANDIDATE UNIT NAME
StepSevenRows<-length(unique(SingleHitData[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES AFTER NARROWING DOWN TO ROWS WITH ONLY ONE CANDIDATE UNIT
StepSevenUnits<-length(unique(SingleHitData[,"UnitName"]))
StepSevenTuples<-"NA"

# STEP 8: Remove sentences from SingleHitData that contain macrostrat unit names which are NOT in CandidateUnits.
# Run another search for ALL macrostrat database long unit names (except candidate units) in SingleHitData sentences
MacroUnitDictionary<-unique(as.character(UnitsFrame[,"strat_name_long"]))
# Remove any empty columns from MacroUnitDictionary
MacroUnitDictionary<-MacroUnitDictionary[which(MacroUnitDictionary!="")]
# Remove CandidateUnits names from MacroUnitDictionary
MacroUnitDictionary<-MacroUnitDictionary[which(MacroUnitDictionary%in%CandidateUnits==FALSE)]

# Run a search for macrostrat units on SingleHitData sentences
# Record start time
print(paste("Search for sentences with non candidate unit macrostrat names.",Sys.time()))
# Apply grep SingleHitData[,"Sentences"]
MacroUnitHits<-parSapply(Cluster,MacroUnitDictionary,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),SingleHitData[,"Sentence"])
# Record end time
print(paste("Finish search for sentences with non candidate unit macrostrat names.",Sys.time()))
    
# Remove the rows in which macrostrat unit names appear
UnitData<-SingleHitData[-unique(unlist(MacroUnitHits)),]
    
# RECORD STATS
StepEightDescription<-"Eliminate sentences with macrostrat unit names that are not candidate units"
# NUMBER OF DOCUMENTS OF INTEREST AFTER NARROWING DOWN ROWS TO THOSE WHICH HAVE ONLY A SINGLE CANDIDATE UNIT AND NO OTHER MACROSTRAT UNIT NAME
StepEightDocs<-length(unique(SubsetDeepDive[UnitData[,"MatchLocation"],"docid"]))
# NUMBER OF ROWS (SENTENCES) IN SUBSETDEEPDIVE WITH SINGLE CANDIDATE UNIT HITS AND NO MACROUNITDICTIONARY NAMES
StepEightRows<-length(unique(UnitData[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES AFTER NARROWING DOWN TO ROWS WITH ONLY ONE CANDIDATE UNIT AND NO OTHER MACROSTRAT NAME
StepEightUnits<-length(unique(UnitData[,"UnitName"]))
StepEightTuples<-"NA"

# STEP 9: Eliminate rows/sentences that are more than 350 characters in length.

# Find the character length for each character string in UnitData sentences
Chars<-sapply(UnitData[,"Sentence"], function (x) nchar(as.character(x)))
# bind the number of characters for each sentence to UnitData
UnitData<-cbind(UnitData,Chars)
# Locate the rows which have UnitData sentences with less than or equal to 350 characters
ShortSents<-which(UnitData[,"Chars"]<=350)
UnitDataCut<-UnitData[ShortSents,]
    
# RECORD STATS
StepNineDescription<-"Eliminate sentences >350 characters in length"
# NUMBER OF DOCUMENTS OF INTEREST AFTER CUTTING OUT LONG ROWS
StepNineDocs<-length(unique(SubsetDeepDive[UnitDataCut[,"MatchLocation"],"docid"]))
# NUMBER OF SHORT SENTENCES IN SUBSETDEEPDIVE WITH SINGLE CANDIDATE UNIT HITS AND NO MACROUNITDICTIONARY NAMES
StepNineRows<-length(unique(UnitDataCut[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES AFTER NARROWING DOWN TO ONLY SHORT ROWS 
StepNineUnits<-length(unique(UnitDataCut[,"UnitName"]))
StepNineTuples<-"NA"
    
# STEP 10: Search for words indicating fossil occurrences in units.

# Search for the word "fossil" in UnitDataCut sentences 
# Record start time
print(paste("Begin search for unit and fossil matches.",Sys.time()))
# NOTE: searching for "fossil" will also return hits for "fossils" and "fossiliferous"
# NOTE: add space in front of "fossil" in grep search so "unfossiliferous" is not returned as a match
FossilHits<-grep(" fossil",UnitDataCut[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Record end time
print(paste("Finish search for unit and fossil matches.",Sys.time()))
    
# Subset SingleHitsCut to only rows with fossil sentences
FossilData<-unique(UnitDataCut[FossilHits,])    
    
# RECORD STATS
StepTenDescription<-"Search for words indicating fossil occurrences"
# NUMBER OF DOCUMENTS OF INTEREST 
StepTenDocs<-length(unique(SubsetDeepDive[FossilData[,"MatchLocation"],"docid"]))
# NUMBER OF UNIQUE ROWS FROM SUBSETDEEPDIVE
StepTenRows<-length(unique(FossilData[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES 
StepTenUnits<-length(unique(FossilData[,"UnitName"]))
StepTenTuples<-"NA"
    
# STEP 11: Search for and remove words that create noise in the data ("underlying","overlying","overlain", "overlie", "overlies", "underlain", "underlie", and "underlies")
# Record start time
print(paste("Begin search for unwanted matches.",Sys.time()))
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlain<-grep("overlain",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlie<-grep("overlie",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying",FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
  
# Combine all of the noisy rows (sentences) into one vector 
NoisySentences<-c(Overlain,Overlie,Underlain,Underlie,Underlying,Overlying)

# Remove noisy sentences from FossilData
FidelityData<-FossilData[-NoisySentences,]

# Record end time
print(paste("Finish removing unwanted matches.",Sys.time()))
  
# RECORD STATS
StepElevenDescription<-"Remove sentences with noisy words"
# NUMBER OF DOCUMENTS OF INTEREST 
StepElevenDocs<-length(unique(SubsetDeepDive[FidelityData[,"MatchLocation"],"docid"]))
# NUMBER OF UNIQUE ROWS FROM SUBSETDEEPDIVE
StepElevenRows<-length(unique(FidelityData[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES 
StepElevenUnits<-length(unique(FidelityData[,"UnitName"]))
StepElevenTuples<-"NA"
    
# Extract the document id data for each match
DocID<-sapply(FidelityData[,"MatchLocation"], function(x) SubsetDeepDive[x,"docid"])
# Extract the sentence id data for each match
SentID<-sapply(FidelityData[,"MatchLocation"], function(x) SubsetDeepDive[x,"sentid"])
    
# Create a data frame for the output  
# Remove unnecessary data from the final output data frame
OutputData<-FidelityData[,c("UnitName","Sentence")]
# bind the unit name match, sentence, document id, and sentence id data into a data frame
OutputData<-cbind(OutputData,DocID,SentID)
    
# STEP 12: Clean and subset the output. Try to varify that the unit matches are valid by searching for their locations.
print(paste("Begin location check.",Sys.time()))
# Remove all rows from UnitsFrame with blank "strat_name_long" columns
UnitsFrame<-UnitsFrame[which(nchar(as.character(UnitsFrame[,"strat_name_long"]))>0),]
# Subset UnitsFrame so it only includes Candidate Units
CandidatesFrame<-UnitsFrame[which(as.character(UnitsFrame[,"strat_name_long"])%in%CandidateUnits),]
# Load col_id, location tuple data
LocationTuples<-read.csv("LocationTuples.csv")
# Join the territory names to CandidatesFrame
CandidatesFrame<-merge(CandidatesFrame,LocationTuples, by="col_id",all.x="TRUE")
CandidatesFrame<-unique(CandidatesFrame)   
# Extracts columns of interest
CandidatesFrame<-CandidatesFrame[,c("strat_name_long","col_id","location")] 

# rename the output data column of unit names so OutputData can be merged with location data for the units
colnames(OutputData)[1]<-"strat_name_long"
# Create a table of unit data that has the unit name, docid of the match, and the location(s) the unit is known to be in.
# NOTE: this merge will create a row for each location/col_id tuple associated with each match.
UnitOutputData<-merge(OutputData,CandidatesFrame, by="strat_name_long", all.x=TRUE)    

# Search for the locations from UnitOutputData[,"location"] column in SubsetDeepDive documents are referenced by docid in UnitOutputData

locationSearch<-function(SubsetDeepDive,Document=UnitOutputData[,"DocID"], location=unique(UnitOutputData[,"location"])){
    # subset SubsetDeepDive to only documents referenced in OutputData
    DeepDive<-subset(SubsetDeepDive, SubsetDeepDive[,"docid"]%in%Document)
    # clean sentences so grep can run
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

# run the locationSearch function
LocationHits<-locationSearch(SubsetDeepDive,Document=UnitOutputData[,"DocID"], location=unique(UnitOutputData[,"location"]))
LocationHits<-unique(LocationHits)    

# Create a version of UnitOutputData with just docid and location names
UnitDocLocation<-as.matrix(UnitOutputData[,c("DocID","location")])  
# Make a column of docid and location data combined for LocationHits and UnitDocLocation
Doc.Location1<-paste(LocationHits[,"LocationDocs"],LocationHits[,"Location"],sep=".")
Doc.Location2<-paste(UnitDocLocation[,"DocID"],UnitDocLocation[,"location"],sep=".")
# Bind these columns to each respective matrix
LocationHits<-cbind(LocationHits, Doc.Location1)
UnitDocLocation<-cbind(UnitDocLocation, Doc.Location2)
                         
# Find the rows from UnitOutputData that are also in LocationHits to varify that the correct location appears in the document with the unit assocoiated with the location.
# NOTE: this removes all rows associated with unit matches which do not have the correct location mentioned in the document
CheckedOutputData<-UnitOutputData[which(UnitDocLocation[,"Doc.Location2"]%in%LocationHits[,"Doc.Location1"]),]
# remove duplicate rows of strat name, sentence, docid, and sentid data that were created from the location data merge
FinalOutputData<-unique(CheckedOutputData[,c("strat_name_long","Sentence","DocID","SentID")])
                         
print(paste("Finish location check.",Sys.time()))
                         
# RECORD STATS
StepTwelveDescription<-"Validate unit locations"
# NUMBER OF DOCUMENTS OF INTEREST 
StepTwelveDocs<-length(unique(FinalOutputData[,"DocID"]))
# NUMBER OF UNIQUE ROWS FROM SUBSETDEEPDIVE
StepTwelveRows<-length(unique(FinalOutputData[,"Sentence"]))
# NUMBER OF UNIT MATCHES 
StepTwelveUnits<-length(unique(FinalOutputData[,"strat_name_long"]))
StepTwelveTuples<-"NA"                                                
                         
# Return stats table 
StepDescription<-c(StepOneDescription, StepTwoDescription, StepThreeDescription, StepFourDescription, StepFiveDescription, StepSixDescription, StepSevenDescription, StepEightDescription, StepNineDescription, StepTenDescription, StepElevenDescription, StepTwelveDescription)
NumberDocuments<-c(StepOneDocs, StepTwoDocs, StepThreeDocs, StepFourDocs, StepFiveDocs, StepSixDocs, StepSevenDocs, StepEightDocs, StepNineDocs, StepTenDocs, StepElevenDocs, StepTwelveDocs)
NumberRows<-c(StepOneRows, StepTwoRows, StepThreeRows, StepFourRows, StepFiveRows, StepSixRows, StepSevenRows, StepEightRows, StepNineRows, StepTenRows, StepElevenRows, StepTwelveRows)
NumberUnits<-c(StepOneUnits, StepTwoUnits, StepThreeUnits, StepFourUnits, StepFiveUnits, StepSixUnits, StepSevenUnits, StepEightUnits, StepNineUnits, StepTenUnits, StepElevenUnits, StepTwelveUnits)
NumberTuples<-c(StepOneTuples, StepTwoTuples, StepThreeTuples, StepFourTuples, StepFiveTuples, StepSixTuples, StepSevenTuples, StepEightTuples, StepNineTuples, StepTenTuples, StepElevenTuples, StepTwelveTuples) 

Stats<-cbind(StepDescription,NumberDocuments,NumberRows,NumberUnits,NumberTuples)

# Stop the cluster
stopCluster(Cluster)

print(paste("Writing Outputs",Sys.time()))
    
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory,"/output",sep=""))

saveRDS(UnitHitData, "UnitHitData.rds")
write.csv(UnitHitData, "UnitHitData.csv")
write.csv(Stats,"Stats.csv",row.names=FALSE)
                         
saveRDS(FinalOutputData,"Fidelity_OutputData.rds")
write.csv(FinalOutputData,"Fidelity_OutputData.csv")

    
print(paste("Complete",Sys.time()))
