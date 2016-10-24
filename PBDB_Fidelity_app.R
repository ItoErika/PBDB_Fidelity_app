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
DeepDiveData<-dbGetQuery(Connection,"SELECT docid, words FROM nlp_sentences_352")
# Consider converting to a chracter matrix to cut down on memory. DeepDiveData<-as.matrix(DeepDiveData)

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
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)
# Subset UnitsFrame to extract only units that are identified as unfossiliferous in PBDB
NoPBDB<-subset(UnitsFrame, UnitsFrame[,"pbdb_collections"]==0)

print(paste("Finish loading postgres tables.",Sys.time()))

# Make a list of units that are unfossiliferous according to PBDB
CandidateUnits<-as.character(unique(NoPBDB[,"strat_name_long"]))
CandidateUnits<-CandidateUnits[which(sapply(CandidateUnits,nchar)>0)]

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
UnitNames<-rep(names(UnitHits),times=UnitHitsLength)
# Bind the unit name column to the corresponding row location for the match
UnitHitData<-cbind(UnitNames,unlist(UnitHits))
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
# Make a table showing the number of unit names which occur in each DeeoDiveData row that we know has at least one unit match
RowHitsTable<-table(UnitHitData[,"MatchLocation"])
# Locate and extract rows which contain only one long unit
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    
    
# Subset UnitHitData to get dataframe of Cleaned Words rows and associated single hit long unit names
SingleHitData<-subset(UnitHitData,UnitHitData[,"MatchLocation"]%in%SingleHits==TRUE)

# Create a column of sentences from CleanedWords and bind it to SingleHitData
Sentences<-CleanedWords[SingleHitData[,"MatchLocation"]]
SingleHitData<-cbind(SingleHitData,Sentences)

# RECORD STATS
StepSevenDescription<-"Eliminate sentences with more than one candidate unit name" 
# NUMBER OF DOCUMENTS OF INTEREST AFTER NARROWING DOWN TO ROWS WITH ONLY ONE CANDIDATE UNIT
StepSevenDocs<-length(unique(SubsetDeepDive[SingleHitData[,"MatchLocation"],"docid"]))
# NUMBER OF ROWS (SENTENCES) IN SUBSETDEEPDIVE WITH UNIT NAME HITS OF CANDIDATE UNITS AFTER REMOVING SENTENCES WHICH CONTAIN MORE THAN ONE CANDIDATE UNIT NAME
StepSevenRows<-length(unique(SingleHitData[,"MatchLocation"]))
# NUMBER OF UNIT MATCHES AFTER NARROWING DOWN TO ROWS WITH ONLY ONE CANDIDATE UNIT
StepSevenUnits<-length(unique(SingleHitData[,"UnitNames"]))
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
MacroUnitHits<-parSapply(Cluster,MacroUnitDictionary,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),SingleHitData[,"Sentences"])
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
StepEightUnits<-length(unique(UnitData[,"UnitNames"]))
StepEightTuples<-"NA"

# STEP 9: Eliminate rows/sentences that are more than 350 characters in length.

# Find the character length for each character string in UnitData sentences
Chars<-sapply(UnitData[,"Sentences"], function (x) nchar(as.character(x)))
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
StepNineUnits<-length(unique(UnitDataCut[,"UnitNames"]))
StepNineTuples<-"NA"
    
# STEP 10: Search for words indicating fossil occurrences in units.

# Search for the word "fossil" in UnitDataCut sentences 
# Record start time
print(paste("Begin search for unit and fossil matches.",Sys.time()))
# NOTE: searching for "fossil" will also return hits for "fossils" and "fossiliferous"
# NOTE: add space in front of "fossil" in grep search so "unfossiliferous" is not returned as a match
FossilHits<-grep(" fossil",UnitDataCut[,"Sentences"], ignore.case=TRUE, perl=TRUE)
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
StepTenUnits<-length(unique(FossilData[,"UnitNames"]))
StepTenTuples<-"NA"
    
# STEP 11: Search for and remove words that create noise in the data ("underlying","overlying","overlain", "overlie", "overlies", "underlain", "underlie", and "underlies")
# Record start time
print(paste("Begin search for unwanted matches.",Sys.time()))
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlain<-grep("overlain",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
Overlie<-grep("overlie",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying",FossilData[,"Sentences"], ignore.case=TRUE, perl=TRUE)
  
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
StepElevenUnits<-length(unique(FidelityData[,"UnitNames"]))
StepElevenTuples<-"NA"
    
# Create a final data frame for the output
# Extract the document id data for each match
DocID<-sapply(FidelityData[,"MatchLocation"], function(x) SubsetDeepDive[x,"docid"])
# Extract the sentence id data for each match
SentID<-sapply(FidelityData[,"MatchLocation"], function(x) SubsetDeepDive[x,"sentid"])
  
# Remove unnecessary data from the final output data frame
OutputData<-FidelityData[,c("UnitNames","Sentences")]
# bind the unit name match, sentence, document id, and sentence id data into a data frame
OutputData<-cbind(OutputData,DocID,SentID)
    
# Return stats table 
StepDescription<-c(StepOneDescription, StepTwoDescription, StepThreeDescription, StepFourDescription, StepFiveDescription, StepSixDescription, StepSevenDescription, StepEightDescription, StepNineDescription, StepTenDescription, StepElevenDescription)
NumberDocuments<-c(StepOneDocs, StepTwoDocs, StepThreeDocs, StepFourDocs, StepFiveDocs, StepSixDocs, StepSevenDocs, StepEightDocs, StepNineDocs, StepTenDocs, StepElevenDocs)
NumberRows<-c(StepOneRows, StepTwoRows, StepThreeRows, StepFourRows, StepFiveRows, StepSixRows, StepSevenRows, StepEightRows, StepNineRows, StepTenRows, StepElevenRows)
NumberUnits<-c(StepOneUnits, StepTwoUnits, StepThreeUnits, StepFourUnits, StepFiveUnits, StepSixUnits, StepSevenUnits, StepEightUnits, StepNineUnits, StepTenUnits, StepElevenUnits)
NumberTuples<-c(StepOneTuples, StepTwoTuples, StepThreeTuples, StepFourTuples, StepFiveTuples, StepSixTuples, StepSevenTuples, StepEightTuples, StepNineTuples, StepTenTuples, StepElevenTuples) 

Stats<-cbind(StepDescription,NumberDocuments,NumberRows,NumberUnits,NumberTuples)

# Stop the cluster
stopCluster(Cluster)

print(paste("Writing Outputs",Sys.time()))
    
CurrentDirectory<-getwd()
setwd(paste(CurrentDirectory,"/output",sep=""))
    
write.csv(Stats,"Stats.csv",row.names=FALSE)
saveRDS(OutputData,"Fidelity_OutputData.rds")
write.csv(OutputData,"Fidelity_OutputData.csv")
    
print(paste("Complete",Sys.time()))
