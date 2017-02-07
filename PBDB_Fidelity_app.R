Start<-print(Sys.time())

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

# Download the config file
Credentials<-as.matrix(read.table("Credentials.yml",row.names=1))

print(paste("Begin loading postgres tables.",Sys.time()))

# Connet to PostgreSQL
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = Credentials["database:",], host = Credentials["host:",], port = Credentials["port:",], user = Credentials["user:",])
# STEP ONE: Load DeepDiveData 
# Make SQL query
DeepDiveData<-dbGetQuery(Connection,"SELECT docid, sentid, words FROM nlp_sentences_352")

# Update the stats table
Description1<-"Initial Data"
# Initial number of documents and rows in DeepDiveData:
Docs1<-length((unique(DeepDiveData[,"docid"])))
Row1s<-nrow(DeepDiveData)
Units1<-"NA"
Tuples1<-"NA"

# STEP TWO: Load strat-name dictionary and docid tuples from GeoDeepDive
DocUnitTuples<-dbGetQuery(Connection,"SELECT * FROM doc_terms")
DocUnitTuples<-as.matrix(DocUnitTuples)
# Assign column names to DocUnitTuples matrix
colnames(DocUnitTuples)[1]<-"docid"
colnames(DocUnitTuples)[2]<-"unit"

# Update the stats table
Description2<-"Load Tuples"
Docs2<-Docs1
Rows2<-Rows1
Units2<-"NA"
# Initial number of tuples: 
Tuples2<-dim(DocUnitTuples)[1]

# STEP THREE: Download a dictionary of unit names from the Macrostrat database. Extract units that are sedimentary and marine according to Macrostrat, and unfossiliferous according to the Paleobiology Database.
# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL,header=TRUE)

# Download all units from Macrostrat database at the formation level
StratURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
StratURL<-getURL(StratURL)
StratFrame<-read.csv(text=StratURL,header=TRUE)

print(paste("Finish loading postgres tables.",Sys.time()))

# Group by long strat name and take sum of pbdb_collections values
Collections<-tapply(UnitsFrame[,"pbdb_collections"],UnitsFrame[,"strat_name_long"],sum)
# Extract strat names with a sum of zero pbdb_collections, indicating the unit name has no fossil occurrences according to PBDB
CandidateUnits<-names(which(Collections==0))
# Subset candidate units to only include formations
CandidateUnits<-subset(CandidateUnits,CandidateUnits%in%StratFrame[,"strat_name_long"])

# Update the stats table
Description3<-"Make unit dictionary of marine, sedimentary, and unfossiliferous(according to PBDB) units"
Docs3<-Docs2
Rows3<-Rows2
# Number of units of interest:
Units3<-length(CandidateUnits)
Tuples3<-StepTwoTuples

# STEP FOUR: Subset the tuples to only those which contain candidate unit names.
SubsetTuples<-subset(DocUnitTuples,DocUnitTuples[,"unit"]%in%CandidateUnits==TRUE) 

# Update the stats table
Description4<-"Subset tuples to only candidate units"
Docs4<-Docs3
Rows4<-Rows3
# Number of units of interest found in initial document set
Units4<-length(unique(SubsetTuples[,"unit"]))
# Numer of tuples after subsetting to candidate tuples only
Tuples4<-dim(SubsetTuples)[1]

# STEP FIVE: Subset DeepDive data to include only documents that are found in the tuples.
SubsetDeepDive<-subset(DeepDiveData,DeepDiveData[,"docid"]%in%unique(SubsetTuples[,"docid"])==TRUE) 

# Update the stats table
Description5<-"Subset DeepDiveData to include only docs in tuples"
# Number of documents of interest after subsetting DeepDiveData to tuples of candidate units
Docs5<-length(unique(SubsetDeepDive[,"docid"]))
# Number OF ROWS AFTER SUBSETTING DEEPDIVEDATA TO TUPLES OF CANDIDATE UNITS
Rows5<-nrow(SubsetDeepDive)
Units5<-Units4
Tuples5<-"NA"

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
    
# Update the stats table
Description6<-"Search for candidate units from tuples in SubsetDeepDive"
# Number of documents in SubsetDeepDive with unit name hits of candidate units found in tuples
Docs6<-length(unique(SubsetDeepDive[UnitHitData[,"MatchLocation"],"docid"]))
# Number of rows in SubsetDeepDive with unit name hits of candidate units found in tuples
Rows6<-length(unique(UnitHitData[,"MatchLocation"]))
# Number of candidate units found in tuples matched in SubsetDeepDive
Units6<-length(unique(names(UnitHits[which(sapply(UnitHits,length)>0)])))   
Tuples6<-"NA"
    
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

# Update the stats table
Description7<-"Eliminate sentences with more than one candidate unit name" 
# Number of documents of interest after narrowing down to rows with only one candidate unit
Docs7<-length(unique(SubsetDeepDive[SingleHitData[,"MatchLocation"],"docid"]))
# Number of sentences in SubsetDeepDive with unit name hits of candidate units after removing sentences which contain more than one candidate unit name
Rows7<-length(unique(SingleHitData[,"MatchLocation"]))
# Number of unit matches after narrowing down to rows with only one candidate unit
Units7<-length(unique(SingleHitData[,"UnitName"]))
Tuples7<-"NA"

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
    
# Update the stats table
Description8<-"Eliminate sentences with macrostrat unit names that are not candidate units"
# Numer of documents of interset after narrowing down to rows which have only a single candidate unit and no other Macrostrat unit name
Docs8<-length(unique(SubsetDeepDive[UnitData[,"MatchLocation"],"docid"]))
# Number of sentences in SubsetDeepDive with single candidate unit hits and no MacroUnitDictionary names
Rows8<-length(unique(UnitData[,"MatchLocation"]))
# Number of unit matches after narrowing down to rows with only one candidate unit and no other macrostrat name
Units8<-length(unique(UnitData[,"UnitName"]))
Tuples8<-"NA"

# STEP 9: Eliminate rows/sentences that are more than 350 characters in length.

# Find the character length for each character string in UnitData sentences
Chars<-sapply(UnitData[,"Sentence"], function (x) nchar(as.character(x)))
# bind the number of characters for each sentence to UnitData
UnitData<-cbind(UnitData,Chars)
# Locate the rows which have UnitData sentences with less than or equal to 350 characters
ShortSents<-which(UnitData[,"Chars"]<=350)
UnitDataCut<-UnitData[ShortSents,]
    
# Update the stats table
Description9<-"Eliminate sentences >350 characters in length"
# Number of documents of interest after cutting out long rows
Docs9<-length(unique(SubsetDeepDive[UnitDataCut[,"MatchLocation"],"docid"]))
# Number of short sentences in SubsetDeepDive with single candidate unit hits and no MacroDictionaryUnits names
Rows9<-length(unique(UnitDataCut[,"MatchLocation"]))
# Number of unit matches after narrowing to only short sentences
Units9<-length(unique(UnitDataCut[,"UnitName"]))
Tuples9<-"NA"
    
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
    
# Update the stats table
Description10<-"Search for words indicating fossil occurrences"
# Number of documents of interest
Docs10<-length(unique(SubsetDeepDive[FossilData[,"MatchLocation"],"docid"]))
# Number of unique rows from SubsetDeepDive
Rows10<-length(unique(FossilData[,"MatchLocation"]))
# Number of unit matches
Units10<-length(unique(FossilData[,"UnitName"]))
Tuples10<-"NA"
    
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
  
# Update the stats table
Description11<-"Remove sentences with noisy words"
# Number of documents of interest
Docs11<-length(unique(SubsetDeepDive[FidelityData[,"MatchLocation"],"docid"]))
# Number of unique rows from SubsetDeepDive
Rows11<-length(unique(FidelityData[,"MatchLocation"]))
# Number of unit matches
Units11<-length(unique(FidelityData[,"UnitName"]))
Tuples11<-"NA"
    
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
                         
# Update the stats table
Description12<-"Validate unit locations"
# Number of documents of interest
Docs12<-length(unique(FinalOutputData[,"DocID"]))
# Number of unique rows from SubsetDeepDive
Rows12<-length(unique(FinalOutputData[,"Sentence"]))
# Number of unit matches
Units12<-length(unique(FinalOutputData[,"strat_name_long"]))
Tuples12<-"NA"                                                
                         
# Return stats table 
StepDescription<-c(Description1, Description2, Description3, Description4, Description5, Description6, Description7, Description8, Description9, Description10, Description11, Description12)
NumberDocuments<-c(Docs1, Docs2, Docs3, Docs4, Docs5, Docs6, Docs7, Docs8, Docs9, Docs10, Docs11, Docs12)
NumberRows<-c(Rows1, Rows2, Rows3, Rows4, Rows5, Rows6, Rows7, Rows8, Rows9, Rows10, Rows11, Rows12)
NumberUnits<-c(Units1, Units2, Units3, Units4, Units5, Units6, Units7, Units8, Units9, Units10, Units11, Units12)
NumberTuples<-c(Tuples1, Tuples2, Tuples3, Tuples4, Tuples5, Tuples6, Tuples7, Tuples8, Tuples9, Tuples10, Tuples11, Tuples12) 

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
