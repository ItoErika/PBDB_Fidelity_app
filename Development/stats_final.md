### STEP 1: Load the initial DeepDive dataset.
Initial number of DeepDive Documents | Initial number of DeepDiveData Rows/Sentences 
 ------------ | ------------
`length((unique(DeepDiveData[,"docid"])))`| `nrow(DeepDiveData)`
````9,508```` | ````5,978,379````

### STEP 2: Download the tuples. 
#### Initial number of tuples:
`dim(DocUnitTuples)[1]`
````
351,024
````

### STEP 3: Download a dictionary of unit names from the Macrostrat database. Extract units that are sedimentary and marine according to Macrostrat, and unfossiliferous according to the Paleobiology Database.
#### Initial number of candidate units:
`length(CandidateUnits)`
````
4,715
````

### STEP 4: Subset the tuples to those which contain candidate unit names.
#### Number of tuples which include candidate unit names:
`dim(SubsetTuples)[1]`
````
128,426
````
#### Number of candidate units found in the tuples: 
`length(unique(SubsetTuples[,"unit"]))`
````
4,095
````

### STEP 5: Subset DeepDive data to include only documents that are found in the tuples. 
Number of Documents | Number of Rows/Sentences 
 ------------ | ------------
`length(unique(SubsetDeepDive[,"docid"]))` | `nrow(SubsetDeepDive)`
````4,753```` | ````3,250,465````

### STEP 6: Search for candidate units known to be in the tuples in SubsetDeepDive data. 
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[UnitHitData[,"MatchLocation"],"docid"]))` | `length(unique(UnitHitData[,"MatchLocation"]))` | `length(unique(names(UnitHits[which(sapply(UnitHits,length)>0)])))`
````4,571```` | ````28,084```` | ````1,914````


### STEP 7: Eliminate row/sentences from SubsetDeepDive which contain more than one candidate unit name.
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[SingleHitData[,"MatchLocation"],"docid"]))` | `length(unique(SingleHitData[,"MatchLocation"]))` | `length(unique(SingleHitData[,"UnitNames"]))`
````4,494```` | ````25,648```` | ````1,723````

### STEP 8: Eliminate rows/sentences which contain Macrostrat unit names other than candidate units. 
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[UnitData[,"MatchLocation"],"docid"]))` | `length(UnitData[,"MatchLocation"])` | `length(unique(UnitData[,"UnitNames"]))`
````4,447```` | ````24,804```` | ````1,683````

### STEP 9: Eliminate rows/sentences that are more than 350 characters in length. 
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[UnitDataCut[,"MatchLocation"],"docid"]))` | `length(UnitDataCut[,"MatchLocation"])` | `length(unique(UnitDataCut[,"UnitNames"]))`
````4,221```` | ````21,771```` | ````1,625````

### STEP 10: Extract sentences which contain the word "fossil" or "fossiliferous". 
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[FossilData[,"MatchLocation"],"docid"]))` | `length(unique(FossilData[,"MatchLocation"]))` | `length(unique(FossilData[,"UnitNames"]))`
````432```` | ````744```` | ````238````

### STEP 11: Remove words that create noise in final results ("overlying", "overlain" "overlie", "overlies", "underlying", "underlain", "underlie", and "underlies").
Number of Documents | Number or Rows/Sentences | Number of Candidate Unit Matches
 ------------ | ------------ | ------------
`length(unique(SubsetDeepDive[FinalFossilData[,"MatchLocation"],"docid"]))` | `length(unique(FinalFossilData[,"MatchLocation"]))` | `length(unique(FinalFossilData[,"UnitNames"]))`
````421```` | ````721```` | ````227````
