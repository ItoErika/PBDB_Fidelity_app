#### Initial number of documents: 
`length((unique(DeepDiveData[,"docid"])))`
````R
9,508
````

#### Initial number of rows: 
`nrow(DeepDiveData)`
````R
5,978,379
````

#### Number of documents after subsetting DeepDiveData to only documents that contain sedimentary, marine units with no fossils according to PBDB:
`length((unique(SubsetDeepDive[,"docid"])))`
````R
4,753
````

#### Number of rows (sentences) in SubsetDeepDive:
`nrow(SubsetDeepDive)`
````R
3,250,465
````

#### Number of unique unit names that are: 
#### 1) marine and sedimentary according to macrostrat
#### 2) unfossiliferous according to PBDB 
#### 3) occur in the tuples file provided with the original dataset
`length(CandidateUnits)`
````R
4,715
```` 

#### Initial number rows (sentences) in SubsetDeepDive with unit name hits of CandidateUnits:
`length(unique(unlist(UnitHits)))`
````R
28,084
````

#### Initial number unique unit names in SubsetDeepDive:
`length(unique(names(UnitHits[which(sapply(UnitHits,length)>0)])))`
````R
1,914
````

#### Number of rows (sentences) in SubsetDeepDive with unit name hits of CandidateUnits after removing sentences which contain more than one candidate unit name:
` length(unique(SingleHitData[,"MatchLocation"]))`
````R
25,648
````

#### Number of unique unit names which appear alone (with no other unit) in a sentence in SubsetDeepDive:
`length(unique(SingleHitData[,"UnitNames"]))`
````R
1,723
````

#### Number of rows (sentences) in SubsetDeepDive with unit name hits after removing sentences comprised of more than 350 characters:
`nrow(SingleHitsCut)`
````R
22,400
````

#### Number of unique unit names which appear in alone (with no other unit) in a SubsetDeepDive sentence under 350 characters in length: 
`length(unique(SingleHitsCut[,"UnitNames"]))`
````R
1,662
````

#### Number of rows (sentences) in SingleHitsCut with the word "fossils" or "fossiliferous":
`nrow(FossilData)`
````R
759
````

#### Number of unique units which appear in a row (sentence) of SingleHitscut uncer 350 characters in length, alone (with no other unit), and in the same sentence as the word "fossil" or "fossiliferous":
`length(unique(FossilData[,"UnitNames"]))`
````R
242
````
