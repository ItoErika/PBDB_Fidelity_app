# Load library
library("RCurl")

# Download outputs from app run
PaperStats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/PaperStats.csv")
AllDocuments<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/AllDocuments.csv", row.names=1)
Stats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Stats.csv")
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Fidelity_OutputData.csv", row.names=1)
# Remove "white dolomite" from Initial Output
InitialOutput<-InitialOutput[-which(InitialOutput[,"Formation"]=="White Dolomite"),]
PBDBTupleOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/PBDBTupleOutput.csv")
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")
NoTraceOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoTraceOutput.csv")
NoMicroOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoMicroOutput.csv")
NoMicroNoTraceOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoMicroNoTraceOutput.csv")
MatrixData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/MatrixData.csv")

# Run date
PaperStats[which(PaperStats[,"V1"]=="Fidelity run date"),"V2"]
#2017-05-05 11:12:55

# Number of documents the application was run on
length(unique(AllDocuments[,"x"]))
# 95,931

# How many fossil occurrences are in PBDB in North America at app run date (From PBDB API)
PaperStats[which(PaperStats[,"V1"]=="Number of North American occurrences in PBDB"),"V2"]
# 491,180

# How many North American fossil occurrences are NOT matched to Macrostrat
PBDB_Occs<-as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of North American occurrences in PBDB"),"V2"]))
Macrostrat_Occs<-as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of PBDB occurrences in Macrostrat"),"V2"]))

PBDB_Occs-Macrostrat_Occs
# 26,525

# Total number of sedimentary, Phanerozoic formations in Macrostrat
# Subtract 1 to account for "White Dolomite" formation removal
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Total number of sedimentary, Phanerozoic formations in Macrostrat"),"V2"]))-1
# 4,681

# How many Phanerozoic sedimentary rock formations in Macrostrat have PBDB fossil occurrences
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of Phanerozoic sedimentary formations in Macrostrat that have PBDB fossil occurrences"),"V2"]))
# 2,021

# Number of Macrostrat, sedimentary, Phanerozoic formations that do NOT have PBDB fossil occurrences (EPSILON-BETA=ZETA, perform in-app check with length(....))
# Subtract 1 to account for "White Dolomite" formation removal
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of candidate units (Phanerozoic sedimentary formations in Macrostrat that do not have PBDB fossil occurrences)"),"V2"]))-1
# 2,660
#Check: 4,681 - 20,21 = 2,660

# How much data is cut down from geolocation check
LocationCheckRow<-which(Stats[,"StepDescription"]=="Validate unit locations")
PostCheckDocs<-Stats[LocationCheckRow,"NumberDocuments"]
PostCheckCandidates<-Stats[LocationCheckRow,"CandidateUnits"]
PreCheckDocs<-Stats[LocationCheckRow-1,"NumberDocuments"]
PreCheckCandidates<-Stats[LocationCheckRow-1,"CandidateUnits"]

PreCheckDocs-PostCheckDocs
# 4,585 documents removed

PreCheckCandidates-PostCheckCandidates
# 87 candidate units removed

# Number of candidate units, non-candidate units (in PBDB), and documents in original output
# Subtract 1 to account for "White Dolomite" formation removal
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 709 candidate formations
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 1,018 documents in initial output mentioning candidate units
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,119 non-candidate formations (in PBDB)
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,974 documents in initial output mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in PBDBTupleOutput
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 701 candidate formations
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 989 documents in initial output mentioning candidate units
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,114 non-candidate formations (in PBDB)
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,913 documents mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in CleanedOutput
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 589 candidate formations
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 818 documents in cleaned output mentioning candidate units
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,042 non-candidate formations (in PBDB)
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,650 documents in initial output mentioning non-candidate units

# Number of formations we found to be fossiliferous which were not in PBDB:
length(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"Formation"]))
# 589
# Double check: 
length(unique(MatrixData[which(MatrixData[,"PBDB_occ"]==FALSE&MatrixData[,"GDD_occ"]==TRUE),"Formation"]))
# 589
# Number of formations we confirmed to be fossiliferous which WERE in PBDB:
length(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"Formation"]))
# 1,042
# Total number of formations we were able to identify as fossiliferous through our application processes: 
length(unique(CleanedOutput[,"Formation"]))
# 1,631
# Double check:
length(unique(MatrixData[which(MatrixData[,"GDD_occ"]==TRUE),"Formation"]))
# 1,631

# Number of candidate units, non-candidate units (in PBDB), and documents in NoTraceOutput
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 571 candidate formations
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 768 documents in cleaned output mentioning candidate units
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,025 non-candidate formations (in PBDB)
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,528 documents in cleaned output mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in NoMicroOutput
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 583 candidate formations
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 809 documents in cleaned output mentioning candidate units
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,037 non-candidate formations (in PBDB)
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,628 documents in cleaned output mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in NoMicroNoTraceOutput
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 565 candidate formations
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 760 documents in cleaned output mentioning candidate units
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,021 non-candidate formations (in PBDB)
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,507 documents in cleaned output mentioning non-candidate units
