# Load library
library("RCurl")

# Download outputs from app run
PaperStats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/PaperStats.csv")
AllDocuments<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/AllDocuments.csv", row.names=1)
Stats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Stats.csv")
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Fidelity_OutputData.csv", row.names=1)
# Remove White Dolomite, Sandsuck Shale, Cochran Formation, Cambridge Argillite, and Deep Spring Formation from InitialOutput
InitialOutput<-InitialOutput[-which(InitialOutput[,"Formation"]=="White Dolomite"|InitialOutput[,"Formation"]=="Sandsuck Shale"|InitialOutput[,"Formation"]=="Cochran Formation"|InitialOutput[,"Formation"]=="Cambridge Argillite"|InitialOutput[,"Formation"]=="Deep Spring Formation"),]
PBDBTupleOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/PBDBTupleOutput.csv")
NoNoiseOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoNoiseOutput.csv")
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
# Subtract 5 to account for White Dolomite, Sandsuck Shale, Cochran Formation, Cambridge Argillite, and Deep Spring Formation removal
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Total number of sedimentary, Phanerozoic formations in Macrostrat"),"V2"]))-5
# 4,677

# How many Phanerozoic sedimentary rock formations in Macrostrat have PBDB fossil occurrences
# Subtract 1 to account for Deep Spring Formation removal
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of Phanerozoic sedimentary formations in Macrostrat that have PBDB fossil occurrences"),"V2"]))-1
# 2,020

# Number of Macrostrat, sedimentary, Phanerozoic formations that do NOT have PBDB fossil occurrences (EPSILON-BETA=ZETA, perform in-app check with length(....))
# Subtract 4 to account for White Dolomite, Sandsuck Shale, Cochran Formation, and Cambridge Argillite removal
as.numeric(as.character(PaperStats[which(PaperStats[,"V1"]=="Number of candidate units (Phanerozoic sedimentary formations in Macrostrat that do not have PBDB fossil occurrences)"),"V2"]))-4
# 2,657
#Check: 4,677 - 2,020 = 2,657

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
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 709 candidate formations
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 1,018 documents in initial output mentioning candidate units
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,118 non-candidate formations (in PBDB)
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,971 documents in initial output mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in PBDBTupleOutput
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 701 candidate formations
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 989 documents in initial output mentioning candidate units
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,113 non-candidate formations (in PBDB)
length(unique(as.character(PBDBTupleOutput[which(PBDBTupleOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,910 documents mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in NoNoiseOutput
length(unique(as.character(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 589 candidate formations
length(unique(as.character(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 815 documents in NoNoiseOutput mentioning candidate units
length(unique(as.character(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,040 non-candidate formations (in PBDB)
length(unique(as.character(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,635 documents in NoNoiseOutput mentioning non-candidate units

# Number of formations we found to be fossiliferous which were not in PBDB:
length(unique(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==FALSE),"Formation"]))
# 589
# Double check: 
length(unique(MatrixData[which(MatrixData[,"PBDB_occ"]==FALSE&MatrixData[,"GDD_occ"]==TRUE),"Formation"]))
# 589
# Number of formations we confirmed to be fossiliferous which WERE in PBDB:
length(unique(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==TRUE),"Formation"]))
# 1,040
# Total number of formations we were able to identify as fossiliferous through our application processes: 
length(unique(NoNoiseOutput[,"Formation"]))
# 1,629
# Double check:
length(unique(MatrixData[which(MatrixData[,"GDD_occ"]==TRUE),"Formation"]))
# 1,629

# Number of candidate units, non-candidate units (in PBDB), and documents in NoTraceOutput
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 571 candidate formations
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 764 documents in NoTraceOutput mentioning candidate units
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,024 non-candidate formations (in PBDB)
length(unique(as.character(NoTraceOutput[which(NoTraceOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,515 documents in NoTraceOutput mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in NoMicroOutput
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 572 candidate formations
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 797 documents in NoMicroOutput mentioning candidate units
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,028 non-candidate formations (in PBDB)
length(unique(as.character(NoMicroOutput[which(NoMicroOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,588 documents in NoMicroOutput mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in NoMicroNoTraceOutput
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 552 candidate formations
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==FALSE),"docid"])))
# 746 documents in NoMicroNoTraceOutput mentioning candidate units
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,013 non-candidate formations (in PBDB)
length(unique(as.character(NoMicroNoTraceOutput[which(NoMicroNoTraceOutput[,"PBDB_occ"]==TRUE),"docid"])))
# 2,468 documents in NoMicroNoTraceOutput mentioning non-candidate units
