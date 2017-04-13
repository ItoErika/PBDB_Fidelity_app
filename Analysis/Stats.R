library("RCurl")

# Download outputs from app run
PaperStats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_12Apr2017/PaperStats.csv")
AllDocuments<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_12Apr2017/AllDocuments.csv")
Stats<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_12Apr2017/Stats.csv")
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_12Apr2017/Fidelity_OutputData.csv")
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")

# Run date
PaperStats[which(PaperStats[,"Text"]=="Fidelity run date"),"X"]
#2017-04-12 12:52:44

# Number of documents the application was run on
length(unique(as.character(AllDocuments[,"x"])))
# 77,679

# How many fossil occurrences are in PBDB in North America at app run date (From PBDB API)
PaperStats[which(PaperStats[,"Text"]=="Number of North American occurrences in PBDB"),"X"]
# 489,776

# How many North American fossil occurrences are NOT matched to Macrostrat
PBDB_Occs<-as.numeric(as.character(PaperStats[which(PaperStats[,"Text"]=="Number of North American occurrences in PBDB"),"X"]))
Macrostrat_Occs<-as.numeric(as.character(PaperStats[which(PaperStats[,"Text"]=="Number of PBDB occurrences in Macrostrat"),"X"]))

PBDB_Occs-Macrostrat_Occs
# 25,121

# Total number of sedimentary, Phanerozoic formations in Macrostrat
as.numeric(as.character(PaperStats[which(PaperStats[,"Text"]=="Total number of sedimentary, Phanerozoic formations in Macrostrat"),"X"]))
# 4,682

# How many Phanerozoic sedimentary rock formations in Macrostrat have PBDB fossil occurrences
as.numeric(as.character(PaperStats[which(PaperStats[,"Text"]=="Number of Phanerozoic sedimentary formations in Macrostrat that have PBDB fossil occurrences"),"X"]))
# 2,021

# Number of Macrostrat, sedimentary, Phanerozoic formations that do NOT have PBDB fossil occurrences (EPSILON-BETA=ZETA, perform in-app check with length(....))
as.numeric(as.character(PaperStats[which(PaperStats[,"Text"]=="Number of candidate units (Phanerozoic sedimentary formations in Macrostrat that do not have PBDB fossil occurrences)"),"X"]))
# 2,661
#Check: 4682 - 2021 = 2661

# How much data is cut down from geolocation check
LocationCheckRow<-which(Stats[,"StepDescription"]=="Validate unit locations")
PostCheckDocs<-Stats[LocationCheckRow,"NumberDocuments"]
PostCheckCandidates<-Stats[LocationCheckRow,"Candidate_Units"]
PreCheckDocs<-Stats[LocationCheckRow-1,"NumberDocuments"]
PreCheckCandidates<-Stats[LocationCheckRow-1,"Candidate_Units"]

PreCheckDocs-PostCheckDocs
# 293 documents removed

PreCheckCandidates-PostCheckCandidates
# 28 candidate units removed

# Number of candidate units, non-candidate units (in PBDB), and documents in original output
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 735 candidate formations
length(unique(InitialOutput[which(InitialOutput[,"PBDB_occ"]==FALSE),"docid"]))
# 1,191 documents in initial output mentioning candidate units
length(unique(as.character(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,120 non-candidate formations (in PBDB)
length(unique(InitialOutput[which(InitialOutput[,"PBDB_occ"]==TRUE),"docid"]))
# 3,869 documents in initial output mentioning non-candidate units

# Number of candidate units, non-candidate units (in PBDB), and documents in cleaned output
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"Formation"])))
# 618 candidate formations
length(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"docid"]))
# 935 documents in cleaned output mentioning candidate units
length(unique(as.character(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"Formation"])))
# 1,032 non-candidate formations (in PBDB)
length(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"docid"]))
# 3,368 documents in cleaned output mentioning non-candidate units
