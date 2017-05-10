# Determine the frequency of mentions of candidate units (sedimentary, Phanerozoic formations with no PBDB occurrences in Macrostrat) 
# vs. non-candidate units (sedimentary, Phanerozoic formations with at least one PBDB occurrence in Macrostrat)

# Load the data collected from the first pass document search for all candidate and non-candidate formations
# NOTE: This is the output for the first search for all formations of interest (candidate and non-candidate)
# We started with 2,661 candidate units and 2,021 non-candidate units
MatchData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/MatchData.csv", row.names=1)

# Extract columns of interest from MatchData
FrequencyData<-unique(MatchData[,c("docid", "Formation", "PBDB_occ")])

# Determine the number of documents which contained non-candidate formations
length(unique(MatchData[which(MatchData[,"PBDB_occ"]==TRUE),"docid"]))
# 26,269

# Determine the number of non-candidate formations found in our search of the literature
length(unique(MatchData[which(MatchData[,"PBDB_occ"]==TRUE),"Formation"]))
# 1,962 (97.1% of all non-candidates)

# Determine the number of documents which contained candidate formations
length(unique(MatchData[which(MatchData[,"PBDB_occ"]==FALSE),"docid"]))
# 15,519

# Determine the number of candidate formations found in our search of the literature
length(unique(MatchData[which(MatchData[,"PBDB_occ"]==FALSE),"Formation"]))
# 2,249 (84.5% of all candidates)
