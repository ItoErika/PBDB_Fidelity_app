# Load libraries
library("RCurl")

# Download taxanomic names (genus and below) from the Paleobiology Database
TaxaURL<-"https://paleobiodb.org/data1.2/taxa/list.csv?rank=max_genus&all_records"
TaxaURL<-getURL(TaxaURL)
Taxa<-read.csv(text=TaxaURL, header=TRUE)

# Split Taxa into species and genera 
Species<-Taxa[which(Taxa[,"taxon_rank"]=="species"),"taxon_name"]
Genera<-Taxa[which(Taxa[,"taxon_rank"]=="genus"),"taxon_name"]

# Load taxonomic name, docid tuples
PBDB_Tuples<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_docid_terms.csv", header=FALSE)
# Assign column names
colnames(PBDB_Tuples)<-c("docid", "taxon_name")

# Subset PBDB_Tuples to only include taxonomic names that are at the genus level
Genera_Tuples<-subset(PBDB_Tuples, PBDB_Tuples[,"taxon_name"]%in%Genera)

# Add sp. to the genus names
Genera_Tuples[,"taxon_name"]<-paste(Genera_Tuples[,"taxon_name"],"sp.",sep=" ")

# Subset PBDB_Tuples to only include taxonomic names that are at the species level
Species_Tuples<-subset(PBDB_Tuples, PBDB_Tuples[,"taxon_name"]%in%Species)
