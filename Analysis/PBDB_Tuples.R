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
PBDB_Tuples<-read.table("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_doc_terms", sep="\t")
# Assign column names
colnames(PBDB_Tuples)<-c("docid", "taxon_name")
PBDB_Tuples[,"docid"]<-as.character(PBDB_Tuples[,"docid"])
PBDB_Tuples[,"taxon_name"]<-as.character(PBDB_Tuples[,"taxon_name"])

# Subset PBDB_Tuples to only include taxonomic names that are at the genus or species level
PBDB_Tuples<-subset(PBDB_Tuples, PBDB_Tuples[,"taxon_name"]%in%Genera|PBDB_Tuples[,"taxon_name"]%in%Species)
    
# Download final cleaned fidelity output data
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")
    
# Subset CleanedOutput to only include documents from PBDB_Tuples
PBDB_Tuple_Output<-subset(CleanedOutput, CleanedOutput[,"docid"]%in%PBDB_Tuples[,"docid"])  

