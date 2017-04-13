# Original approach of using PBDB tuples from Ian to subset output:

# Load libraries
library("RCurl")
library("RJSONIO")
library("doParallel")
library("RPostgreSQL")
library("velociraptr")

# Make cluster
Cluster<-makeCluster(3)

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

# Download final cleaned fidelity output data
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")

# Subset Genera_Tuples to only include docs from CleanedOutput
Genera_Tuples<-subset(Genera_Tuples, Genera_Tuples[,"docid"]%in%CleanedOutput[,"docid"])

# Replace the spaces in Genera_Tuples taxon names with %20 for the API term search
Genera_Tuples[,"taxon_name"]<-gsub(" ", "%20", Genera_Tuples[,"taxon_name"])

# Use for-loop to create a URL that will use the GeoDeepDive API to search for each tuple's term in its document
URL<-vector(length=nrow(Genera_Tuples))
for(i in 1:nrow(Genera_Tuples)){
    URL[i]<-paste0("https://geodeepdive.org/api/articles?docid=",paste(Genera_Tuples[i,"docid"]),"&term=",paste(Genera_Tuples[i,"taxon_name"]))
    }

# Pass fromJSON function to cluster
clusterExport(cl=Cluster,varlist="fromJSON")

# Determine whether or not the terms are found in the documents
APIresults<-parSapply(Cluster, URL, function(x) fromJSON(x)$error$message)
  
# Replace the %20 in Genera_Tuples with a space
Genera_Tuples[,"taxon_name"]<-gsub("%20", " ", Genera_Tuples[,"taxon_name"])
  
# Add a column to Genera_Tuples telling whether or not the term was matched in the document
 Genera_Tuples[,"match"]<-APIResults=="NULL"
  
# Assign row names
rownames(Genera_Tuples)<-1:nrow(Genera_Tuples)
