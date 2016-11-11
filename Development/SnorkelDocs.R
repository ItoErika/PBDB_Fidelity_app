library("RCurl")
library("RJSONIO")

# download all references from PBDB
RefsURL<-"https://paleobiodb.org/data1.2/taxa/refs.csv?select=taxonomy&private&all_records"
GotURL<-getURL(RefsURL)
ReferencesFrame<-read.csv(text=GotURL,header=TRUE)

# download all article data form geodeepdive with a pubname that contains the word "palaeogeography"
DDPubName<-fromJSON("https://geodeepdive.org/api/articles?pubname_like=palaeogeography.json/accessType=DOWNLOAD")





Doc<-read.delim("~/Downloads/Telegram Desktop/sentences_nlp352_55b3d56de138231",header=FALSE)
colnames(Doc)[4]<-"words"
Doc[,"words"]<-gsub("\\{|\\}","",Doc[,"words"])
CleanedDoc<-gsub(","," ",Doc[,"words"])

# download all species names from PBDB
SpeciesURL<-"https://paleobiodb.org/data1.2/taxa/list.csv?rank=species&private&all_records"
GotURL<-getURL(SpeciesURL)
SpeciesFrame<-read.csv(text=GotURL,header=TRUE)

Species<-unique(SpeciesFrame[,"taxon_name"])

# search for all species names in document
# Apply grep to cleaned words
SpeciesHits<-sapply(Species,function(x,y) grep(x,y,ignore.case=TRUE, perl = TRUE),CleanedDoc)
