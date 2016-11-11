library("RCurl")
library("RJSONIO")

# download all references from PBDB
RefsURL<-"https://paleobiodb.org/data1.2/taxa/refs.csv?select=taxonomy&private&all_records"
GotURL<-getURL(RefsURL)
PBDBRefs<-read.csv(text=GotURL,header=TRUE)

# download all article data form geodeepdive with a pubname that contains the word "palaeogeography"
DDRefs<-fromJSON("https://geodeepdive.org/api/articles?pubname_like=palaeogeography")
DDRefs<-DDRefs[[1]][[2]]

# make a column of DD reference numbers
DDRefNums<-vector(length=length(DDRefs))
for (i in 1:length(DDRefs)){
    DDRefNums[i]<-DDRefs[[i]][[2]]
    }     
# make a vector of DD authors
DDAuthors<-vector(length=length(DDRefs))
for (i in 1:length(DDRefs)){
    DDAuthors[i]<-paste(unlist(DDRefs[[i]][[9]]), collapse=" ")
    }
# make a vector of DD publication years
DDPubYr<-vector(length=length(DDRefs))
for (i in 1:length(DDRefs)){
    DDPubYr[i]<-DDRefs[[i]][[13]]
    } 
# make a vector of DD ref titles 
DDTitles<-vector(length=length(DDRefs))
for (i in 1:length(DDRefs)){
    DDTitles[i]<-DDRefs[[i]][[4]]
    }   
# make a column of DD jornalnames 
DDJournals<-vector(length=length(DDRefs))
for (i in 1:length(DDRefs)){
    DDJournals[i]<-DDRefs[[i]][[6]]
    }   

# create identically formatted matrices for geodeepdive and pbdb references 
DDRefs<-cbind(DDRefNums,DDAuthors,DDPubYr,DDTitles,DDJournals)
PBDBRefs<-cbind(PBDBRefs[c("reference_no","author1last","pubyr","reftitle","pubtitle")])  
  
# convert matrices to dataframes
DDRefs<-as.data.frame(DDRefs)
PBDBRefs<-as.data.frame(PBDBRefs)
  
# make sure all of the data in the data frames are formatted correctly
  
DDRefs[,"DDRefNums"]<-as.character(DDRefs[,"DDRefNums"])
DDRefs[,"DDAuthors"]<-as.character(DDRefs[,"DDAuthors"])
DDRefs[,"DDPubYr"]<-as.numeric(as.character(DDRefs[,"DDPubYr"]))
DDRefs[,"DDTitles"]<-as.character(DDRefs[,"DDTitles"])
DDRefs[,"DDJournals"]<-as.character(DDRefs[,"DDJournals"])

DDRefs[,"author"]<-DDRefs[,"DDAuthors"]

colnames(DDRefs)[1]<-"reference_no"
colnames(DDRefs)[2]<-"author"
colnames(DDRefs)[3]<-"pubyr"
colnames(DDRefs)[4]<-"title"
colnames(DDRefs)[5]<-"pubtitle"

 
PBDBRefs[,"reference_no"]<-as.numeric(as.character(PBDBRefs[,"reference_no"]))
PBDBRefs[,"authorlast1"]<-as.character(PBDBRefs[,"authorlast1"])
PBDBRefs[,"pubyr"]<-as.numeric(as.character(PBDBRefs[,"pubyr"]))
PBDBRefs[,"reftitle"]<-as.character(PBDBRefs[,"reftitle"])
PBDBRefs[,"pubtitle"]<-as.character(PBDBRefs[,"pubtitle"])

colnames(PBDBRefs)[1]<-"reference_no"
colnames(PBDBRefs)[2]<-"author"
colnames(PBDBRefs)[3]<-"pubyr"
colnames(PBDBRefs)[4]<-"title"
colnames(PBDBRefs)[5]<-"pubtitle"

  
  
### Phase 2: A MATCHING FUNCTION IS BORN
matchBibs<-function(Bib1,Bib2) {
    # Title Similarity
    Title<-stringsim(Bib1["title"],Bib2[1,"title"])
    # Pub year match
    Year<-Bib1["pubyr"]==Bib2[1,"pubyr"]
    # Journal Similarity
    Journal<-stringsim(Bib1["pubtitle"],Bib2[1,"pubtitle"])
    # Author present
    Author<-length(grep(Bib1["author"],Bib2[1,"author"],perl=TRUE,ignore.case=TRUE))
    # Return output     
    return(c(Title,Year,Journal,Author))
    }
  
  ##we should add docid and refid to the output of match bibs function!!!!!!!!!!!
  
  
  
  
  
  

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
