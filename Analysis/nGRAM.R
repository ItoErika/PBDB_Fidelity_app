library("RJSONIO")
library("pbapply")

parseGDD<-function(JSON) {
    gddid<-sapply(JSON$success$data, function(x) x$`_gddid`)
    type<-sapply(JSON$success$data, function(x) x$type)
    author<-sapply(JSON$success$data, function(x) paste(x$author, collapse=";"))
    year<-sapply(JSON$success$data, function(x) x$year)
    title<-sapply(JSON$success$data, function(x) x$title)
    journal<-sapply(JSON$success$data, function(x) x$journal)
    volume<-sapply(JSON$success$data, function(x) x$volume)
    number<-sapply(JSON$success$data, function(x) x$number)
    pages<-sapply(JSON$success$data, function(x) x$pages)
    publisher<-sapply(JSON$success$data, function(x) x$publisher)
    link<-sapply(JSON$success$data,function(x) unlist(x$link)[["url"]])
    doi<-sapply(JSON$success$data, function(x) unlist(x$identifier)[["id"]])
    Output<-cbind(gddid, type, author, year, title, journal, volume, number, pages, publisher, link, doi)
    return(Output)
    }

nGRAM<-function(Term, Publisher="", Journal=""){
    Journal<-gsub(" ", "%20", Journal)
    Term<-gsub(" ", "%20", Term)
    Publisher<-gsub(" ","%20",Publisher)
    URL<-paste0("https://geodeepdive.org/api/articles?pubname=",Journal,"&term=",Term,"&publisher=",Publisher)
    JSON<-RJSONIO::fromJSON(URL)
    ParsedJSON<-parseGDD(JSON)
    NumDocs<-length(unique(ParsedJSON[,"gddid"]))
    # Remove rows where the year is NULL for the min() function
    JSONYears<-ParsedJSON[-which(ParsedJSON[,"year"]=="NULL"),"year"]
    MinYear<-min(as.numeric(as.character(JSONYears)))
    Sys.sleep(1)
    return(cbind(NumDocs, MinYear))
    }
    
# Load CleanedOutput
NoNoiseOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/NoNoiseOutput.csv")

# Create a vector of candidate formations
Candidates<-as.character(unique(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==FALSE),"Formation"]))                
# Create a vector of non-candidate formations
PBDBUnits<-as.character(unique(NoNoiseOutput[which(NoNoiseOutput[,"PBDB_occ"]==TRUE),"Formation"]))
                
# Apply the nGRAM function to all candidate units
CandidatesGram<-pbsapply(Candidates, function(x) nGRAM(Term=x))      
# Apply the nGRAM function to all PBDBUnits
PBDBGram<-pbsapply(PBDBUnits, function(x) nGRAM(Term=x))  


