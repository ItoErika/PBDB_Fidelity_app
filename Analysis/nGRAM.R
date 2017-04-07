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
    MinYear<-min(as.numeric(as.character(ParsedJSON[,"year"])))
    Sys.sleep(1)
    return(cbind(NumDocs, MinYear))
    }
    
# Load CleanedOutput
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")

# Create a vector of candidate formations
Candidates<-as.character(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),"Formation"]))                
# Create a vector of non-candidate formations
NonCandidates<-as.character(unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==TRUE),"Formation"]))
                
# Apply the nGRAM function to all candidate units
CandidatesGram<-pbsapply(Candidates, function(x) nGRAM(Term=x))      
# Apply the nGRAM function to all non-candidate units
NonCandidatesGram<-pbsapply(NonCandidates, function(x) nGRAM(Term=x))  


