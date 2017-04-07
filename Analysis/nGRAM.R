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
    NumDocs<-length(unique(ParsedJSON[,"gddid"]
    MinYear<-min(as.numeric(as.character(ParsedJSON[,"year"])))
    return(cbind(NumDocs, MinYear)
    }
    
  
