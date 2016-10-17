library("rbenchmark")
library("RCurl")
library("pbapply")
library("doParallel")
library("data.table")

Cluster<-makeCluster(3)
clusterExport(cl=Cluster,varlist=c("grepFunction","greplFunction","greplFixed","greplPerl","greplPatternsPerl","greplPatternsFixed"))

# Record start time
Start<-print(Sys.time())
# Apply grep to cleaned words
benchmark(
sapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplPerl, CleanedWords=CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplFixed, CleanedWords=CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="the"),
sapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="the"),  
replications=100)
# Record end time
End<-print(Sys.time())
# Find total runtime
End-Start
  
stopCluster(Cluster)

#                                                                                           test replications elapsed relative user.self sys.self user.child sys.child
# 1              sapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1 = "the")          100  39.485    1.000    30.147    0.467          0         0
# 3 sapply(CandidateUnits[1:50], greplFixed, CleanedWords = CleanedWords[1:10000], Word1 = "the")          100  43.052    1.090    35.624    0.310          0         0
# 5        sapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1 = "the")          100  71.095    1.801    69.726    0.666          0         0
# 4         sapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000], Word1 = "the")          100  77.124    1.953    65.952    0.915          0         0
# 2  sapply(CandidateUnits[1:50], greplPerl, CleanedWords = CleanedWords[1:10000], Word1 = "the")          100  42.580    1.078    30.804    0.665          0         0

  
grepFunction<-function(CandidateUnits=CandidateUnits[1:50],CleanedWords=CleanedWords[1:10000],Word1="the"){
  Hits<-grep(CandidateUnits,CleanedWords, ignore.case=FALSE, perl=TRUE)
  SubsetWords<-CleanedWords[Hits]
  Location<-which(Hits==TRUE)
  SubsetMatrix<-cbind(Location,SubsetWords)
  Hits2<-grep(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
  return(SubsetMatrix[which(Hits2==TRUE)]) 
  }

grepFunctionHits<-sapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="the")
                                                      
greplPerl<-function(CandidateUnits=CandidateUnits[1:50], CleanedWords=CleanedWords[1:10000],Word1="the"){
     Hits<-grepl(CandidateUnits,CleanedWords, ignore.case=FALSE, perl=TRUE)
     Location<-which(Hits==TRUE)
     SubsetWords<-CleanedWords[Location]
     SubsetMatrix<-cbind(Location,SubsetWords)
     Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
     return(SubsetMatrix[which(Hits2==TRUE)])
     }

greplPerlHits<-sapply(CandidateUnits[1:50], greplPerl, CleanedWords=CleanedWords[1:10000], Word1="the")                               
                   
greplFixed<-function(CandidateUnits=CandidateUnits[1:50], CleanedWords=CleanedWords[1:10000],Word1="the"){
     Hits<-grepl(CandidateUnits,CleanedWords, ignore.case=FALSE, fixed=TRUE)
     Location<-which(Hits==TRUE)
     SubsetWords<-CleanedWords[Location]
     SubsetMatrix<-cbind(Location,SubsetWords)
     Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, fixed=TRUE)
     return(SubsetMatrix[which(Hits2==TRUE)])
     }

greplFixedHits<-sapply(CandidateUnits[1:50], greplFixed, CleanedWords=CleanedWords[1:10000], Word1="the")                     
    
greplPatternsPerl<-function(x,y,Word1="the"){ 
    Hits<-grepl(x,y,perl=TRUE)&grepl(Word1,y,perl=TRUE)
    return(Hits)
    }

PatternsPerlHits<-sapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="the")
                 
greplPatternsFixed<-function(x,y,Word1="the"){ 
    Hits<-grepl(x,y,fixed=TRUE)&grepl(Word1,y,fixed=TRUE)
    return(Hits)
    }

PatternsFixedHits<-sapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="the")
 
                     
                     
                     
                     
                     
                     
