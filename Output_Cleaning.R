# Load output data from application
# OutputData<-readRDS("")

# Remove hits for microfossils and trace fossils within the output sentences
Micro<-grep("microfossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Trace<-grep("trace fossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Remove words or phrases that are likely to cause reading errors creating false hits
NoFossils<-grep(" no fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lack<-grep(" lack ", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lacks<-grep(" lacks ", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
AbsentFossils<-grep("absence of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Above<-grep("above", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Below<-grep("below", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlain<-grep("overlain", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlie<-grep("overlie", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)

NoisySentences<-unique(c(Micro, Trace, NoFossils, Lack, Lacks, AbsentFossils, VoidFossils, Correlative, Equivalent, Above, 
Below, Overlain, Overlie, Underlain, Underlie, Underlying, Overlying))
                         
CleanedOutput<-OutputData[-NoisySentences,]

# Make a sample
Sample<-unique(CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),c("Formation","Sentence","docid","sentid")])
# Take a random sample of 100  Sample rows to check accuracy
Sample<-Sample[sample(1:nrow(Sample), size=200, replace=FALSE),]
